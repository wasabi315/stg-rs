use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use super::ast::*;

struct State<'ast> {
    code: Code<'ast>,
    args: ArgStack<'ast>,
    returns: ReturnStack<'ast>,
    updates: UpdateStack<'ast>,
    globals: Env<'ast>,
}

enum Code<'ast> {
    Eval {
        expr: &'ast Expr,
        locals: Env<'ast>,
    },
    Enter(Addr<'ast>),
    ReturnCon {
        constr: &'ast Constr,
        args: Vec<Value<'ast>>,
    },
    ReturnInt(i64),
}

type ArgStack<'ast> = Vec<Value<'ast>>;

struct ReturnStackFrame<'ast> {
    alts: &'ast Alts,
    locals: Env<'ast>,
}

type ReturnStack<'ast> = Vec<ReturnStackFrame<'ast>>;

struct UpdateStackFrame<'ast> {
    args: ArgStack<'ast>,
    returns: ReturnStack<'ast>,
    addr: Addr<'ast>,
}

type UpdateStack<'ast> = Vec<UpdateStackFrame<'ast>>;

type Env<'ast> = HashMap<Var, Value<'ast>>;

type Addr<'ast> = Rc<RefCell<Option<Closure<'ast>>>>;

#[derive(Clone)]
enum Value<'ast> {
    Addr(Addr<'ast>),
    Int(i64),
}

struct Closure<'ast> {
    lf: &'ast LambdaForm,
    free: Vec<Value<'ast>>,
}

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug)]
struct UnboundVariable(String);

impl fmt::Display for UnboundVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unbound vairable: {}", self.0)
    }
}

impl Error for UnboundVariable {}

trait ToValue: Sized {
    fn val<'ast>(&self, locals: &Env<'ast>, globals: &Env<'ast>) -> Result<Value<'ast>>;

    fn vals<'ast>(
        xs: &[Self],
        locals: &Env<'ast>,
        globals: &Env<'ast>,
    ) -> Result<Vec<Value<'ast>>> {
        xs.iter().map(|x| x.val(locals, globals)).collect()
    }
}

impl ToValue for String {
    fn val<'ast>(&self, locals: &Env<'ast>, globals: &Env<'ast>) -> Result<Value<'ast>> {
        locals
            .get(self)
            .or_else(|| globals.get(self))
            .map(Clone::clone)
            .ok_or_else(|| UnboundVariable(self.clone()).into())
    }
}

impl ToValue for i64 {
    fn val<'ast>(&self, _: &Env<'ast>, _: &Env<'ast>) -> Result<Value<'ast>> {
        Ok(Value::Int(*self))
    }
}

impl ToValue for Atom {
    fn val<'ast>(&self, locals: &Env<'ast>, globals: &Env<'ast>) -> Result<Value<'ast>> {
        match self {
            Atom::Lit(n) => n.val(locals, globals),
            Atom::Var(v) => v.val(locals, globals),
        }
    }
}

fn run(state: &mut State) -> Result<()> {
    loop {
        match &mut state.code {
            Code::Eval {
                expr: Expr::VarApp { var, args },
                locals,
            } => {
                let args = ToValue::vals(args, locals, &state.globals)?;
                match var.val(locals, &state.globals)? {
                    Value::Addr(addr) => {
                        state.code = Code::Enter(Rc::clone(&addr));
                        prepend(&mut state.args, args);
                    }
                    Value::Int(n) => {
                        if !args.is_empty() {
                            return Err(ApplyToInt.into());
                        }
                        state.code = Code::ReturnInt(n);
                    }
                }
            }

            Code::Eval {
                expr: Expr::Let { rec, binds, expr },
                locals,
            } => {
                // 1. allocate closures
                let addrs = binds
                    .0
                    .values()
                    .map(|lf| Rc::new(RefCell::new(Some(Closure { lf, free: vec![] }))));

                // 2. create bindings
                let mut next_locals = locals.clone();
                for (var, addr) in binds.0.keys().zip(addrs.clone()) {
                    next_locals.insert(var.clone(), Value::Addr(addr));
                }

                // 3. enable closures to access free variables
                let locals_rhs = if *rec { &next_locals } else { locals };
                for addr in addrs {
                    let mut addr = addr.borrow_mut();
                    let closure = addr.as_mut().unwrap();
                    closure.free =
                        ToValue::vals(&closure.lf.free, locals_rhs, &Default::default())?;
                }

                state.code = Code::Eval {
                    expr,
                    locals: next_locals,
                };
            }

            Code::Eval {
                expr: Expr::Case { expr, alts },
                locals,
            } => {
                let locals = std::mem::take(locals);
                state.code = Code::Eval {
                    expr,
                    locals: locals.clone(),
                };
                state.returns.push(ReturnStackFrame { alts, locals });
            }

            Code::Eval {
                expr: Expr::ConstrApp { constr, args },
                locals,
            } => {
                let args = ToValue::vals(args, locals, &state.globals)?;
                state.code = Code::ReturnCon { constr, args };
            }

            Code::Eval {
                expr: Expr::Lit(n), ..
            } => {
                state.code = Code::ReturnInt(*n);
            }

            Code::Eval {
                expr: Expr::PrimApp { prim, args },
                locals,
            } => {
                let n = match (
                    &prim[..],
                    ToValue::vals(args, locals, &Default::default())?.as_slice(),
                ) {
                    ("+#", [Value::Int(x), Value::Int(y)]) => x + y,
                    ("-#", [Value::Int(x), Value::Int(y)]) => x - y,
                    ("*#", [Value::Int(x), Value::Int(y)]) => x * y,
                    ("/#", [Value::Int(x), Value::Int(y)]) => {
                        if *y == 0 {
                            return Err(DivisionByZero.into());
                        } else {
                            x / y
                        }
                    }
                    (op, _) => return Err(UnknownPrimOp(op.to_owned()).into()),
                };
                state.code = Code::ReturnInt(n);
            }

            Code::Enter(addr) => {
                let _addr = addr.borrow();
                match &*_addr {
                    Some(Closure { lf, free }) => {
                        if lf.updatable {
                            if !lf.args.is_empty() {
                                return Err(UpdatableClosureWithArgs.into());
                            }

                            let args = std::mem::take(&mut state.args);
                            let returns = std::mem::take(&mut state.returns);
                            let locals =
                                lf.free.iter().cloned().zip(free.iter().cloned()).collect();
                            let expr = &lf.expr;
                            drop(_addr);

                            // Set blackhole
                            *addr.borrow_mut() = None;

                            state.updates.push(UpdateStackFrame {
                                args,
                                returns,
                                addr: Rc::clone(&addr),
                            });
                            state.code = Code::Eval { expr, locals };
                        } else {
                            let args = state.args.drain(..lf.args.len());

                            let arg_pairs = lf.args.iter().cloned().zip(args);
                            let free_pairs = lf.free.iter().cloned().zip(free.iter().cloned());
                            let locals = arg_pairs.chain(free_pairs).collect();
                            let expr = &lf.expr;
                            drop(_addr);

                            state.code = Code::Eval { expr, locals };
                        }
                    }

                    None => return Err(EnterBlackhole.into()),
                }
            }

            _ => return Ok(()),
        };
    }
}

fn prepend<T>(dest: &mut Vec<T>, src: Vec<T>) {
    let tmp = std::mem::replace(dest, src);
    dest.extend(tmp);
}

#[derive(Debug)]
struct UnknownPrimOp(String);

impl fmt::Display for UnknownPrimOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unknown primitive operation: {}", self.0)
    }
}

impl Error for UnknownPrimOp {}

#[derive(Debug)]
struct DivisionByZero;

impl fmt::Display for DivisionByZero {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Division by zero")
    }
}

impl Error for DivisionByZero {}

#[derive(Debug)]
struct EnterBlackhole;

impl fmt::Display for EnterBlackhole {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Enter blackhole")
    }
}

impl Error for EnterBlackhole {}

#[derive(Debug)]
struct ApplyToInt;

impl fmt::Display for ApplyToInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Arguments are applied to integer")
    }
}

impl Error for ApplyToInt {}

#[derive(Debug)]
struct UpdatableClosureWithArgs;

impl fmt::Display for UpdatableClosureWithArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Updatable closure with arguments")
    }
}

impl Error for UpdatableClosureWithArgs {}
