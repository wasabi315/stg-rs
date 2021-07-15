use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

extern crate once_cell;
use once_cell::sync::Lazy;

use super::ast::*;

#[derive(Debug)]
pub struct State<'ast> {
    code: Code<'ast>,
    args: ArgStack<'ast>,
    returns: ReturnStack<'ast>,
    updates: UpdateStack<'ast>,
    globals: Env<'ast>,
}

#[derive(Debug)]
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

#[derive(Debug)]
struct ReturnStackFrame<'ast> {
    alts: &'ast Alts,
    locals: Env<'ast>,
}

type ReturnStack<'ast> = Vec<ReturnStackFrame<'ast>>;

#[derive(Debug)]
struct UpdateStackFrame<'ast> {
    args: ArgStack<'ast>,
    returns: ReturnStack<'ast>,
    addr: Addr<'ast>,
}

type UpdateStack<'ast> = Vec<UpdateStackFrame<'ast>>;

type Env<'ast> = HashMap<Cow<'ast, str>, Value<'ast>>;

type Addr<'ast> = Rc<RefCell<Option<Closure<'ast>>>>;

#[derive(Clone, Debug)]
enum Value<'ast> {
    Addr(Addr<'ast>),
    Int(i64),
}

#[derive(Clone, Debug)]
struct Closure<'ast> {
    lf: &'ast LambdaForm,
    free: Vec<Value<'ast>>,
}

type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub struct Machine<'a, W: ?Sized> {
    pub out: &'a mut W,
}

impl<'a, W> Machine<'a, W>
where
    W: ?Sized + std::io::Write,
{
    pub fn run(&'a mut self, program: &'a Program) -> MachineState<'a, W> {
        MachineState(MachineStateInner::Init(program), self.out)
    }
}

pub struct MachineState<'a, W: ?Sized>(MachineStateInner<'a>, &'a mut W);

enum MachineStateInner<'a> {
    Init(&'a Program),
    Exec { state: State<'a>, fresh: usize },
}

impl<'a, W> MachineState<'a, W>
where
    W: ?Sized + std::io::Write,
{
    pub fn run(&'a mut self) -> Result<()> {
        let out = &mut self.1;
        loop {
            match &mut self.0 {
                MachineStateInner::Init(program) => {
                    let addrs = program
                        .0
                        .values()
                        .map(|lf| Rc::new(RefCell::new(Some(Closure { lf, free: vec![] }))));
                    let globals = program
                        .0
                        .keys()
                        .map(Into::into)
                        .zip(addrs.clone().map(Value::Addr))
                        .collect();
                    for addr in addrs {
                        let mut addr = addr.borrow_mut();
                        let closure = addr.as_mut().unwrap();
                        closure.free = ToValue::vals(&closure.lf.free, &HashMap::new(), &globals)?;
                    }

                    static MAIN: Lazy<Expr> = Lazy::new(|| expr! { main {} });

                    self.0 = MachineStateInner::Exec {
                        state: State {
                            code: Code::Eval {
                                expr: &*MAIN,
                                locals: HashMap::new(),
                            },
                            args: vec![],
                            returns: vec![],
                            updates: vec![],
                            globals,
                        },
                        fresh: 0,
                    };
                }

                MachineStateInner::Exec { state, .. } => {
                    match &mut state.code {
                        Code::Eval {
                            expr: Expr::VarApp { var, args },
                            locals,
                        } => {
                            let args = ToValue::vals(args, locals, &state.globals)?;
                            match var.val(locals, &state.globals)? {
                                Value::Addr(addr) => {
                                    state.code = Code::Enter(Rc::clone(&addr));
                                    let args = std::mem::replace(&mut state.args, args);
                                    state.args.extend(args);
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
                            let addrs = binds.0.values().map(|lf| {
                                Rc::new(RefCell::new(Some(Closure { lf, free: vec![] })))
                            });

                            // 2. create bindings
                            let mut next_locals = locals.clone();
                            next_locals.extend(
                                binds
                                    .0
                                    .keys()
                                    .map(Into::into)
                                    .zip(addrs.clone().map(Value::Addr)),
                            );

                            // 3. enable closures to access free variables
                            let locals_rhs = if *rec { &next_locals } else { locals };
                            for addr in addrs {
                                let mut addr = addr.borrow_mut();
                                let closure = addr.as_mut().unwrap();
                                closure.free =
                                    ToValue::vals(&closure.lf.free, locals_rhs, &HashMap::new())?;
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
                                &ToValue::vals(args, locals, &HashMap::new())?[..],
                            ) {
                                ("add#", [Value::Int(x), Value::Int(y)]) => x + y,
                                ("sub#", [Value::Int(x), Value::Int(y)]) => x - y,
                                ("mul#", [Value::Int(x), Value::Int(y)]) => x * y,
                                ("div#", [Value::Int(x), Value::Int(y)]) => {
                                    if *y == 0 {
                                        return Err(DivisionByZero.into());
                                    } else {
                                        x / y
                                    }
                                }
                                ("traceInt#", [Value::Int(x)]) => {
                                    writeln!(out, "{}", x).unwrap();
                                    *x
                                }
                                ("undefined#", []) => return Err(Undefined.into()),
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
                                        let locals = lf
                                            .free
                                            .iter()
                                            .map(Into::into)
                                            .zip(free.iter().cloned())
                                            .collect();
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
                                    } else if state.args.len() < lf.args.len() {
                                        unimplemented!("Partial Application");
                                    } else {
                                        let args = state.args.drain(..lf.args.len());

                                        let arg_pairs = lf.args.iter().map(Into::into).zip(args);
                                        let free_pairs = lf
                                            .free
                                            .iter()
                                            .map(Into::into)
                                            .zip(free.iter().cloned());
                                        let locals = arg_pairs.chain(free_pairs).collect();
                                        let expr = &lf.expr;
                                        drop(_addr);

                                        state.code = Code::Eval { expr, locals };
                                    }
                                }

                                None => return Err(EnterBlackhole.into()),
                            }
                        }

                        Code::ReturnCon { constr, args } => {
                            if let Some(ReturnStackFrame { alts, mut locals }) = state.returns.pop()
                            {
                                let expr = match lookup_algalt(alts, constr)? {
                                    Match::Match(AlgAlt { vars, expr, .. }) => {
                                        locals.extend(
                                            vars.iter().map(Into::into).zip(args.iter().cloned()),
                                        );
                                        expr
                                    }
                                    Match::Default(DefAlt::DefAlt { expr }) => expr,
                                    Match::Default(DefAlt::VarAlt { .. }) => {
                                        unimplemented!("Default alternative with bound");
                                    }
                                };
                                state.code = Code::Eval { expr, locals };
                            } else if let Some(UpdateStackFrame { .. }) = state.updates.pop() {
                                unimplemented!();
                            }
                        }

                        Code::ReturnInt(n) => {
                            let ReturnStackFrame { alts, mut locals } = match state.returns.pop() {
                                Some(frame) => frame,
                                None => return Err(ReturnWithEmptyReturnStack.into()),
                            };

                            let expr = match lookup_primalt(alts, *n)? {
                                Match::Match(PrimAlt { expr, .. }) => expr,
                                Match::Default(DefAlt::DefAlt { expr }) => expr,
                                Match::Default(DefAlt::VarAlt { var, expr }) => {
                                    locals.insert(var.into(), Value::Int(*n));
                                    expr
                                }
                            };

                            state.code = Code::Eval { expr, locals };
                        }
                    };
                }
            }
        }
    }
}

trait ToValue {
    fn val<'ast>(&self, locals: &Env<'ast>, globals: &Env<'ast>) -> Result<Value<'ast>>;

    fn vals<'ast, 'a, T>(
        iter: T,
        locals: &Env<'ast>,
        globals: &Env<'ast>,
    ) -> Result<Vec<Value<'ast>>>
    where
        T: IntoIterator<Item = &'a Self>,
        Self: 'a,
    {
        iter.into_iter().map(|x| x.val(locals, globals)).collect()
    }
}

impl ToValue for String {
    fn val<'ast>(&self, locals: &Env<'ast>, globals: &Env<'ast>) -> Result<Value<'ast>> {
        let key = self.as_str();
        locals
            .get(key)
            .or_else(|| globals.get(key))
            .cloned()
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

enum Match<'a, T> {
    Match(&'a T),
    Default(&'a DefAlt),
}

fn lookup_algalt<'a>(alts: &'a Alts, constr: &'a str) -> Result<Match<'a, AlgAlt>> {
    match &alts.0 {
        NonDefAlts::AlgAlts(aalts) => Ok(aalts
            .iter()
            .find(|aalt| aalt.constr == constr)
            .map_or(Match::Default(&alts.1), Match::Match)),
        NonDefAlts::Empty => Ok(Match::Default(&alts.1)),
        NonDefAlts::PrimAlts(_) => Err(InvalidNonDefAlt.into()),
    }
}

fn lookup_primalt(alts: &Alts, lit: Literal) -> Result<Match<PrimAlt>> {
    match &alts.0 {
        NonDefAlts::PrimAlts(palts) => Ok(palts
            .iter()
            .find(|palt| palt.lit == lit)
            .map_or(Match::Default(&alts.1), Match::Match)),
        NonDefAlts::Empty => Ok(Match::Default(&alts.1)),
        NonDefAlts::AlgAlts(_) => Err(InvalidNonDefAlt.into()),
    }
}

// Errors

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

#[derive(Debug)]
struct InvalidNonDefAlt;

impl fmt::Display for InvalidNonDefAlt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Try to match constructor with integer or integer with constructor"
        )
    }
}

impl Error for InvalidNonDefAlt {}

#[derive(Debug)]
struct UnboundVariable(String);

impl fmt::Display for UnboundVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unbound vairable: {}", self.0)
    }
}

impl Error for UnboundVariable {}

#[derive(Debug)]
struct ReturnWithEmptyReturnStack;

impl fmt::Display for ReturnWithEmptyReturnStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Try to return integer when return stack is empty",)
    }
}

impl Error for ReturnWithEmptyReturnStack {}

#[derive(Debug)]
struct Undefined;

impl fmt::Display for Undefined {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Try to return integer when return stack is empty",)
    }
}

impl Error for Undefined {}
