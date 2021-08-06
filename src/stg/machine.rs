use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::iter;
use std::rc::Rc;

extern crate once_cell;
use once_cell::sync::Lazy;

use super::ast::*;

use crate::extension::vec::VecExt;

pub fn run(program: &Program) -> Result<()> {
    static MAIN: Lazy<Expr> = Lazy::new(|| expr! { main {} });
    let mut code = Code::Eval {
        expr: &*MAIN,
        locals: HashMap::new(),
    };

    let mut stacks = Stacks {
        args: vec![],
        returns: vec![],
        updates: vec![],
    };

    let globals = env_from_binds(true, program, &HashMap::new(), &HashMap::new())?;
    let stdcons = stdcons(program);
    let papps = parapp_helpers();

    while let Some(next) = code.exec(&mut stacks, &globals, &stdcons, &papps)? {
        code = next;
    }

    Ok(())
}

fn stdcons<'a>(program: &'a Program) -> HashMap<&'a String, LambdaForm> {
    type Arity = usize;
    fn make_stdcon(con: &str, arity: Arity) -> LambdaForm {
        let free = (0..arity).map(|i| format!("$stdcon_{}_args{}", con, i));
        LambdaForm {
            free: free.clone().collect(),
            updatable: false,
            args: vec![],
            expr: Expr::ConApp {
                con: con.to_owned(),
                args: free.map(Atom::Var).collect(),
            },
        }
    }

    fn collect_stdcon<'a>(expr: &'a Expr, con_binds: &mut HashMap<&'a String, LambdaForm>) {
        match expr {
            Expr::Let { binds, expr, .. } => {
                for LambdaForm { expr, .. } in binds.values() {
                    collect_stdcon(expr, con_binds);
                }
                collect_stdcon(expr, con_binds);
            }
            Expr::Case { expr, alts } => {
                collect_stdcon(expr, con_binds);
                for alt in alts {
                    match alt {
                        Alt::AlgAlt { con, vars, expr } => {
                            con_binds.insert(con, make_stdcon(con, vars.len()));
                            collect_stdcon(expr, con_binds);
                        }
                        Alt::PrimAlt { expr, .. } => {
                            collect_stdcon(expr, con_binds);
                        }
                        Alt::VarAlt { expr, .. } => {
                            collect_stdcon(expr, con_binds);
                        }
                        Alt::DefAlt { expr } => {
                            collect_stdcon(expr, con_binds);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let mut binds = HashMap::new();
    for LambdaForm { expr, .. } in program.values() {
        collect_stdcon(expr, &mut binds);
    }
    binds
}

fn parapp_helpers() -> HashMap<usize, LambdaForm> {
    fn make_helper(arity: usize) -> LambdaForm {
        let var = format!("$papp_{}_var", arity);
        let args = (0..arity).map(|i| format!("$papp_{}_args{}", arity, i));
        LambdaForm {
            free: iter::once(var.clone()).chain(args.clone()).collect(),
            updatable: false,
            args: vec![],
            expr: Expr::VarApp {
                var,
                args: args.into_iter().map(Atom::Var).collect(),
            },
        }
    }

    (0..10).map(|arity| (arity, make_helper(arity))).collect()
}

type Result<T> = std::result::Result<T, Box<dyn Error>>;

enum Code<'a> {
    Eval { expr: &'a Expr, locals: Env<'a> },
    Enter(Addr<'a>),
    ReturnCon { con: &'a Con, args: Vec<Value<'a>> },
    ReturnInt(i64),
}

struct Stacks<'a> {
    args: Vec<Value<'a>>,
    returns: Vec<RetFrame<'a>>,
    updates: Vec<UpdFrame<'a>>,
}

struct RetFrame<'a> {
    env: Env<'a>,
    alts: &'a [Alt],
}

struct UpdFrame<'a> {
    args: Vec<Value<'a>>,
    returns: Vec<RetFrame<'a>>,
    addr: Addr<'a>,
}

type Env<'a> = HashMap<&'a String, Value<'a>>;

#[derive(Debug, Clone)]
enum Value<'a> {
    Addr(Addr<'a>),
    Int(i64),
}

type Addr<'a> = Rc<RefCell<Option<Closure<'a>>>>;

#[derive(Debug)]
struct Closure<'a> {
    lf: &'a LambdaForm,
    refs: Vec<Value<'a>>,
}

type AlgCont<'a> = Box<
    dyn Fn(Env<'a>, Vec<Value<'a>>, &'a HashMap<&'a String, LambdaForm>) -> Result<Option<Code<'a>>>
        + 'a,
>;

type PrimCont<'a> = Box<dyn Fn(Env<'a>) -> Result<Option<Code<'a>>> + 'a>;

impl<'a> Code<'a> {
    fn exec(
        self,
        stacks: &mut Stacks<'a>,
        globals: &Env<'a>,
        stdcons: &'a HashMap<&'a String, LambdaForm>,
        papps: &'a HashMap<usize, LambdaForm>,
    ) -> Result<Option<Self>> {
        match self {
            Code::Eval { expr, locals } => expr.eval(locals, stacks, globals),

            Code::Enter(addr) => {
                let mut closure_ref = addr.borrow_mut();
                let Closure { lf, refs } = match closure_ref.as_mut() {
                    Some(closure) => closure,
                    None => return Err(EnterBlackhole.into()),
                };
                let LambdaForm {
                    free,
                    updatable,
                    args,
                    expr,
                } = lf;

                if stacks.args.len() < args.len() {
                    let frame = match stacks.updates.pop() {
                        Some(frame) => frame,
                        None => return Ok(None),
                    };
                    *frame.addr.borrow_mut() = Some(Closure {
                        lf: papps.get(&stacks.args.len()).unwrap(),
                        refs: iter::once(Value::Addr(Rc::clone(&addr)))
                            .chain(stacks.args.clone())
                            .collect(),
                    });
                    stacks.args.extend(frame.args);
                    stacks.returns = frame.returns;
                    return Ok(Some(Code::Enter(Rc::clone(&addr))));
                }

                let arg_env = args.iter().zip(stacks.args.drain(..args.len()));
                let free_env = free.iter().zip(refs.iter().cloned());
                let env = arg_env.chain(free_env).collect();

                if *updatable {
                    stacks.updates.push(UpdFrame {
                        args: std::mem::take(&mut stacks.args),
                        returns: std::mem::take(&mut stacks.returns),
                        addr: Rc::clone(&addr),
                    });
                    *closure_ref = None;
                }

                Ok(Some(Code::Eval { expr, locals: env }))
            }

            Code::ReturnCon { con, args } => {
                if let Some(frame) = stacks.returns.pop() {
                    let cont = match frame.alts.iter().find_map(|alt| alt.match_con(con)) {
                        Some(cont) => cont,
                        None => return Err(NoAlternativeMatched.into()),
                    };
                    return cont(frame.env, args, stdcons);
                }

                if let Some(frame) = stacks.updates.pop() {
                    stacks.args = frame.args;
                    stacks.returns = frame.returns;
                    *frame.addr.borrow_mut() = Some(Closure {
                        lf: stdcons.get(con).unwrap(),
                        refs: args.clone(),
                    });
                    return Ok(Some(Code::ReturnCon { con, args }));
                }

                Ok(None)
            }

            Code::ReturnInt(n) => {
                let frame = match stacks.returns.pop() {
                    Some(frame) => frame,
                    None => return Err(ReturnWithEmptyReturnStack.into()),
                };
                let cont = match frame.alts.iter().find_map(|alt| alt.match_lit(n)) {
                    Some(cont) => cont,
                    None => return Err(NoAlternativeMatched.into()),
                };
                cont(frame.env)
            }
        }
    }
}

fn env_from_binds<'a>(
    rec: bool,
    binds: &'a Binds,
    locals: &Env<'a>,
    globals: &Env<'a>,
) -> Result<Env<'a>> {
    let addrs: Vec<Addr<'a>> = (0..binds.len())
        .map(|_| Rc::new(RefCell::new(None)))
        .collect();

    let mut env = locals.clone();
    env.extend(binds.keys().zip(addrs.clone().into_iter().map(Value::Addr)));

    let locals_rhs = if rec { &env } else { &locals };

    for (addr, lf) in addrs.into_iter().zip(binds.values()) {
        *addr.borrow_mut() = Some(Closure {
            lf,
            refs: ToValue::vals(&lf.free, locals_rhs, globals)?,
        });
    }

    Ok(env)
}

impl Expr {
    fn eval<'a>(
        &'a self,
        locals: Env<'a>,
        stacks: &mut Stacks<'a>,
        globals: &Env<'a>,
    ) -> Result<Option<Code<'a>>> {
        match self {
            Expr::Let { rec, binds, expr } => {
                let next_locals = env_from_binds(*rec, binds, &locals, globals)?;

                Ok(Some(Code::Eval {
                    expr,
                    locals: next_locals,
                }))
            }

            Expr::Case { expr, alts } => {
                stacks.returns.push(RetFrame {
                    env: locals.clone(),
                    alts,
                });
                Ok(Some(Code::Eval { expr, locals }))
            }

            Expr::VarApp { var, args } => {
                let var = var.val(&locals, &globals)?;
                let args = ToValue::vals(args, &locals, &globals)?;

                match var {
                    Value::Addr(addr) => {
                        stacks.args.prepend(args);
                        Ok(Some(Code::Enter(addr)))
                    }
                    Value::Int(n) => {
                        if !args.is_empty() {
                            return Err(ApplyToInt.into());
                        }
                        Ok(Some(Code::ReturnInt(n)))
                    }
                }
            }

            Expr::ConApp { con, args } => {
                let args = ToValue::vals(args, &locals, &globals)?;
                Ok(Some(Code::ReturnCon { con, args }))
            }

            Expr::PrimApp { prim, args } => {
                let args = ToValue::vals(args, &locals, &globals)?;
                let n = match (&prim[..], &args[..]) {
                    ("add#", [Value::Int(x), Value::Int(y)]) => x + y,
                    ("sub#", [Value::Int(x), Value::Int(y)]) => x - y,
                    ("mul#", [Value::Int(x), Value::Int(y)]) => x * y,
                    ("div#", [Value::Int(x), Value::Int(y)]) => {
                        if y == &0 {
                            return Err(DivisionByZero.into());
                        } else {
                            x / y
                        }
                    }
                    ("neg#", [Value::Int(x)]) => -x,
                    ("traceInt#", [Value::Int(x)]) => {
                        println!("{}", x);
                        *x
                    }
                    _ => return Err(UnknownPrimOp(prim.to_owned()).into()),
                };
                Ok(Some(Code::ReturnInt(n)))
            }

            Expr::Lit(n) => Ok(Some(Code::ReturnInt(*n))),
        }
    }
}

impl Alt {
    fn match_con<'a>(&'a self, con1: &'a String) -> Option<AlgCont> {
        match self {
            Alt::AlgAlt {
                con: con2,
                vars,
                expr,
            } => {
                if con1 != con2 {
                    return None;
                }
                Some(Box::new(move |mut env, args, _| {
                    for (var, arg) in vars.iter().zip(args.into_iter()) {
                        env.insert(var, arg);
                    }
                    Ok(Some(Code::Eval { expr, locals: env }))
                }))
            }
            Alt::PrimAlt { .. } => None,
            Alt::VarAlt { var, expr } => Some(Box::new(move |mut env, args, stdcons| {
                let closure = Closure {
                    lf: stdcons.get(con1).unwrap(),
                    refs: args,
                };
                env.insert(var, Value::Addr(Rc::new(RefCell::new(Some(closure)))));
                Ok(Some(Code::Eval { expr, locals: env }))
            })),
            Alt::DefAlt { expr } => Some(Box::new(move |env, _, _| {
                Ok(Some(Code::Eval { expr, locals: env }))
            })),
        }
    }

    fn match_lit<'a>(&'a self, lit1: i64) -> Option<PrimCont> {
        match self {
            Alt::AlgAlt { .. } => None,
            Alt::PrimAlt { lit: lit2, expr } => {
                if &lit1 != lit2 {
                    return None;
                }
                Some(Box::new(move |env| {
                    Ok(Some(Code::Eval { expr, locals: env }))
                }))
            }
            Alt::VarAlt { var, expr } => Some(Box::new(move |mut env| {
                env.insert(var, Value::Int(lit1));
                Ok(Some(Code::Eval { expr, locals: env }))
            })),
            Alt::DefAlt { expr } => Some(Box::new(move |env| {
                Ok(Some(Code::Eval { expr, locals: env }))
            })),
        }
    }
}

trait ToValue {
    fn val<'a>(&self, locals: &Env<'a>, globals: &Env<'a>) -> Result<Value<'a>>;

    fn vals<'a, 'b, T>(iter: T, locals: &Env<'a>, globals: &Env<'a>) -> Result<Vec<Value<'a>>>
    where
        T: IntoIterator<Item = &'b Self>,
        Self: 'b,
    {
        iter.into_iter().map(|x| x.val(locals, globals)).collect()
    }
}

impl ToValue for String {
    fn val<'a>(&self, locals: &Env<'a>, globals: &Env<'a>) -> Result<Value<'a>> {
        locals
            .get(self)
            .or_else(|| globals.get(self))
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
struct NoAlternativeMatched;

impl fmt::Display for NoAlternativeMatched {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "No alternative matched",)
    }
}

impl Error for NoAlternativeMatched {}
