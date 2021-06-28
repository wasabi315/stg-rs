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
            .ok_or_else(|| -> Box<dyn Error> { Box::new(UnboundVariable(self.clone())) })
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

const RULES: &'static [fn(&mut State) -> Result<bool>] = &[
    eval_var_app,
    enter_non_updatable,
    eval_let,
    eval_case,
    eval_constr_app,
    eval_literal,
    eval_literal_app,
    eval_prim_app,
];

fn transition(state: &mut State) -> Result<()> {
    for transition in RULES.iter() {
        if transition(state)? {
            break;
        }
    }

    Ok(())
}

fn prepend<T>(dest: &mut Vec<T>, src: Vec<T>) {
    let tmp = std::mem::replace(dest, src);
    dest.extend(tmp);
}

fn eval_var_app(state: &mut State) -> Result<bool> {
    if let Code::Eval {
        expr: Expr::VarApp {
            var,
            args: expr_args,
        },
        locals,
    } = &state.code
    {
        if let (Value::Addr(addr), expr_args) = (
            var.val(locals, &state.globals)?,
            ToValue::vals(expr_args, locals, &state.globals)?,
        ) {
            state.code = Code::Enter(Rc::clone(&addr));
            prepend(&mut state.args, expr_args);
            Ok(true)
        } else {
            Ok(false)
        }
    } else {
        Ok(false)
    }
}

fn enter_non_updatable(state: &mut State) -> Result<bool> {
    if let Code::Enter(addr) = &state.code {
        let addr = addr.borrow();
        match &*addr {
            Some(Closure {
                lf:
                    LambdaForm {
                        free: lf_free,
                        updatable: true,
                        args: lf_args,
                        expr,
                    },
                free,
            }) if state.args.len() >= lf_args.len() => {
                let args = state.args.drain(..lf_args.len());

                let arg_pairs = lf_args.iter().cloned().zip(args);
                let free_pairs = lf_free.iter().cloned().zip(free.iter().cloned());
                let locals = arg_pairs.chain(free_pairs).collect();
                drop(addr);
                state.code = Code::Eval { expr, locals };

                Ok(true)
            }
            _ => Ok(false),
        }
    } else {
        Ok(false)
    }
}

fn eval_let(state: &mut State) -> Result<bool> {
    match &state.code {
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
                closure.free = ToValue::vals(&closure.lf.free, locals_rhs, &Default::default())?;
            }

            state.code = Code::Eval {
                expr,
                locals: next_locals,
            };
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn eval_case(state: &mut State) -> Result<bool> {
    match &mut state.code {
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
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn eval_constr_app(state: &mut State) -> Result<bool> {
    match &state.code {
        Code::Eval {
            expr: Expr::ConstrApp { constr, args },
            locals,
        } => {
            let args = ToValue::vals(args, locals, &state.globals)?;
            state.code = Code::ReturnCon { constr, args };
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn eval_literal(state: &mut State) -> Result<bool> {
    if let Code::Eval {
        expr: Expr::Lit(n), ..
    } = &state.code
    {
        state.code = Code::ReturnInt(*n);
        Ok(true)
    } else {
        Ok(false)
    }
}

fn eval_literal_app(state: &mut State) -> Result<bool> {
    if let Code::Eval {
        expr: Expr::VarApp { var, args },
        locals,
    } = &state.code
    {
        if let (Value::Int(n), true) = (var.val(locals, &state.globals)?, args.is_empty()) {
            state.code = Code::ReturnInt(n);
            Ok(true)
        } else {
            Ok(false)
        }
    } else {
        Ok(false)
    }
}

fn eval_prim_app(state: &mut State) -> Result<bool> {
    if let Code::Eval {
        expr: Expr::PrimApp { prim, args },
        locals,
    } = &state.code
    {
        let n = match (
            &prim[..],
            ToValue::vals(args, locals, &Default::default())?.as_slice(),
        ) {
            ("+#", [Value::Int(x), Value::Int(y)]) => x + y,
            ("-#", [Value::Int(x), Value::Int(y)]) => x - y,
            ("*#", [Value::Int(x), Value::Int(y)]) => x * y,
            ("/#", [Value::Int(x), Value::Int(y)]) => x / y,
            _ => return Ok(false),
        };
        state.code = Code::ReturnInt(n);
        Ok(true)
    } else {
        Ok(false)
    }
}
