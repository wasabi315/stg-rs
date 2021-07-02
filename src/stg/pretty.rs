use pretty::{BoxAllocator, BuildDoc, DocAllocator, DocBuilder};
use std::io;

use super::ast::*;

const TAB_SIZE: isize = 4;
const LINE_LEN: usize = 80;

pub fn pretty<W>(program: &Program, out: &mut W) -> io::Result<()>
where
    W: ?Sized + io::Write,
{
    let allocator = &BoxAllocator;
    program.pretty::<_, ()>(allocator).1.render(LINE_LEN, out)
}

impl Binds {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        allocator.intersperse(
            self.0
                .iter()
                .map(|(v, lf)| allocator.text(v).append(" = ").append(lf.pretty(allocator))),
            allocator.hardline().append(allocator.hardline()),
        )
    }
}

impl LambdaForm {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        braces_and_sep_by_comma(allocator, &self.free)
            .append(allocator.text(if self.updatable { " \\u " } else { " \\n " }))
            .append(braces_and_sep_by_comma(allocator, &self.args))
            .append(allocator.text(" ->"))
            .append(
                allocator
                    .hardline()
                    .append(self.expr.pretty(allocator))
                    .nest(TAB_SIZE),
            )
    }
}

impl Expr {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            Expr::Let { rec, binds, expr } => allocator
                .text(if *rec { "let rec" } else { "let" })
                .append(
                    allocator
                        .hardline()
                        .append(binds.pretty(allocator))
                        .nest(TAB_SIZE),
                )
                .append(allocator.hardline())
                .append("in")
                .append(
                    allocator
                        .hardline()
                        .append(expr.pretty(allocator))
                        .nest(TAB_SIZE),
                ),
            Expr::Case { expr, alts } => allocator
                .text("case ")
                .append(expr.pretty(allocator))
                .append(allocator.text(" of"))
                .append(
                    allocator
                        .hardline()
                        .append(alts.pretty(allocator))
                        .nest(TAB_SIZE),
                ),
            Expr::VarApp { var, args } => {
                allocator
                    .text(var)
                    .append(allocator.text(" "))
                    .append(braces_and_sep_by_comma(
                        allocator,
                        args.iter().map(|arg| arg.pretty(allocator)),
                    ))
            }
            Expr::ConstrApp { constr, args } => allocator
                .text(constr)
                .append(allocator.text(" "))
                .append(braces_and_sep_by_comma(
                    allocator,
                    args.iter().map(|arg| arg.pretty(allocator)),
                )),
            Expr::PrimApp { prim, args } => allocator
                .text(prim)
                .append(allocator.text(" "))
                .append(braces_and_sep_by_comma(
                    allocator,
                    args.iter().map(|arg| arg.pretty(allocator)),
                )),
            Expr::Lit(n) => allocator.text(n.to_string()),
        }
    }
}

impl Alts {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match &self.0 {
            NonDefAlts::Empty => self.1.pretty(allocator),
            _ => self
                .0
                .pretty(allocator)
                .append(allocator.hardline().append(allocator.hardline()))
                .append(self.1.pretty(allocator)),
        }
    }
}

impl NonDefAlts {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            NonDefAlts::Empty => allocator.nil(),
            NonDefAlts::AlgAlts(aalts) => allocator.intersperse(
                aalts.iter().map(|AlgAlt { constr, vars, expr }| {
                    allocator
                        .text(constr)
                        .append(allocator.text(" "))
                        .append(braces_and_sep_by_comma(allocator, vars))
                        .append(allocator.text(" ->"))
                        .append(
                            allocator
                                .hardline()
                                .append(expr.pretty(allocator))
                                .nest(TAB_SIZE),
                        )
                }),
                allocator.hardline().append(allocator.hardline()),
            ),
            NonDefAlts::PrimAlts(palts) => allocator.intersperse(
                palts.iter().map(|PrimAlt { lit, expr }| {
                    allocator
                        .text(lit.to_string())
                        .append(allocator.text(" ->"))
                        .append(
                            allocator
                                .hardline()
                                .append(expr.pretty(allocator))
                                .nest(TAB_SIZE),
                        )
                }),
                allocator.hardline().append(allocator.hardline()),
            ),
        }
    }
}

impl DefAlt {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            DefAlt::VarAlt { var, expr } => {
                allocator.text(var).append(allocator.text(" ->")).append(
                    allocator
                        .hardline()
                        .append(expr.pretty(allocator))
                        .nest(TAB_SIZE),
                )
            }
            DefAlt::DefAlt { expr } => allocator.text("default ->").append(
                allocator
                    .hardline()
                    .append(expr.pretty(allocator))
                    .nest(TAB_SIZE),
            ),
        }
    }
}

impl Atom {
    fn pretty<'a, D, A>(&'a self, allocator: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
    {
        match self {
            Atom::Var(v) => allocator.text(v),
            Atom::Lit(n) => allocator.text(n.to_string()),
        }
    }
}

fn braces_and_sep_by_comma<'a, D, A, I>(allocator: &'a D, docs: I) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
    A: Clone,
    I: IntoIterator,
    I::Item: Into<BuildDoc<'a, D::Doc, A>>,
{
    allocator
        .text("{")
        .append(allocator.intersperse(docs, allocator.text(",")))
        .append("}")
}
