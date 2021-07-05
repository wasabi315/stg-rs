use super::ast::*;

pub fn and() -> Program {
    stg! {
        and = {} n {x,y} -> {
            case { x {} } of
                :True {} -> { y {} }
                :False {} -> { :False {} }
                _ -> { undefined# {} }
        }
    }
}

pub fn or() -> Program {
    stg! {
        or = {} n {x,y} -> {
            case { x {} } of
                :False {} -> { y {} }
                :True {} -> { :True {} }
                _ -> { undefined# {} }
        }
    }
}

pub fn not() -> Program {
    stg! {
        not = {} n {x} -> {
            case { x {} } of
                :True {} -> { :False {} }
                :False {} -> { :True {} }
                _ -> { undefined# {} }
        }
    }
}

pub fn fix() -> Program {
    stg! {
        fix = {} n {f} -> {
            let rec {
                x = {f, x} u {} -> { f {x} }
            } in
                x {}
        }
    }
}

pub fn seq() -> Program {
    stg! {
        seq = {} n {x, y} -> {
            case { x {} } of
                _ -> { y {} }
        }
    }
}

pub fn id() -> Program {
    stg! {
        id = {} n {x} -> { x {} }
    }
}

pub fn const_() -> Program {
    stg! {
        const_ = {} n {x, y} -> { x {} }
    }
}

pub fn compose() -> Program {
    stg! {
        compose = {} n {f, g, x} -> {
            let {
                gx = {g, x} u {} -> { g {x} }
            } in
                f {gx}
        }
    }
}

pub fn fst() -> Program {
    stg! {
        fst = {} n {x} -> {
            case { x {} } of
                :Pair {a, b} -> { a {} }
                _ -> { undefined# {} }
        }
    }
}

pub fn snd() -> Program {
    stg! {
        fst = {} n {x} -> {
            case { x {} } of
                :Pair {a, b} -> { b {} }
                _ -> { undefined# {} }
        }
    }
}

pub fn curry() -> Program {
    stg! {
        curry = {} n {f, x, y} -> {
            let {
                p = {x, y} u {} -> { :Pair {x, y} }
            } in
                f {p}
        }
    }
}

pub fn uncurry() -> Program {
    stg! {
        uncurry = {} n {f, p} -> {
            case { p {} } of
                :Pair {x, y} -> { f {x, y} }
                _ -> { undefined# {} }
        }
    }
}

pub fn swap() -> Program {
    stg! {
        swap = {} n {p} -> {
            case { p {} } of
                :Pair {x, y} -> { :Pair {y, x} }
                _ -> { undefined# {} }
        }
    }
}
