use pest::{Parser, iterators::Pair};

#[derive(pest_derive::Parser)]
#[grammar = "syntax.pest"]
struct Syntax;

#[derive(Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
enum Expr {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Literal(i64),
}

impl Expr {
    fn parse(tree: Pair<Rule>) -> Self {
        match tree.as_rule() {
            Rule::addExpr => {
                let mut pairs = tree.into_inner();
                let lhs = pairs.next().unwrap();
                let op = pairs.next().unwrap();
                let rhs = pairs.next().unwrap();
                let op = match op.as_rule() {
                    Rule::add => BinOp::Add,
                    Rule::sub => BinOp::Sub,
                    Rule::mul => BinOp::Mul,
                    Rule::div => BinOp::Div,
                    _ => unreachable!(),
                };
                Expr::Binary(op, Box::new(Expr::parse(lhs)), Box::new(Expr::parse(rhs)))
            }
            Rule::number => {
                dbg!(tree.as_str());
                let num = tree.as_str().parse().unwrap();
                Expr::Literal(num)
            }
            _ => unreachable!(),
        }
    }

    fn interp(&self) -> i64 {
        match self {
            Expr::Binary(op, lhs, rhs) => {
                let lhs = lhs.interp();
                let rhs = rhs.interp();
                match op {
                    BinOp::Add => lhs + rhs,
                    BinOp::Sub => lhs - rhs,
                    BinOp::Mul => lhs * rhs,
                    BinOp::Div => lhs / rhs,
                }
            }
            Expr::Literal(num) => *num,
        }
    }
}

fn main() {
    let mut pairs = Syntax::parse(Rule::expr, "1 + 2").expect("syntax error");
    let pair = pairs.next().unwrap();
    let expr = dbg!(Expr::parse(dbg!(pair)));
    println!("{}", expr.interp());
}
