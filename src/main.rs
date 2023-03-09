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
    Num(i64),
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
                Expr::Num(num)
            }
            _ => unreachable!(),
        }
    }
}

fn main() {
    let mut pairs = Syntax::parse(Rule::expr, "1 + 2").expect("syntax error");
    let pair = pairs.next().unwrap();
    let expr = Expr::parse(dbg!(pair));
    dbg!(expr);
}
