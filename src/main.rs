use pest::{Parser, iterators::Pair};
use std::io;
use rand::Rng;

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
                    BinOp::Add => lhs.wrapping_add(rhs),
                    BinOp::Sub => lhs.wrapping_sub(rhs),
                    BinOp::Mul => lhs.wrapping_mul(rhs),
                    BinOp::Div => lhs.checked_div(rhs).unwrap_or(0),
                }
            }
            Expr::Literal(num) => *num,
        }
    }
}

#[derive(Default)]
struct Generator {
    rng: rand::rngs::ThreadRng,
}

impl Generator {
    fn gen(&mut self, bin_prob: f64) -> Expr {
        if self.rng.gen::<f64>() > bin_prob {
            Expr::Literal(self.rng.gen_range(0..100))
        } else {
            let lhs = Box::new(self.gen(bin_prob.powi(2)));
            let rhs = Box::new(self.gen(bin_prob.powi(2)));
            let op = match self.rng.gen_range(0..4) {
                0 => BinOp::Add,
                1 => BinOp::Sub,
                2 => BinOp::Mul,
                3 => BinOp::Div,
                _ => unreachable!(),
            };
            Expr::Binary(op, lhs, rhs)
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Binary(op, lhs, rhs) => {
                let op = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                };
                write!(f, "({} {} {})", lhs, op, rhs)
            }
            Expr::Literal(num) => write!(f, "{}", num),
        }
    }
}

fn main() {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();

    let mut pairs = Syntax::parse(Rule::expr, &buffer).expect("syntax error");
    let pair = pairs.next().unwrap();
    let expr = Expr::parse(pair);
    println!("{}", expr);
    println!("{}", expr.interp());

    let other_expr = Generator::default().gen(0.999);
    println!("{}", other_expr);
    println!("{}", other_expr.interp());
}
