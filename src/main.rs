use pest::{iterators::Pair, Parser};
use rand::{rngs::SmallRng, Rng, SeedableRng, distributions::Distribution};
use std::env;
use std::io::Read;

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
            Rule::addExpr | Rule::mulExpr => {
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

struct Generator {
    rng: SmallRng,
}

impl Generator {
    fn new(seed: u64) -> Self {
        Self {
            rng: SmallRng::seed_from_u64(seed),
        }
    }

    fn default() -> Self {
        Self {
            rng: SmallRng::from_entropy(),
        }
    }

    fn gen(&mut self, lit_prob_inv: u32) -> Expr {
        let dist = rand::distributions::Bernoulli::from_ratio(1, lit_prob_inv).unwrap();
        if dist.sample(&mut self.rng) {
            Expr::Literal(self.rng.gen_range(0..100))
        } else {
            let lhs = Box::new(self.gen(lit_prob_inv / 2));
            let rhs = Box::new(self.gen(lit_prob_inv / 2));
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

fn parse_stdin() -> std::io::Result<Expr> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    let mut pairs = Syntax::parse(Rule::expr, &buffer).expect("syntax error");
    let pair = pairs.next().unwrap();
    Ok(Expr::parse(pair))
}

fn generate() -> Expr {
    let seed = env::args().nth(2);
    let mut gen = match seed {
        Some(s) => Generator::new(s.parse().expect("seed must be a number")),
        None => Generator::default(),
    };
    gen.gen(10000000)
}

fn main() {
    let mode = env::args().nth(1).unwrap_or("interp".to_string());

    if mode == "interp" {
        let expr = parse_stdin().unwrap();
        println!("{}", expr.interp());
    } else if mode == "pretty" {
        let expr = parse_stdin().unwrap();
        println!("{}", expr);
    } else if mode == "gen" {
        println!("{}", generate());
    } else if mode == "gen_interp" {
        let expr = generate();
        println!("{}", expr.interp());
    } else {
        eprintln!("unknown mode: {}", mode);
    }
}
