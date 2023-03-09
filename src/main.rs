use pest::{iterators::Pair, Parser};
use rand::{rngs::SmallRng, Rng, SeedableRng};
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

#[derive(Debug, Clone, Copy)]
struct ExprRef(u32);

#[derive(Debug)]
enum Expr {
    Binary(BinOp, ExprRef, ExprRef),
    Literal(i64),
}

#[derive(Default)]
struct ExprPool(Vec<Expr>);

impl ExprPool {
    fn get(&self, expr: ExprRef) -> &Expr {
        &self.0[expr.0 as usize]
    }

    fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = self.0.len();
        self.0.push(expr);
        ExprRef(idx.try_into().expect("too many exprs in the pool"))
    }

    fn parse(&mut self, tree: Pair<Rule>) -> ExprRef {
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
                let expr = Expr::Binary(op, self.parse(lhs), self.parse(rhs));
                self.add(expr)
            }
            Rule::number => {
                let num = tree.as_str().parse().unwrap();
                self.add(Expr::Literal(num))
            }
            _ => unreachable!(),
        }
    }

    fn interp(&self, expr: ExprRef) -> i64 {
        match self.get(expr) {
            Expr::Binary(op, lhs, rhs) => {
                let lhs = self.interp(*lhs);
                let rhs = self.interp(*rhs);
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

    fn flat_interp(&self, root: ExprRef) -> i64 {
        let mut state: Vec<i64> = vec![0; self.0.len()];
        for (i, expr) in self.0.iter().enumerate() {
            let res = match expr {
                Expr::Binary(op, lhs, rhs) => {
                    let lhs = state[lhs.0 as usize];
                    let rhs = state[rhs.0 as usize];
                    match op {
                        BinOp::Add => lhs.wrapping_add(rhs),
                        BinOp::Sub => lhs.wrapping_sub(rhs),
                        BinOp::Mul => lhs.wrapping_mul(rhs),
                        BinOp::Div => lhs.checked_div(rhs).unwrap_or(0),
                    }
                }
                Expr::Literal(num) => *num
            };
            state[i] = res;
        }
        state[root.0 as usize]
    }
}

struct Generator {
    rng: SmallRng,
    pool: ExprPool,
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

    fn gen(&mut self, bin_prob: f64) -> Expr {
        if self.rng.gen::<f64>() > bin_prob {
            self.pool.add(Expr::Literal(self.rng.gen_range(0..100)))
        } else {
            let lhs = self.gen(bin_prob.powi(2));
            let rhs = self.gen(bin_prob.powi(2));
            let op = match self.rng.gen_range(0..4) {
                0 => BinOp::Add,
                1 => BinOp::Sub,
                2 => BinOp::Mul,
                3 => BinOp::Div,
                _ => unreachable!(),
            };
            self.pool.add(Expr::Binary(op, lhs, rhs))
        }
    }
}

struct ExprDisplay<'a> {
    pool: &'a ExprPool,
    expr: ExprRef,
}

impl<'a> std::fmt::Display for ExprDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.pool.get(self.expr) {
            Expr::Binary(op, lhs, rhs) => {
                let op = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                };
                write!(
                    f,
                    "({} {} {})",
                    ExprDisplay {
                        pool: self.pool,
                        expr: *lhs
                    },
                    op,
                    ExprDisplay {
                        pool: self.pool,
                        expr: *rhs
                    }
                )
            }
            Expr::Literal(num) => write!(f, "{}", num),
        }
    }
}

fn parse_stdin(pool: &mut ExprPool) -> std::io::Result<ExprRef> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    let mut pairs = Syntax::parse(Rule::expr, &buffer).expect("syntax error");
    let pair = pairs.next().unwrap();
    Ok(pool.parse(pair))
}

fn generate() -> Expr {
    let seed = env::args().nth(2);
    let mut gen = match seed {
        Some(s) => Generator::new(s.parse().expect("seed must be a number")),
        None => Generator::default(),
    };
    gen.gen(0.9999999)
}

fn main() {
    let mut pool = ExprPool::default();
    let mode = env::args().nth(1).unwrap_or("interp".to_string());

    if mode == "interp" {
        let expr = parse_stdin(&mut pool).unwrap();
        println!("{}", pool.interp(expr));
    } else if mode == "pretty" {
        let expr = parse_stdin(&mut pool).unwrap();
        println!("{}", ExprDisplay { pool: &pool, expr });
    } else if mode == "gen" {
        let expr = generate();
        println!("{}", ExprDisplay { pool: &pool, expr });
    } else if mode == "flat_interp" {
        let expr = parse_stdin(&mut pool).unwrap();
        println!("{}", pool.flat_interp(expr));
    } else if mode == "gen_interp" {
        let expr = generate();
        println!("{}", expr.interp());
    } else {
        eprintln!("unknown mode: {}", mode);
    }
}
