use pest::{iterators::Pair, Parser};
use rand::{distributions::Distribution, rngs::SmallRng, SeedableRng};
use std::env;
use std::io::Read;

#[derive(pest_derive::Parser)]
#[grammar = "syntax.pest"]
struct Syntax;

/// The arithmetic operators our language supports.
#[derive(Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Our little programming language.
#[derive(Debug)]
enum Expr {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Literal(i64),
}

impl Expr {
    /// Translate a Pest parse tree into an expression.
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

    /// Evaluate the expression.
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

/// A random program generator.
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

    /// Generate a random expression.
    ///
    /// The expression is a literal with probablity 1/lit_prob_inv, and a binary expression
    /// otherwise. When we generate a binary expression, the two subtrees have double the
    /// probablity of generating literals.
    ///
    /// I haven't tried to work out the expected size/depth of the resulting tree, but it
    /// empirically seems to be in the ballpark of lit_prob_env.
    fn gen(&mut self, lit_prob_inv: u32) -> Expr {
        let dist = rand::distributions::Bernoulli::from_ratio(1, lit_prob_inv).unwrap();
        if dist.sample(&mut self.rng) {
            let unif = rand::distributions::Uniform::new(0i64, 100i64);
            Expr::Literal(unif.sample(&mut self.rng))
        } else {
            let lhs = Box::new(self.gen(lit_prob_inv / 2));
            let rhs = Box::new(self.gen(lit_prob_inv / 2));
            let unif = rand::distributions::Uniform::new(0u8, 4u8);
            let op = match unif.sample(&mut self.rng) {
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

/// Pretty-printing for expressions.
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

/// Parse the program from stdin.
fn parse_stdin() -> std::io::Result<Expr> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    let mut pairs = Syntax::parse(Rule::expr, &buffer).expect("syntax error");
    let pair = pairs.next().unwrap();
    Ok(Expr::parse(pair))
}

/// Generate a random program, with an optional seed taken from the second argv position.
fn generate() -> Expr {
    let seed = env::args().nth(2);
    let mut gen = match seed {
        Some(s) => Generator::new(s.parse().expect("seed must be a number")),
        None => Generator::default(),
    };
    gen.gen(100_000_000)
}

/// An extremely simple CLI. The commands are:
///
/// * `interp`: Read a program from stdin and run it.
/// * `pretty`: Read a program from stdin and print it back out on stdout.
/// * `gen [SEED]`: Generate a random program and print it out on stdout.
/// * `gen_interp [SEED]`: Generate a random program and run it.
fn main() {
    let mode = env::args().nth(1).unwrap_or("interp".to_string());
    match mode.as_str() {
        "interp" => {
            let expr = parse_stdin().unwrap();
            println!("{}", expr.interp());
        }
        "pretty" => {
            let expr = parse_stdin().unwrap();
            println!("{}", expr);
        }
        "gen" => {
            let expr = generate();
            println!("{}", expr);
        }
        "gen_interp" => {
            let expr = generate();
            println!("{}", expr.interp());
        }
        _ => {
            eprintln!("unknown mode: {}", mode);
        }
    }
}
