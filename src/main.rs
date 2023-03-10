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

#[derive(Debug, Clone, Copy)]
struct ExprRef(u32);

/// Our little programming language.
#[derive(Debug)]
enum Expr {
    Binary(BinOp, ExprRef, ExprRef),
    Literal(i64),
}

struct ExprPool(Vec<Expr>);

impl ExprPool {
    fn default() -> Self {
        Self(Vec::with_capacity(100_000_000))
    }

    fn get(&self, expr: ExprRef) -> &Expr {
        &self.0[expr.0 as usize]
    }

    fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = self.0.len();
        self.0.push(expr);
        ExprRef(idx.try_into().expect("too many exprs in the pool"))
    }

    /// Translate a Pest parse tree into an expression.
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

    /// Evaluate the expression.
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

    fn flat_interp(self, root: ExprRef) -> i64 {
        let mut state: Vec<i64> = vec![0; self.0.len()];
        for (i, expr) in self.0.into_iter().enumerate() {
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
                Expr::Literal(num) => num,
            };
            state[i] = res;
        }
        state[root.0 as usize]
    }

    /// Wrap an expression in a type that implements `Display` for easy formatting.
    fn disp(&self, expr: ExprRef) -> ExprDisplay {
        ExprDisplay { pool: self, expr }
    }
}

/// A random program generator.
struct Generator {
    rng: SmallRng,
    pool: ExprPool,
}

impl Generator {
    fn new(seed: u64) -> Self {
        Self {
            rng: SmallRng::seed_from_u64(seed),
            pool: ExprPool::default(),
        }
    }

    fn default() -> Self {
        Self {
            rng: SmallRng::from_entropy(),
            pool: ExprPool::default(),
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
    fn gen(&mut self, lit_prob_inv: u32) -> ExprRef {
        let dist = rand::distributions::Bernoulli::from_ratio(1, lit_prob_inv).unwrap();
        if dist.sample(&mut self.rng) {
            let unif = rand::distributions::Uniform::new(0i64, 100i64);
            self.pool.add(Expr::Literal(unif.sample(&mut self.rng)))
        } else {
            let lhs = self.gen(lit_prob_inv / 2);
            let rhs = self.gen(lit_prob_inv / 2);
            let unif = rand::distributions::Uniform::new(0u8, 4u8);
            let op = match unif.sample(&mut self.rng) {
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

/// Pretty-printing for expressions.
///
/// Because we can't print `ExprRef`s alone, this wrapper associates one with a pool so we can
/// format it. Use `pool.disp(expr)` to create this wrapper conveniently.
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
                    self.pool.disp(*lhs),
                    op,
                    self.pool.disp(*rhs)
                )
            }
            Expr::Literal(num) => write!(f, "{}", num),
        }
    }
}

/// Parse the program from stdin.
fn parse_stdin(pool: &mut ExprPool) -> std::io::Result<ExprRef> {
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    let mut pairs = Syntax::parse(Rule::expr, &buffer).expect("syntax error");
    let pair = pairs.next().unwrap();
    Ok(pool.parse(pair))
}

/// Generate a random program, with an optional seed taken from the second argv position.
fn generate() -> (ExprPool, ExprRef) {
    let seed = env::args().nth(2);
    let mut gen = match seed {
        Some(s) => Generator::new(s.parse().expect("seed must be a number")),
        None => Generator::default(),
    };
    let expr = gen.gen(100_000_000);
    (gen.pool, expr)
}

/// An extremely simple CLI. The commands are:
///
/// * `interp`: Read a program from stdin and run it.
/// * `pretty`: Read a program from stdin and print it back out on stdout.
/// * `gen [SEED]`: Generate a random program and print it out on stdout.
/// * `gen_interp [SEED]`: Generate a random program and run it.
fn main() {
    let mode = env::args().nth(1).unwrap_or_else(|| "interp".to_string());
    match mode.as_str() {
        "interp" => {
            let mut pool = ExprPool::default();
            let expr = parse_stdin(&mut pool).unwrap();
            println!("{}", pool.interp(expr));
        }
        "pretty" => {
            let mut pool = ExprPool::default();
            let expr = parse_stdin(&mut pool).unwrap();
            println!("{}", pool.disp(expr));
        }
        "gen" => {
            let (pool, expr) = generate();
            println!("{}", pool.disp(expr));
        }
        "gen_interp" => {
            let (pool, expr) = generate();
            println!("{}", pool.interp(expr));
        }
        "flat_interp" => {
            let mut pool = ExprPool::default();
            let expr = parse_stdin(&mut pool).unwrap();
            println!("{}", pool.flat_interp(expr));
        }
        "gen_flat_interp" => {
            let (pool, expr) = generate();
            println!("{}", pool.flat_interp(expr));
        }
        _ => {
            eprintln!("unknown mode: {}", mode);
        }
    }
}
