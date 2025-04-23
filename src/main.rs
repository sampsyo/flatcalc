use fastrand::Rng;
use std::env;
use std::io::Read;

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
    Binary(BinOp, ExprRef, ExprRef),
    Literal(i64),
}

/// The thing we use instead of pointers to refer to expressions.
///
/// In this "flattened" implementation, references are just indices into a vector where the `Expr`s
/// are stored. We use the "newtype" pattern instead of a plain `u32` here to clarify where the
/// reference is supposed to be used.
#[derive(Debug, Clone, Copy)]
struct ExprRef(u32);

/// An "arena" for storing expressions that can refer to each other.
///
/// This is just a plain, dense array where a family of `Expr`s live.
struct ExprPool(Vec<Expr>);

impl ExprPool {
    /// Create an empty pool.
    fn default() -> Self {
        Self(Vec::with_capacity(100_000_000))
    }

    /// Dereference an AST node reference, obtaining the underlying `Expr`.
    fn get(&self, expr: ExprRef) -> &Expr {
        &self.0[expr.0 as usize]
    }

    /// Add an expression to the pool and get a reference to it.
    fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = self.0.len();
        self.0.push(expr);
        ExprRef(idx.try_into().expect("too many exprs in the pool"))
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

    /// An alternative interpreter that exploits the flat structure.
    ///
    /// Instead of recursively traversing from the root, we take advantage of the fact that our
    /// expressions only refer "backward" in the pool. Therefore, it suffices to evaluate each
    /// expression in the pool *in order*. No recursion required.
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

/// A parser for our language.
///
/// Requires fully parenthesized syntax, like (1*2)+(3-4).
struct Parser<'a, 'b> {
    pool: &'a mut ExprPool,
    buf: &'b str,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(pool: &'a mut ExprPool, buf: &'b str) -> Self {
        Self { pool, buf }
    }

    fn consume(&mut self, char_pred: fn(char) -> bool) -> Option<char> {
        let first = self.buf.chars().next()?;
        if char_pred(first) {
            self.buf = &self.buf[first.len_utf8()..];
            Some(first)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while self
            .consume(|c| c == ' ' || c == '\n' || c == '\t')
            .is_some()
        {}
    }

    fn parse_digits(&mut self) -> Option<i64> {
        let first = self.consume(|c| c.is_ascii_digit())?; // Require at least one digit.
        let mut num = first as i64 - b'0' as i64;
        while let Some(digit) = self.consume(|c| c.is_ascii_digit()) {
            num = num * 10 + (digit as i64 - b'0' as i64);
        }
        Some(num)
    }

    fn parse_int(&mut self) -> Option<i64> {
        let negative = self.consume(|c| c == '-').is_some();
        let digits = self.parse_digits()?;
        Some(if negative { -digits } else { digits })
    }

    fn parse_expr(&mut self) -> Option<ExprRef> {
        let lhs = self.parse_term()?;
        self.skip_whitespace();
        let op = match self.consume(|c| c == '+' || c == '-' || c == '*' || c == '/') {
            Some(c) => match c {
                '+' => BinOp::Add,
                '-' => BinOp::Sub,
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                _ => unreachable!(),
            },
            None => {
                return Some(lhs);
            }
        };
        self.skip_whitespace();
        let rhs = self.parse_term()?;
        Some(self.pool.add(Expr::Binary(op, lhs, rhs)))
    }

    fn parse_term(&mut self) -> Option<ExprRef> {
        if self.consume(|c| c == '(').is_some() {
            self.skip_whitespace();
            let expr = self.parse_expr()?;
            self.skip_whitespace();
            self.consume(|c| c == ')')?;
            Some(expr)
        } else {
            self.parse_int().map(|i| self.pool.add(Expr::Literal(i)))
        }
    }

    fn parse(pool: &mut ExprPool, buf: &str) -> Option<ExprRef> {
        let mut parser = Parser::new(pool, buf);
        parser.skip_whitespace();
        let res = parser.parse_expr()?;
        parser.skip_whitespace();
        if parser.buf.is_empty() {
            Some(res)
        } else {
            None
        }
    }
}

/// A random program generator.
struct Generator {
    rng: Rng,
    pool: ExprPool,
}

impl Generator {
    fn new(seed: u64) -> Self {
        Self {
            rng: Rng::with_seed(seed),
            pool: ExprPool::default(),
        }
    }

    fn default() -> Self {
        Self {
            rng: Rng::new(),
            pool: ExprPool::default(),
        }
    }

    /// Generate a random expression.
    ///
    /// The expression is a literal with probability 1/`lit_prob_inv`, and a binary expression
    /// otherwise. When we generate a binary expression, the two subtrees have double the
    /// probability of generating literals.
    ///
    /// I am not very good at statistics, but I think the expected tree size is `lit_prob_inv`.
    /// Let s(i) be the size of a tree generated by `gen(i)` (i.e., i is a shorthand for
    /// `lit_prob_env`). s(i) is of course a random variable:
    ///
    ///     s(i) = 1                    with probability 1/i
    ///     s(i) = 1 + s(i/2) + s(i/2)  with probability 1-1/i
    ///
    /// I think it's correct to say that the expected size is therefore:
    ///
    ///     E[s(i)] = (1/i) * 1 + (1-1/i) * (1 + E[s(i/2)] + E[s(i/2)])
    ///
    /// To simplify notation, let e(i) be the expected size, i.e., E[s(i)]. Then we have the
    /// recurrence:
    ///
    ///     e(i) = 1/i + (1-1/i) * (1 + 2*e(i/2))
    ///
    /// My claim above is that e(i) = i. We can confirm that this solves the recurrence (for i>0):
    ///
    ///     i = 1/i + (1-1/i) * (1 + 2*(i/2))
    ///     i = 1/i + (1-1/i) * (1 + i)
    ///     i = 1/i + 1 + i - 1/i - i/i
    ///     i = i
    ///
    /// Anecdotally, this also seems to be the right ballpark in practice.
    fn generate(&mut self, lit_prob_inv: u32) -> ExprRef {
        if self.rng.u32(0..lit_prob_inv) == 0 {
            self.pool.add(Expr::Literal(self.rng.i64(0..100)))
        } else {
            let lhs = self.generate(lit_prob_inv / 2);
            let rhs = self.generate(lit_prob_inv / 2);
            let op = match self.rng.u8(0..4) {
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
    let expr = Parser::parse(pool, &buffer).expect("syntax error");
    Ok(expr)
}

/// Generate a random program, with an optional seed taken from the second argv position.
fn generate() -> (ExprPool, ExprRef) {
    let seed = env::args().nth(2);
    let mut genr = match seed {
        Some(s) => Generator::new(s.parse().expect("seed must be a number")),
        None => Generator::default(),
    };
    let expr = genr.generate(100_000_000);
    (genr.pool, expr)
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
            #[cfg(feature = "nofree")]
            std::mem::forget(pool);
        }
        "pretty" => {
            let mut pool = ExprPool::default();
            let expr = parse_stdin(&mut pool).unwrap();
            println!("{}", pool.disp(expr));
            #[cfg(feature = "nofree")]
            std::mem::forget(pool);
        }
        "gen" => {
            let (pool, expr) = generate();
            println!("{}", pool.disp(expr));
            #[cfg(feature = "nofree")]
            std::mem::forget(pool);
        }
        "gen_interp" => {
            let (pool, expr) = generate();
            println!("{}", pool.interp(expr));
            #[cfg(feature = "nofree")]
            std::mem::forget(pool);
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
