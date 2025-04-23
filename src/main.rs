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

struct HandParser<'a> {
    buf: &'a str,
}

impl<'a> HandParser<'a> {
    fn new(buf: &'a str) -> Self {
        HandParser { buf }
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
        while self.consume(|c| c == ' ').is_some() {}
    }

    fn parse_digits(&mut self) -> Option<i64> {
        let mut num = 0;
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

    fn parse_addsub(&mut self) -> Option<Expr> {
        let lhs = self.parse_mulexpr()?;
        self.skip_whitespace();
        let op = match self.consume(|c| c == '+' || c == '-')? {
            '+' => BinOp::Add,
            '-' => BinOp::Sub,
            _ => unreachable!(),
        };
        self.skip_whitespace();
        let rhs = self.parse_addexpr()?;
        Some(Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
    }

    fn parse_muldiv(&mut self) -> Option<Expr> {
        let lhs = self.parse_term()?;
        self.skip_whitespace();
        let op = match self.consume(|c| c == '*' || c == '/')? {
            '*' => BinOp::Mul,
            '/' => BinOp::Div,
            _ => unreachable!(),
        };
        self.skip_whitespace();
        let rhs = self.parse_mulexpr()?;
        Some(Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
    }

    fn maybe(&mut self, action: fn(&mut Self) -> Option<Expr>) -> Option<Expr> {
        let orig = self.buf;
        match action(self) {
            Some(expr) => Some(expr),
            None => {
                self.buf = orig;
                None
            }
        }
    }

    fn parse_term(&mut self) -> Option<Expr> {
        if self.consume(|c| c == '(').is_some() {
            self.skip_whitespace();
            let expr = self.parse_addexpr()?;
            self.skip_whitespace();
            self.consume(|c| c == ')')?;
            Some(expr)
        } else {
            self.parse_int().map(|n| Expr::Literal(n))
        }
    }

    fn parse_addexpr(&mut self) -> Option<Expr> {
        match self.maybe(Self::parse_addsub) {
            Some(expr) => Some(expr),
            None => self.parse_mulexpr(),
        }
    }

    fn parse_mulexpr(&mut self) -> Option<Expr> {
        match self.maybe(Self::parse_muldiv) {
            Some(expr) => Some(expr),
            None => self.parse_term(),
        }
    }

    fn parse(buf: &str) -> Option<Expr> {
        let mut parser = HandParser::new(buf);
        parser.skip_whitespace();
        let res = parser.parse_addexpr()?;
        parser.skip_whitespace();
        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blug() {
        let expr = HandParser::parse("1 * (0-2) * 3/2").unwrap();
        dbg!(expr);
    }
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
    let mode = env::args().nth(1).unwrap_or_else(|| "interp".to_string());
    match mode.as_str() {
        "interp" => {
            let expr = parse_stdin().unwrap();
            println!("{}", expr.interp());
            #[cfg(feature = "nofree")]
            std::mem::forget(expr);
        }
        "pretty" => {
            let expr = parse_stdin().unwrap();
            println!("{}", expr);
            #[cfg(feature = "nofree")]
            std::mem::forget(expr);
        }
        "gen" => {
            let expr = generate();
            println!("{}", expr);
            #[cfg(feature = "nofree")]
            std::mem::forget(expr);
        }
        "gen_interp" => {
            let expr = generate();
            println!("{}", expr.interp());
            #[cfg(feature = "nofree")]
            std::mem::forget(expr);
        }
        _ => {
            eprintln!("unknown mode: {}", mode);
        }
    }
}
