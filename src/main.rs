use pest::{iterators::Pair, Parser};
use rand::Rng;
use std::env;
use std::io;
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

struct MyParser<r: io::BufRead> {
    reader: r,
}

impl<R: io::BufRead> MyParser<R> {
    fn new(reader: R) -> Self {
        Self {
            reader,
        }
    }
    
    /// Read digits from the input stream into `out` for a chunk of input. Return whether we should
    /// keep going, i.e., whether the digits hit the end of the chunk we looked at.
    fn read_digits(&mut self, out: &mut Vec<u8>) -> io::Result<bool> {
        let buf = self.reader.fill_buf()?;
        let buflen = buf.len();
        let num_digits = {
            let mut num_digits = buf.len();
            for i in 0..buf.len() {
                if !buf[i].is_ascii_digit() {
                    num_digits = i;
                    break;
                }
            }
            num_digits
        };
        if num_digits > 0 {
            out.extend_from_slice(buf[..num_digits].as_ref());
            self.reader.consume(num_digits);
        }
        Ok(num_digits == buflen)
    }
    
    fn parse_lit(&mut self) -> io::Result<Option<Expr>> {
        // TODO Skip entirely if the first character is not a digit?
        let mut digits: Vec<u8> = vec!();
        while self.read_digits(&mut digits)? { }
        if digits.is_empty() {
            Ok(None)
        } else {
            let num = std::str::from_utf8(&digits).unwrap().parse().unwrap();
            Ok(Some(Expr::Literal(num)))
        }
    }

    fn parse_expr(&mut self) -> io::Result<Option<Expr>> {
        self.parse_lit()
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

fn parse_stdin() -> std::io::Result<Expr> {
    let opt = MyParser::new(io::BufReader::new(io::stdin())).parse_expr()?;
    Ok(opt.expect("parse error"))
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
        let expr = Generator::default().gen(0.9999);
        println!("{}", expr);
    } else {
        eprintln!("unknown mode: {}", mode);
    }
}
