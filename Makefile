.PHONY: bench
bench:
	cargo build --release
	hyperfine -w1 './target/release/flatcalc gen_interp 12345'
