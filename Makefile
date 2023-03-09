.PHONY: bench
bench:
	cargo build --release
	hyperfine -m3 './target/release/flatcalc gen_interp 12345'
