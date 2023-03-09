.PHONY: bench
bench:
	cargo build --release
	hyperfine -m3 './target/release/flatcalc < expr.txt' \
		'./target/release/flatcalc flat_interp < expr.txt'
