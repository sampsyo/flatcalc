#!/bin/sh
seed=12345
hyperfine --export-csv bench.csv -w1 \
  --prepare 'git checkout main ; cargo build --release' \
    -n 'normal' "./target/release/flatcalc gen_interp $seed" \
  --prepare 'git checkout main ; cargo build --release --features nofree' \
    -n 'normal nofree' "./target/release/flatcalc gen_interp $seed" \
  --prepare 'git checkout flat ; cargo build --release' \
    -n 'flat' "./target/release/flatcalc gen_interp $seed" \
  --prepare 'git checkout flat ; cargo build --release --features nofree' \
    -n 'flat nofree' "./target/release/flatcalc gen_interp $seed" \
  --prepare 'git checkout flat ; cargo build --release' \
    -n 'extra-flat' "./target/release/flatcalc gen_flat_interp $seed"
