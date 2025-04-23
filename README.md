flatcalc
========

This repository contains two different implementations of a very simple calculator language.
The "language" is just binary integer operations, like `(47 - 26) * 2`.
We have a parser, pretty printer, interpreter, and random expression generator for this language.
The implementations are:

* On [the `main` branch][main], a "normal" version where the AST nodes are allocated normally and use [pointers][box] to refer to their children.
* On [the `flat` branch][flat], a "flattened" version where the AST nodes are packed together into one big array---i.e., [an *arena* or a *region*][region].
  The nodes refer to each other with plain [integer][u32] indices into the array.

The best way to view this repository is by [comparing the two branches][compare].
That way, you can see exactly what has to change to "flatten" the whole implementation.

[main]: https://github.com/sampsyo/flatcalc/tree/main
[flat]: https://github.com/sampsyo/flatcalc/tree/flat
[box]: https://doc.rust-lang.org/std/boxed/struct.Box.html
[u32]: https://doc.rust-lang.org/std/primitive.u32.html
[region]: https://en.wikipedia.org/wiki/Region-based_memory_management
[compare]: https://github.com/sampsyo/flatcalc/compare/main...flat#diff-42cb6807ad74b3e201c5a7ca98b911c5fa08380e942be6e4ac5807f8377f87fc

Run It
------

Use `flatcalc` or `flatcalc interp` to parse an expression from stdin and evaluate it.
There is also `flatcalc pretty`, which just pretty-prints the expression from stdin back out on stdout.

`flatcalc gen` randomly generates a really big expression.
There is an optional seed, so do `flatcalc gen 42` to get a deterministic expression.
You can also immediately run the randomly generated expression (without printing it) by typing `flatcalc gen_interp 42`.
This is nice for benchmarking.

This `flat` branch also adds `flat_interp` and `gen_flat_interp`, which demonstrate a different, even flatter interpretation strategy.

There is a `bench.sh` script that uses [Hyperfine][] for a quick performance measurement.

[hyperfine]: https://github.com/sharkdp/hyperfine

Author
------

This is by [Adrian Sampson][adrian].
I can't imagine why you'd want to use the code, but the license is [MIT][] nonetheless.

[adrian]: https://www.cs.cornell.edu/~asampson/
[mit]: https://choosealicense.com/licenses/mit/
