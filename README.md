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

Author
------

This is by [Adrian Sampson][adrian].
I can't imagine why you'd want to use the code, but the license is [MIT][] nonetheless.

[adrian]: https://www.cs.cornell.edu/~asampson/
[mit]: https://choosealicense.com/licenses/mit/
