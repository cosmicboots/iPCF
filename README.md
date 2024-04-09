# iPCF

Intensional Programming Computable Functions (iPCF) extends the PCF lambda
calculus language by adding intentionality, and was introduced in a [paper by
G. A. Kavvos](https://seis.bristol.ac.uk/~tz20861/papers/ipcf.pdf).

This project aims to implement an interpreter for iPCF in OCaml.

## Building the Project

This project is written in OCaml, and thus it's recommended to use the
[Dune](https://dune.build/) build system to build the project.

Assuming you have the OCaml tool chain (including Dune and
[opam](https://opam.ocaml.org/)) installed, you can setup an environment and
install all the project dependencies with the following:

```shell
opam switch create .
```

At this point you should have a local switch (OCaml's term for environment)
setup in the `./_opam` directory.

You can then build the project with:

```shell
dune build
```

Which will output the `ipcf` binary at `./_build/install/default/bin/ipcf`.

Unit tests can be run with `dune test`, and running the binary directly can be
done with `dune exec ipcf`.

## The Interpreter

Starting the interpreter is as simple as calling the `ipcf` binary:
```
❯ ./ipcf

=========================
Welcome to the iPCF REPL!
=========================

You can exit the REPL with either [exit] or [CTRL+D]

iPCF>
```

### Storing Expressions

Expressions can be saved in the REPL by assigning them to a variable using the
walrus operator (`:=`):
```
iPCF> kcomb := \x .\y . x
kcomb : 'a -> 'b -> 'a
```

These expressions can then be reused in later expressions:
```
iPCF> kcomb true false
true : 'a
```

### Built-in REPL Commands

The interpreter supports a few special commands, which are all prefixed with a
colon:

- `:quit`: Exits the REPL
- `:ctx`: Prints all the expressions that are stored in the REPL
- `:load <file>` or `:l <file>`: Loads a file with iPCF expressions and evaluates them

## Language Syntax

### Primitive Types

The language supports natural numbers and booleans as primitive data types.

**Booleans**:
```
iPCF> true
true : Bool
iPCF> false
false : Bool
```

**Natural Numbers** are encoded using the [Church
Encoding](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals):
```
iPCF> zero
0 : Nat
iPCF> succ zero
1 : Nat
iPCF> succ succ zero
2 : Nat
```

The interpreter also supports the `pred` and "is zero" (`?`)
functions:
```
iPCF> pred zero
pred 0 : Nat
iPCF> pred succ zero
0 : Nat
iPCF> pred succ succ zero
1 : Nat
iPCF> zero?
true : Bool
iPCF> (succ zero)?
false : Bool
```

Constant numerals can also be used and will internally expanded to the Church
Encoding:
```
iPCF> 0
0 : Nat
iPCF> 1
1 : Nat
iPCF> 42
42 : Nat
```

### Control Flow

If-then-else expressions are supported with the following syntax:
```
if <condition> then <true-branch> else <false-branch>
```

For example:
```
iPCF> if true then succ 0 else 0
1 : Nat
```

### Abstraction and Application

Lambda abstractions are supported with the following syntax:
```
\ <variable> . <body>
```

For example, the I combinator (identity function) would be written as:
```
iPCF> \x . x
(fun) : 'a -> 'a
```

And the K combinator as:
```
iPCF> \x . \y . x
(fun) : 'a -> 'b -> 'a
```

### Intensional Operations

TODO

