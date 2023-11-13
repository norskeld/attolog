# Attolog

Minimal [Prolog] interpreter.

## Description

This is mostly a learning project to know more about **F#** and **Prolog**.

The implementation is rather basic and therefore limited. It **does not** contain cuts, arithmetic, lists, equality and so on — only basic [Horn clauses][horn-clause].

## CLI

Available options:

- `-f`, `--file` A file to load before starting a REPL.

## REPL

Simply run the project:

```sh
$ dotnet run --project Attolog
```

With arguments:

```sh
$ dotnet run --project Attolog -- -f examples/sw.pl
```

### rlwrap

REPL can also be used with [rlwrap]:

```sh
$ rlwrap -a -N -t dumb dotnet run --project Attolog
```

With arguments:

```sh
$ rlwrap -a -N -t dumb dotnet run --project Attolog -- -f examples/sw.pl
```

### just

If you have [just] installed, you can use the following recipes:

```sh
$ just --list

Available recipes:
    default
    rl *args=''  # Run Attolog with rlwrap for a slightly better experience in REPL.
    run *args='' # Run Attolog with the default REPL.
```

Arguments can be passed as well:

```sh
$ just run -f examples/sw.pl
```

## Language

The language currently consists of:

- **Constants**. Example: `x`, `y`, `constant`
- **Variables**. Example: `X`, `Y`, `Variable`
- **Clauses**. Example: `son(abel, adam)`, `less(X, plus(X, one))`
- **Assertions**. Example: `son(X, Y) :- male(X), child(X, Y).`
- **Queries**. Example: `?- child(X, vader).`

Sample program:

```prolog
% Persons.
female(leia).
male(vader).
male(luke).
male(kylo).

% Relations.
child(luke, vader).
child(leia, vader).
child(kylo, leia).

% Assertions.
son(X, Y) :- male(X), child(X, Y).
daughter(X, Y) :- female(X), child(X, Y).
grandchild(X, Z) :- child(X, Y), child(Y, Z).

% Queries (goals) can't be part of a saved program.
% ?- son(X, vader).
% ?- daughter(X, vader).
% ?- grandchild(X, vader).
```

Peano numbers (using terms to represent unary numbers, e.g. `0`, `s(0)`, `s(s(0))`, `s(s(s(0)))`, etc.):

```prolog
add(0, N, N).
add(s(N), M, s(R)) :- add(N, M, R).

% ?- add(s(0), s(0), X).
% X = s(s(0))

% ?- add(s(0), X, s(s(0))).
% X = s(0)
```

## TODO

- [ ] Get rid of exceptions, embrace errors as values.
- [ ] Refactor solver to produce values instead of effects (direct printing, reading input, etc).

## License

[MIT](LICENSE).

<!-- Links. -->

[prolog]: https://en.wikipedia.org/wiki/Prolog
[horn-clause]: https://en.wikipedia.org/wiki/Horn_clause
[rlwrap]: https://github.com/hanslub42/rlwrap
[just]: https://github.com/casey/just
