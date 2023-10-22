# Attolog

Minimal [Prolog] interpreter.

## Description

This is mostly a learning project to know more about **F#** and **Prolog**.

The implementation is rather basic and therefore limited. It **does not** contain cuts, arithmetic, lists, equality and so on — only basic [Horn clauses][horn-clause].

## REPL

Simply run the project:

```sh
dotnet run --project Attolog
```

### rlwrap

REPL can also be used with [rlwrap]:

```sh
rlwrap -a -N -t dumb dotnet run --project Attolog
```

### just

If you have [just] installed, you can use the following recipes:

```sh
$ just --list

Available recipes:
    default
    rl      # Run Attolog with rlwrap for a slightly better experience in REPL.
    run     # Run Attolog with the default REPL.
```

## Language

The language currently consists of:

- **Constants**. Example: `x`, `y`, `constant`
- **Variables**. Example: `X`, `Y`, `Variable`
- **Clauses**. Example: `son(abel, adam)`, `less(X, plus(X, one))`
- **Assertions**. Example: `son(X, Y) :- male(X), child(X, Y).`
- **Queries**. Example: `?- child(X, vader).`

Program example:

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
