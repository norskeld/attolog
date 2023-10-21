# Attolog

Minimal [Prolog] interpreter.

## Description

This is mostly a learning project to know more about **F#** and **Prolog**.

The implementation is rather basic, naïve (not following ISO spec) and therefore limited. It **does not** contain cuts, arithmetic, lists, equality and so on — only basic [Horn clauses][horn-clause].

## Language

The language currently consists of:

- **Constants**. Example: `x`, `y`, `constant`
- **Variables**. Example: `X`, `Y`, `Variable`
- **Clauses**. Example: `son(abel, adam)`, `less(X, plus(X, one))`
- **Assertions**. Example: `son(X, Y) :- male(X), child(X, Y).`
- **Queries**. Example: `?- child(X, vader).`

## TODO

- [ ] Get rid of exceptions, embrace errors as values.
- [ ] Refactor solver to produce values instead of effects (direct printing, reading input, etc).

## License

[MIT](LICENSE).

<!-- Links. -->

[prolog]: https://en.wikipedia.org/wiki/Prolog
[horn-clause]: https://en.wikipedia.org/wiki/Horn_clause
