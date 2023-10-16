# Attolog

Minimal [Prolog] interpreter.

## Description

This is mostly a learning project to know more about **F#** and **Prolog**.

The implementation is rather basic and therefore limited. It **does not** contain cuts, arithmetic, lists, equality and so on — only basic [Horn clauses][horn-clause].

## Language

The language currently consists of the following terms:

- **Constants**.
  - Start with a lower case letter followed by alphanumeric characters or `_`.
  - Examples: `x`, `y`, `constant`.
- **Variables**.
  - Start with an upper case letter followed by alphanumeric characters or `_`.
  - Examples: `X`, `Y`, `Variable`.
- **Composite terms**.
  - Have the form `f(t1, ..., tn)` where `f` is a constant and `t1` and `tn` are terms.
  - Examples: `son(abel, adam)`, `less(X, plus(X, one))`.

## License

[MIT](LICENSE).

<!-- Links. -->

[prolog]: https://en.wikipedia.org/wiki/Prolog
[horn-clause]: https://en.wikipedia.org/wiki/Horn_clause
