module Attolog.Syntax

/// Constants and atoms start with lower-case letters.
type Constant = string

/// Variables start with upper-case letters, followed by a number which indicates an instance of the variable.
type Variable = string * int

/// The datatype of terms.
type Term =
  /// Variable `X1`, `Y0`, `Z2`, ...
  | Var of Variable
  /// Constant `a`, `b`, `c`, ...
  | Const of Constant
  /// Compound term `f(t_1,...,t_n)`
  | App of Constant * list<Term>

/// Atomic proposition `p(t_1, ..., t_n)`
type Atom = Constant * list<Term>

/// A conjunction of atomic propositions `p_1, ..., p_n`. The empty list represents `true`.
type Clause = list<Atom>

/// An assertion `(a,b_1,...,b_n)` is a Horn formula `b_1 & ... & b_n => a`.
type Assertion = Atom * Clause

/// An environment represents the current values of variables.
type Env = list<Variable * Term>

/// A database is a list of assertions. It represents the current program.
type Database = list<Assertion>

/// Toplevel commands.
type Command =
  /// Assertion `a :- b_1, ..., b_n.` or `a.`.
  | Assert of Assertion
  /// Query `?- a`.
  | Query of Clause
