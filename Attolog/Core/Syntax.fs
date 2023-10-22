module Attolog.Core.Syntax

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
  /// Compound term `f(t1, ..., tn)`
  | App of Constant * list<Term>

/// Atomic proposition `p(t1, ..., tn)`
type Atom = Constant * list<Term>

/// A conjunction of atomic propositions `p1, ..., pn`. The empty list represents `true`.
type Clause = list<Atom>

/// An assertion `(a, b1, ..., bn)` is a Horn formula `b1 & ... & bn => a`.
type Assertion = Atom * Clause

/// An environment represents the current values of variables.
type Env = { values: list<Variable * Term> }

/// A database is a list of assertions. It represents the current program.
type Database = list<Assertion>

/// Toplevel commands.
type Command =
  /// Assertion `a :- b_1, ..., b_n.` or `a.`.
  | Assert of Assertion
  /// Query `?- a`.
  | Query of Clause

/// Returns the value of variable instance `x` in environment `env`.
let lookup (env: Env) (var: Variable) : Term =
  try
    env.values |> List.find (fst >> ((=) var)) |> snd
  with _ ->
    Var(var)

/// Substitutes in term `t` values for variables, as specified by the `env`. It substitutes repeatedly until terms
/// stops changing, so this is not the usual kind of substitution. It is what we need during unification.
let rec substitute (env: Env) (term: Term) : Term =
  match term with
  | Var(var) as term ->
    let found = lookup env var

    if term = found then found else substitute env found
  | Const(_) as term -> term
  | App(constant, terms) -> App(constant, List.map (substitute env) terms)

/// Checks if variable instance `x` appears in term `term`.
let rec occurs (x: Variable) (term: Term) : bool =
  match term with
  | Var(var) -> var = x
  | Const(_) -> false
  | App(_, terms) -> List.exists (occurs x) terms

type Term with

  /// Returns a string representation of the term.
  static member toString(term: Term) : string =
    match term with
    | Var(var, _) -> var
    | Const(constant) -> constant
    | App(constant, terms) -> constant + " " + String.concat " " (List.map Term.toString terms)

type Env with

  /// Creates an empty environment.
  static member empty: Env = { values = [] }

  /// Returns a string representation of the environment. It only keeps instance variables at level 0, i.e. those that
  /// appear in the toplevel query.
  static member toString(env: Env) : string =
    match List.filter (fun ((_, n), _) -> n = 0) env.values with
    | [] -> "Yes"
    | env' ->
      let items =
        List.map (fun ((x, _), e) -> x + " = " + Term.toString (substitute env e)) (List.rev env')

      String.concat "\n" items
