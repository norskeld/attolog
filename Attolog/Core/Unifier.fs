module Attolog.Core.Unifier

open Attolog.Core.Syntax

/// Raised when terms cannot be unified.
exception NoUnify

/// Tries to unify terms `t1` and `t2` in the current environment `env`. On success it returns the environment extended
/// with the result of unification.
///
/// It raises `NoUnify` on failure.
let rec unifyTerms (env: Env) (t1: Term) (t2: Term) : Env =
  match (substitute env t1, substitute env t2) with
  | t1, t2 when t1 = t2 -> env
  | Var(y), t
  | t, Var(y) ->
    if occurs y t then
      raise NoUnify
    else
      { values = (y, t) :: env.values }
  | Const(_), _ -> raise NoUnify
  | Int(_), _ -> raise NoUnify
  | App(c1, ts1), App(c2, ts2) when c1 = c2 -> unifyLists env ts1 ts2
  | App(_), _ -> raise NoUnify

/// Unifies two lists of terms in current environment `env` and returns a new environment on success.
///
/// It raises `NoUnify` on failure or if the lists differ in length.
and unifyLists (env: Env) (ts1: list<Term>) (ts2: list<Term>) : Env =
  try
    List.fold2 unifyTerms env ts1 ts2
  with _ ->
    raise NoUnify

/// Unifies atomic propositions in current environment `env` and returns a new environment on success.
///
/// It raises `NoUnify` on failure.
and unifyAtoms (env: Env) (c1, ts1) (c2, ts2) : Env =
  if c1 = c2 then unifyLists env ts1 ts2 else raise NoUnify
