module Attolog.Core.Solver

open System

open Attolog.Prelude
open Attolog.Core.Syntax

/// Raised when a goal can't be proved.
exception NoSolution

/// Represents a choice point in the proof search at which we may continue searching for another solution. It is a
/// tuple `(asrl, env, clause, n)` where `asrl` is the assertion list used to search for other solutions of clause
/// `clause` in environment `env`, and `n` is the search depth.
type Choice = Database * Env * Clause * int

/// The global database of assertions.
let private db: ref<Database> = ref []

/// Add a new assertion at the end of the current database.
let private assertz (a: Assertion) =
  let rec add =
    function
    | [] -> [ a ]
    | b :: bs -> b :: add bs

  db := add !db

/// Renumbers all variable instances occurring in term `term` so that they have level `n`.
let rec private renumberTerm (n: int) (term: Term) : Term =
  match term with
  | Var(var, _) -> Var(var, n)
  | Const(_) as term -> term
  | App(constant, terms) -> App(constant, List.map (renumberTerm n) terms)

/// Renumbers all variable instances occurring in atom so that they have level `n`.
let private renumberAtom (n: int) ((constant, terms): Atom) : Atom =
  (constant, List.map (renumberTerm n) terms)

/// Prints the solution of a query (goal) encoded in `env`. It then gives the user the option to search for other
/// solutions, as described by the list of choice points `choices`, or to abort the currect search.
///
/// TODO: This should be refactored to avoid effects.
let rec private printSolution (choices: list<Choice>) (env: Env) =
  let isPositiveReply (reply: string) : bool =
    List.contains reply [ "yes"; "y"; "Yes"; "YES"; "" ]

  match (Env.toString env, choices) with
  | ("Yes", _) -> printfn "Yes."
  | (answer, []) -> printfn "%s" answer
  | (answer, choice) ->
    printf "%s. Show more?" answer

    match Console.ReadLine() with
    | reply when isPositiveReply reply -> continueSearch choice
    | _ -> raise NoSolution

/// Looks for other answers. It accepts a list of choices. It continues the search at the first choice in the list.
and private continueSearch (choices: list<Choice>) =
  match choices with
  | [] -> raise NoSolution
  | (asrl, env, clauses, n) :: choices -> findSolution choices asrl env clauses n

/// Looks for the proof of clause `clause`.
///
/// - `choices` is a list of choices we may look for other solutions.
/// - `asrl` is the list of assertions the are used to reduce `clause` to subgoals.
/// - `env` is the current environment (values of variables).
/// - `n` is the search depth which increases at each level of search.
///
/// When a solution is found, it is printed. The user then decides whether other solutions should be searched for.
and private findSolution
  (choices: list<Choice>)
  (asrl: Database)
  (env: Env)
  (clause: Clause)
  (n: int)
  =
  /// Reduces atom `atom` to subgoals by using the first assertion in the assertion list `asrl` whose conclusions
  /// matches `atom`.
  ///
  /// Returns `None` if the atom can't be reduced.
  let rec reduceAtom (atom: Atom) (asrl: Database) =
    match asrl with
    | [] -> None
    | (atom', lst) :: asrl' ->
      try
        let env' = Unifier.unifyAtoms env atom (renumberAtom n atom')
        Some(asrl', env', List.map (renumberAtom n) lst)
      with Unifier.NoUnify ->
        reduceAtom atom asrl'

  match clause with
  | [] ->
    // All atoms are solved, we found a solution.
    printSolution choices env
  | atom :: clause' ->
    // Reduce the first atom in the clause.
    match reduceAtom atom asrl with
    | None ->
      // This clause can't be solved, look for other solutions.
      continueSearch choices
    | Some(asrl', env', subgoals) ->
      // The atom was reduced to subgoals. Continue search with the subgoals added to the list of goals.
      let choices' = (asrl', env, clause, n) :: choices
      findSolution choices' !db env' (subgoals @ clause') (n + 1)

/// Searches for the proof of clause `clause` using the global database `db`.
///
/// TODO: Avoid effects, produce value(s) instead.
let private findSolutionToplevel (clause: Clause) =
  try
    findSolution [] !db Env.empty clause 1
  with NoSolution ->
    printfn "No."

let solve (command: Command) =
  match command with
  | Query(clause) -> findSolutionToplevel clause
  | Assert(assertion) -> assertz assertion
  | _ -> ()
