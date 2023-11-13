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

% Queries. These should be used in the REPL after loading this file.

% ?- son(X, vader).
% ?- daughter(X, vader).
% ?- grandchild(X, vader).
