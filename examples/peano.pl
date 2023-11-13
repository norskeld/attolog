add(0, N, N).
add(s(N), M, s(R)) :- add(N, M, R).

% Queries. These should be used in the REPL after loading this file.

% ?- add(s(0), s(0), X).
% X = s(s(0))

% ?- add(s(0), X, s(s(0))).
% X = s(0)
