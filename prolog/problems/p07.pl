% flatten a nested list structure
% e.g. [a, [b, [c, d], e]] -> [a,b,c,d,e]

% flattened(X, Y) :- Y is a flattened version of X.


flattened([], []).

flattened(X, [X]) :-
    \+ is_list(X).

flattened([X|Xs], Zs) :-
    flattened(X, Y),
    flattened(Xs, Ys),
    append(Y,Ys,Zs).