% Find the K'th element of a list

% element_at(K, L, X) :- X is the K'th element in the list L.

element_at(X, [X|_], 0).

element_at(X, [_|Ls], K) :-
    K0 is K - 1,
    element_at(X, Ls, K0).