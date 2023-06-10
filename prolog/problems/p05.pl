% Reverse a list

% reversed(L, R) :- R is the reversal of L

reversed(L1, L2) :- my_rev(L1, L2, []).

my_rev([], L2, L2).

my_rev([X|Xs], L2, Acc) :- my_rev(Xs, L2, [X|Acc]).