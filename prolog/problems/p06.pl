% Find out whether a list is a palindrome.

% palindrome(X) :- true if X is a palindrome.

palindrome(X) :- 
    reversed(X,X).


reversed(L1, L2) :- my_rev(L1, L2, []).

my_rev([], L2, L2).

my_rev([X|Xs], L2, Acc) :- my_rev(Xs, L2, [X|Acc]).