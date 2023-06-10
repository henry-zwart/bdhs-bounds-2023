% Find the number of items in a list

% items_count(L,X) :- X is the number of items in the list L

items_count([], 0).

items_count([_|L], X) :-
    items_count(L, X0),
    X is X0 + 1.