% Pack consecutive duplicates of list elements into sublists

% pack(X,Y) :- Y is a list of lists of consecutive identical elements in X

pack([], []).

pack([X], [[X]]).

pack([X,X|Xs], [[X|Z]|Zs]) :-
    pack([X|Xs], [Z|Zs]).

pack([X,Y|Ys], [[X]|Zs]) :-
    X \= Y,
    pack([Y|Ys], Zs).