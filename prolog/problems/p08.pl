% Eliminate consecutive duplicates of list elements.

% compress(Xs,Ys) :- Ys is Xs after consecutive elements have been deduplicated
compress([X], [X]).

compress([X0|[X1|Xs]], Ys) :-
    dif(X0, X1),
    compress([X1|Xs], Y1s),
    append([X0], Y1s, Ys).

compress([X0|[X0|Xs]], Y1s) :-
    compress([X0|Xs], Y1s).