% Finds the second to last element of a list

% last_but_one(X,L) :- X is the second-to-last element of the list L

last_but_one(X,[X, _]).

last_but_one(X,[_, T|Ts]) :- last_but_one(X, [T|Ts]).