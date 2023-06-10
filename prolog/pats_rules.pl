% state
state(a).
state(b).
state(c).
state(d).
state(e).
state(f).

% termini
terminal(a).
terminal(f).

% Successors
parent(a,b,fw).
parent(a,c,fw).
parent(b,c,fw).
parent(c,d,fw).
parent(d,e,fw).
parent(d,f,fw).
parent(e,f,fw).

parent(f,e,bw).
parent(f,d,bw).
parent(e,d,bw).
parent(d,c,bw).
parent(c,b,bw).
parent(c,a,bw).
parent(b,a,bw).

% Lower bound graph
forward_lb(a,f,1).
forward_lb(a,e,2).
forward_lb(a,d,2).
forward_lb(a,c,3).
forward_lb(a,b,4).
forward_lb(b,f,2).
forward_lb(b,e,5).
forward_lb(b,d,4).
forward_lb(b,c,4).
forward_lb(c,f,2).
forward_lb(c,e,4).
forward_lb(c,d,3).
forward_lb(d,f,3).
forward_lb(d,e,4).
forward_lb(e,f,4).



solution_cost(3).

lb(X,Y,Z) :- forward_lb(X,Y,Z).
lb(X,Y,Z) :- forward_lb(Y,X,Z).


ordering(Order) :-
    Order = [turn(a, Ao), turn(b, Bo), turn(c, Co, turn(d, Do), turn(e, Eo), turn(f, Fo)],
    