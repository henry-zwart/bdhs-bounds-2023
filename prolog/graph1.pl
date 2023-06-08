% States
s(a).
s(b).
s(c).
s(d).
s(e).
s(f).

% Successors
parent(a,b,forward).
parent(a,c,forward).
parent(b,c,forward).
parent(c,d,forward).
parent(d,e,forward).
parent(d,f,forward).
parent(e,f,forward).

parent(f,e,backward).
parent(f,d,backward).
parent(e,d,backward).
parent(d,c,backward).
parent(c,b,backward).
parent(c,a,backward).
parent(b,a,backward).

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

lb(X,Y,Z) :- forward_lb(X,Y,Z).
lb(X,Y,Z) :- forward_lb(Y,X,Z).


descendant(X,Y,D) :- parent(Y,X,D).

descendant(X,Y,D) :- parent(Y,Z,D), descendant(X,Z,D).
