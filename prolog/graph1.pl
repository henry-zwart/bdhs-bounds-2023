:- use_module(library(lists)).

% States
s_(a).
s_(b).
s_(c).
s_(d).
s_(e).
s_(f).

s(S-0) :- s_(S).
s(S-1) :- s_(S).


% Successors
forward_parents([
    [a,b],
    [a,c], 
    [b,c],
    [c,d],
    [d,e],
    [d,f],
    [e,f]
]).
backward_parents([
    [f,e],
    [f,d],
    [e,d],
    [d,c],
    [c,b],
    [c,a],
    [b,a]
]).

parent(s(X0-0), s(Y0-0)) :-
    forward_parents(Z),
    member([X0,Y0], Z).

parent(s(X0-1), s(Y0-1)) :-
    backward_parents(Z),
    member([X0,Y0], Z).

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

lb(s(X-0),s(Y-1),Z) :- forward_lb(X,Y,Z).
lb(s(X-1),s(Y-0),Z) :- forward_lb(Y,X,Z).

lbs(S, LBs) :-
    setof(Z, T^lb(S,T,Z), LBs).
