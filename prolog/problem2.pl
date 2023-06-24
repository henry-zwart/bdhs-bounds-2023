:- use_module(library(reif)).
:- use_module(library(clpz)).

dir(fw).
dir(bw).

opp_dir(fw, bw).
opp_dir(bw, fw).

st_(1).
st_(2).
st_(3).

terminal(st(1,fw)).
terminal(st(3,bw)).
terminal(1,fw).
terminal(3,bw).

solution_cost(2).
solution_cost_plus_one(X) :- solution_cost(X0), X #= X0 + 1.

% Each state exists in two directions
% st(X,D) :- dir(D), st_(X).
st(X,D) :- opp_dir(D,OD), terminal(Y,OD), if_(dif(X,Y), st_(X), false).

parent_(1,2,fw).
parent_(2,3,fw).

p(st(S1,fw),st(S2,fw)) :- parent_(S1,S2,fw).
p(st(S1,bw),st(S2,bw)) :- parent_(S2,S1,fw).

lb_(1,3,1,fw).
lb_(1,2,2,fw).
lb_(2,3,2,fw).
lb(st(X,fw),st(Y,bw),LB) :- lb_(X,Y,LB,fw).
lb(st(X,bw),st(Y,fw),LB) :- lb_(Y,X,LB,fw).

lb(st(X,fw),st(Y,bw),LB) :- 
    st(X,fw), st(Y,bw),
    \+(lb_(X,Y,LB,fw)),
    solution_cost(CStar),
    LB #= CStar + 1.

lb(st(X,bw),st(Y,fw),LB) :- 
    st(X,bw), st(Y,fw),
    \+(lb_(Y,X,LB,fw)),
    solution_cost(CStar),
    LB #= CStar + 1.
