:- use_module(library(reif)).

dir(fw).
dir(bw).

opp_dir(fw, bw).
opp_dir(bw, fw).

st_(1).
st_(2).
st_(3).
st_(4).

terminal(st(1,fw)).
terminal(st(4,bw)).
terminal(1,fw).
terminal(4,bw).

solution_cost(2).

% Each state exists in two directions
st(X,D) :- dir(D), st_(X).
% st(X,D) :- opp_dir(D,OD), terminal(Y,OD), if_(dif(X,Y), st_(X), false).

parent_(1,2,fw).
parent_(1,3,fw).
parent_(2,4,fw).
parent_(3,4,fw).

p(st(S1,fw),st(S2,fw)) :- parent_(S1,S2,fw).
p(st(S1,bw),st(S2,bw)) :- parent_(S2,S1,fw).

lb_(1,4,1,fw).
lb_(1,3,2,fw).
lb_(1,2,2,fw).
lb_(2,4,2,fw).
lb_(3,4,2,fw).
lb(st(X,fw),st(Y,bw),LB) :- lb_(X,Y,LB,fw).
lb(st(X,bw),st(Y,fw),LB) :- lb_(Y,X,LB,fw).
lb(st(_,D),st(_,OD),LB) :- opp_dir(D,OD), solution_cost(Cstar), LB #= Cstar + 1.
% lb(st(_,D),st(_,OD),LB) :- opp_dir(D,OD), solution_cost(Cstar), LB #> Cstar.
