parent(a,b,forward).
parent(a,c,forward).
parent(b,c,forward).
parent(c,d,forward).
parent(c,x,forward).
parent(d,e,forward).
parent(d,f,forward).
parent(e,f,forward).

parent(f,e,backward).
parent(f,d,backward).
parent(e,d,backward).
parent(d,c,backward).
parent(d,y,backward).
parent(c,b,backward).
parent(c,a,backward).
parent(b,a,backward).

descendant(X,Y,D) :- parent(Y,X,D).

descendant(X,Y,D) :- parent(Y,Z,D), descendant(X,Z,D).
