:- use_module(library(clpfd)).

%[X,Y] ins 10..20, labeling([max(X),min(Y)],[X,Y]).


%%%run like this
%%% X in 1..6, labeling([min(X)],[X]),starting_openlist(Y),c_star(Z),ordering(Y,[],X,1,Z,[],Found).


ordering(Open,CLosed,Size,C,C_star,List,Found) :-
    C > C_star, writeln("bug"),trace,writeln(C),fail.
    
ordering(Open,CLosed,Size,C,C_star,List,Found) :-  %right now recurses until it runs out of nodes and then raises C level
    length(List,Size),  %%might remove when use labeling - !!!!! this is the thing that makes it add states when a "reasonable algorithm" would not %%need this!
    Found == true,
    C == C_star,writeln("nodes"),write(List).


ordering(Open,Closed,Size,C,C_star,List,Found) :-
    not(length(List,Size)),
    ((add_state(Open,Closed,New_Open,New_Closed,List,New_list,C,Found,C_star),
      ordering(New_Open,New_Closed,Size,C,C_star,New_list,Found))
    ; (C < C_star, New_C is C + 1,
       ordering(Open,Closed,Size,New_C,C_star,List,Found))).


add_state(Open,Closed,New_Open,New_Closed,List,New_list,C,Found,C_star) :-
    (not(solution_found(Node,Closed,C_star))
    ; C < C_star),
    find_node(Open,Closed,Node,C,New_G),
    New_list = [Node|List], %Succeed = True, %%%
    delete(Open,Node,New_Open),
    New_Closed = [[Node,New_G]|Closed],
    ((solution_found(Node,New_Closed,C_star),Found=true)
    ; not(solution_found(Node,New_Closed,C_star))). %%%at least one problem here!!!!    %%%not sure this helped at all????

    

find_node(Open,Closed,Node,C,New_G) :-
    member(Node,Open),
    (lower_bound_pair(Node,Other,X)
    ;
    lower_bound_pair(Other,Node,X)),    %%need this get ~ 59 answers
    member(Other,Open),
    X =:= C,
    ((parent(Parent,Node,Cost), member([Parent,G],Closed), New_G is G + Cost)
    ;
     (special_nodes(Special_list),member(Node, Special_list),New_G = 0)).
    
    
solution_found(Node,Closed,C_star):-
	parent(Node,Child,Cost),
	member([Node,G2],Closed),
	(opposite(Child,Other_Child)
	;  opposite(Other_Child,Child)),  %%need this get ~ 120 answers
    ((parent(Other_Node,Other_Child,Cost2),
    member([Other_Node,G1],Closed),
    Test is G1 + G2 + Cost + Cost2,
    Test == C_star)
    ;
     (special_nodes(Special_list),member(Other_Child,Special_list), Test2 is G2 + Cost,Test2 == C_star )).


%%graph specific%%

starting_openlist([fa,fb,fc,fd,fe,ff,bf,be,bd,bc,bb,ba]).

c_star(3).

special_nodes([fa,bf]).

parent(fa,fb,1).
    parent(fa,fc,1).
    parent(fb,fc,1).
    parent(fc,fd,1).
    parent(fd,fe,1).
    parent(fd,ff,1).
    parent(bf,be,1).
    parent(bf,bd,1).
    parent(be,bd,1).
    parent(bd,bc,1).
    parent(bc,bb,1).
    parent(bc,ba,1).

        lower_bound_pair(fa,bf,1).
lower_bound_pair(fa,be,2).
lower_bound_pair(fa,bd,2).
lower_bound_pair(fa,bc,3).
lower_bound_pair(fb,bf,2).
lower_bound_pair(fb,be,3).
lower_bound_pair(fb,bd,3).
lower_bound_pair(fc,bf,2).
lower_bound_pair(fc,be,3).
lower_bound_pair(fc,bd,3).
    lower_bound_pair(fd,bf,3).

    opposite(fa,ba).
    opposite(fb,bb).
    opposite(fc,bc).
    opposite(fd,bd).
    opposite(fe,be).
    opposite(ff,bf).

    
