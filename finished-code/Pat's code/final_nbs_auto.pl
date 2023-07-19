:- use_module(library(clpfd)).
:- use_module(library(statistics)).

%[X,Y] ins 10..20, labeling([max(X),min(Y)],[X,Y]).

%%%run like this
%starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is max(Size1,Size2), c_star(W),V is ceiling(W/2), writeln("V"),writeln(V),writeln("Size"),writeln(Size),Q is ceiling(Size/2),Start is floor(W/2),X in Start..Q, labeling([min(X)],[X]),writeln("closed-size"),P is X*2,writeln(P),setof(List,ordering(Y,Z,[],P,1,W,[],Found,Old,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).
%starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is max(Size1,Size2), c_star(W),V is ceiling(W/2), writeln("V"),writeln(V),writeln("Size"),writeln(Size),Q is ceiling(Size/2),X in 1..Q, labeling([min(X)],[X]),writeln("closed-size"),P is X*2,writeln(P),setof(List,ordering(Y,Z,[],P,1,W,[],Found,Old,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).
%starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is max(Size1,Size2), c_star(W),V is W/2, X in V..Size, labeling([min(X)],[X]),writeln("X"),writeln(X),setof(List,ordering(Y,Z,[],X,1,W,[],Found,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).
%%% starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is max(Size1,Size2), X in 1..Size, labeling([min(X)],[X]),c_star(Z),writeln("X"),writeln(X),setof(List,ordering(Y,[],X,1,Z,[],Found,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).

nbs :- (set_prolog_flag(stack_limit, 3_147_483_648),starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2),Size is min(Size1,Size2), c_star(W),Start is floor(W/2),write('\"c_star\":'),write(W),write(','),write('\"start\":'),write(Start),write(','),write('\"size\":'),Real_Size is Size *2, write(Real_Size),write(','),Q is ceiling(Size/2),write('\"output\": {'),X in Start..Size, labeling([min(X)],[X]),P is X*4,write('\"'),RealP is X*2,write(RealP),write('\"'),nl,write(':'),setof(List,ordering([Y,Z,[],P,1,W,[],Found,Old,List]),Set),write('{\"length\":'),length(Set,Num),write(Num),write(',\"set\":'),write_term(Set,[quoted(true)]),write('},'),fail).

%% two testing methods from Mike
test_bag(Bag) :-
   bagof(New_Return,
         (X=5,
          starting_openlistF(Y),
	  starting_openlistB(W),
          c_star(Z),
          ordering([Y,W,[],X,1,Z,[],Found,List,New_Return])
         ),
         Bag),writeln(Bag),length(Bag,Size),writeln(Size).

test_set(Set) :-
   setof(New_Return,
         (X=4,
          starting_openlist(Y),
          c_star(Z),
          ordering([Y,[],X,1,Z,[],Found,List,New_Return])
         ),
         Set),writeln(Set),length(Set,Size),writeln(Size).

    

% this ordering clause tells us we have raise C past C_star - this should never occur.

ordering([OpenF,OpenB,CLosed,Size,C,C_star,List,Found,Return,New_Return]) :-
    C_star == 0.

ordering([OpenF,OpenB,CLosed,Size,C,C_star,List,Found,Return,New_Return]) :-
    not(C_star == 0),C > C_star, writeln("bug"),trace,writeln(C),fail.


% this ordering clause tells us that we have suceed in
% - (1) putting "Size" nodes in the closed list,
% - (2) have Found at solution, and
% - (3) C is a C_star

ordering([OpenF,OpenB,CLosed,Size,C,C_star,List,Found,Return,New_Return]) :-  %right now recurses until it runs out of nodes and then raises C level
    not(C_star == 0),
    length(List,Size),  %%might remove when use labeling - !!!!! this is the thing that makes it add states when a "reasonable algorithm" would not %%need this!
    Found == true,
    C == C_star,New_Return=Return.


% this ordering clause is telling us
% - (1) C is less than C_star
%   (2) there are no more nodes left of expand at this C level
%   so we increase C by 1

ordering([OpenF,OpenB,Closed,Size,C,C_star,List,Found,Return,New_Return]) :-
    not(C_star == 0),
    C < C_star,%writeln("WWWWW"),
    not(node_left(OpenF,OpenB,List,C)),%writeln("XXXXX"),
    New_C is C + 1,
    ordering([OpenF,OpenB,Closed,Size,New_C,C_star,List,Found,Return,New_Return]).

% this ordering clause is telling us
% - that the List has list than Size number of nodes, so we can add more
%  - add_state adds another node and recurses.

ordering([OpenF,OpenB,Closed,Size,C,C_star,List,Found,Return,New_Return]) :-
    not(C_star == 0),
    not(length(List,Size)),
    add_states(OpenF,OpenB,Closed,New_OpenF,New_OpenB,New_Closed,List,New_list,C,Found,C_star,Return,New_Return_List,Size),
      ordering([New_OpenF,New_OpenB,New_Closed,Size,C,C_star,New_list,Found,New_Return_List,New_Return]).



% add_state removes a node from the open list and adds it to the closed list.
% - this can only happen if the solution is not found OR C < C_star
% - !!!!!!what if both are true????
% - Then it checks to see if a solution has been found or not.

add_states(OpenF,OpenB,Closed,New_OpenF,New_OpenB,New_Closed,List,New_list,C,Found,C_star,Return,New_Return,Size) :-
%    ((List == [bh, fb, bi, fa],Size == 14,writeln(C),write(List),writeln("Size"),writeln(Size),trace(ordering));true),
%    (not(solution_found(Node,Another,Closed,C_star))
%    ; (solution_found(Node,Another,Closed,C_star), C < C_star)),                           %%%fixing this took us from 80 to 18
%    writeln("in1"),write("C"),writeln(C),writeln("C_star"),writeln(C_star),writeln("Found"),writeln(Found),
    ((Found == true, C < C_star) ; (not(Found == true))),
%    writeln("out3"),
%    ((List == [be, fj3, bf, fj2, bg, fj1, bk1, fc, bh, fb, bi, fa],writeln("C"),writeln(C),writeln("C_star"),writeln(C_star),write(List),writeln("Size"),writeln(Size),trace);true),
    find_nodes(OpenF,OpenB,Closed,Node1,Node2,C,New_G1,New_G2),
   %     writeln("in38"),
  %  write("nodes"),write(Node1),write(Node2),write(List),
    New_list1 = [Node1|List],
    New_list1a = [C|New_list1],
    New_list1b = [Node2|New_list1a],
    New_list = [C|New_list1b],%Succeed = True, %%%
 %      writeln("in39"),writeln(New_list),
    delete(OpenF,Node1,New_OpenF),
    delete(OpenB,Node2,New_OpenB),
%       writeln("in40"),writeln(New_list),
    New_Closed1 = [[Node1,New_G1]|Closed],    %
    New_Closed = [[Node2,New_G2]|New_Closed1], 
%        writeln("new_closed"),writeln(New_Closed),writeln("open"),write(New_OpenF),write(New_OpenB),writeln("list"),write(New_list),
	((solution_found(Node1,Node2,New_Closed,C_star),%writeln("in50"),writeln(Found),writeln(Return),writeln(New_list),
	  Found=true,New_Return=New_list)%,write("59")) %%%%must fix solution_found nodes are bd,fj4
    ; not(solution_found(Node1,Node2,New_Closed,C_star))).%,write("in51")),writeln("out60"). %%%at least one problem here!!!!    %%%not sure this helped at all????


% node_left returns true if there is at least one pair of nodes in the Open list that are in a lower bound pair at C or less than C
% - this tells us we cannot raise C

node_left(OpenF,OpenB,List,C) :- %writeln("start"),write(OpenF),write(OpenB),
    member(Node1,OpenF),member(Node2,OpenB),
    % writeln("problem"),write(Node1),write(Node2),write(C),
     lower_bound_pair(Node1,Node2,Test_C),%writeln("Test_C"),write(Test_C),
%    ;
%    (member(Node1,OpenB),member(Node2,OpenF),lower_bound_pair(Node2,Node1,Test_C))),
   Test_C =< C.%,writeln("nodes"),writeln(Node1),writeln(Node2),writeln(Test_C),writeln("and"),writeln(C).
    

% find_node returns true and the bound variable Node if there is a lower_bound_pair at the current C level containing Node and any other node from the open list.
% - it returns the New_G value that will go in the closed list with Node.

find_nodes(OpenF,OpenB,Closed,NodeF,NodeB,C,New_GF,New_GB) :-
     member(NodeF,OpenF),%writeln("z"),write(NodeF),
     member(NodeB,OpenB),%writeln("zee"),write(NodeB),
    lower_bound_pair(NodeF,NodeB,X),%writeln("done"),write(X),
%    ;
%    lower_bound_pair(Other,Node,X)),    %%need this get ~ 59 answers
%    member(Other,Open),
     X =:= C,
%     (Node = NodeF
%      ;
 %     Node = NodeB)
    ((parent(Parent1,NodeF,Cost1),member([Parent1,GF],Closed), New_GF is GF + Cost1)
    ;
     (special_nodes(Special_list),member(NodeF, Special_list),New_GF = 0)),
    ((parent(Parent2,NodeB,Cost2),member([Parent2,GB],Closed),New_GB is GB + Cost2)
    ;
     (special_nodes(Special_list),member(NodeB, Special_list),New_GB = 0)).%,writeln("finish"),write(New_GB),write(New_GF).
    

% solution_found should return true if there is a solution path "that Node participates" in in the Closed list
% - if there is another solution in the closed list it might return false

solution_found(Node1,Node2,Closed,C_star):-  %%%%stopped here do each seperately and then try to do both...should be enough
    %    writeln("solution"),write(Node1),write(Node2),write(Closed),
%    ((Closed == [[bd, 5], [fj4, 1], [be, 4], [fj3, 1], [bf, 3], [fj2, 1], [bg, 2], [fj1, 1], [bk1,1], [fc,2], [bh,1], [fb,1], [bi,0], [fa,0]], trace);true),
    ((	parent(Node1,Child1,Cost),
	member([Node1,G2],Closed),
	(opposite(Child1,Other_Child1)
	;  opposite(Other_Child1,Child1)),  %%need this get ~ 120 answers
    ((parent(Other_Node1,Other_Child1,Cost2),
      member([Other_Node1,G1],Closed),
    Test is G1 + G2 + Cost + Cost2,%writeln("Test"),writeln(Test),
    Test == C_star)
     %writeln("one"),writeln(Child))
    ;
    (special_nodes(Special_list),member(Other_Child1,Special_list), Test2 is G2 + Cost,Test2 == C_star )
    ;
    (special_nodes(Special_list),member(Child1,Special_list), Test2 is G2 + Cost,Test2 == C_star )
    ))
    ;
 (	parent(Node2,Child2,Cost3),
	member([Node2,G3],Closed),
	(opposite(Child2,Other_Child2)
	;  opposite(Other_Child2,Child2)),  %%need this get ~ 120 answers
    ((parent(Other_Node2,Other_Child2,Cost4),
    member([Other_Node2,G4],Closed),
    Test3 is G3 + G4 + Cost3 + Cost4,
    Test3 == C_star)
     %writeln("one"),writeln(Child))
    ;
    (special_nodes(Special_list),member(Other_Child2,Special_list), Test4 is G3 + Cost3,Test4 == C_star )
    ;
    (special_nodes(Special_list),member(Child1,Special_list), Test4 is G3 + Cost3,Test4 == C_star )
 ))
    ;
( parent(Node1,Child3,Cost5),
  parent(Node2,Child3,Cost6),
  member([Node1,G5],Closed),
  member([Node2,G6],Closed),
  Test5 is G5 + G6 + Cost5 + Cost6,
  Test5 == C_star)).
  
