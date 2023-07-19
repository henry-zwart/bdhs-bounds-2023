:- use_module(library(clpfd)).
:- use_module(library(statistics)).

%[X,Y] ins 10..20, labeling([max(X),min(Y)],[X,Y]).


%%%run like this
%%%starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is Size1 + Size2, c_star(W),Start is W-1,writeln("Size"),writeln(Size),X in Start..Size, labeling([min(X)],[X]),writeln("closed-size"),writeln(X),setof(List,ordering(Y,Z,[],X,1,W,[],Found,Old,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).
%%% starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2), Size is max(Size1,Size2), X in 1..Size, labeling([min(X)],[X]),c_star(Z),writeln("X"),writeln(X),setof(List,ordering(Y,[],X,1,Z,[],Found,List),Set),writeln("Setz"),writeln(Set),writeln("Length"),length(Set,Num),writeln(Num).

bdhs :- (set_prolog_flag(stack_limit, 3_147_483_648),starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2),Size is Size1 + Size2-1, c_star(W),Start is max(W,0),write('\"c_star\":'),write(W),write(','),write('\"start\":'),write(Start),write(','),write('\"size\":'),write(Size),write(','),write('\"output\": {'),X in Start..Size, labeling([min(X)],[X]), P is X*2,write('\"'),write(X),write('\"'),nl,write(':'),nl,setof(List,ordering([Y,Z,[],P,1,W,[],Found,Old,List]),Set),write('{\"length\":'),length(Set,Num),write(Num),write(',\"set\":'),write_term(Set,[quoted(true)]),write('},'),fail).

%nbs :- (set_prolog_flag(stack_limit, 3_147_483_648),starting_openlistF(Y),starting_openlistB(Z),length(Y,Size1),length(Z,Size2),Size is min(Size1,Size2), c_star(W),Start is floor(W/2),write('\"c_star\":'),write(W),write(','),write('\"start\":'),write(Start),write(','),write('\"size\":'),write(Size),write(','),Q is ceiling(Size/2),write('\"output\": {'),X in Start..Size, labeling([min(X)],[X]),P is X*4,write('\"'),RealP is X*2,write(RealP),write('\"'),nl,write(':'),setof(List,ordering([Y,Z,[],P,1,W,[],Found,Old,List]),Set),write('{\"length\":'),length(Set,Num),write(Num),write(',\"set\":'),write_term(Set,[quoted(true)]),write('}'),fail).

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
          ordering([Y,[],X,1,Z,[],Found,New_Return])
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
    %   ((List == [bd, be, fb, fa], trace) ; true),
    %    writeln("size"),writeln(Size),
    not(C_star == 0),
    length(List,Size),  %%might remove when use labeling - !!!!! this is the thing that makes it add states when a "reasonable algorithm" would not %%need this!
    Found == true,
    C == C_star,New_Return=Return. %,write("clause1"),writeln("nodes"),write(List),writeln("c"),writeln(C).


% this ordering clause is telling us
% - (1) C is less than C_star
%   (2) there are no more nodes left of expand at this C level
%   so we increase C by 1

ordering([OpenF,OpenB,Closed,Size,C,C_star,List,Found,Return,New_Return]) :-
%    Found == true,
    C < C_star,
    not(C_star == 0),
    not(node_left(OpenF,OpenB,List,C)),
    % write("clause2"),write(List),write(C),
%        prolog_current_frame(Frame),
%    format("~w~n", [Frame]),
    New_C is C + 1,
    ordering([OpenF,OpenB,Closed,Size,New_C,C_star,List,Found,Return,New_Return]).

% this ordering clause is telling us
% - that the List has less than Size number of nodes, so we can add more
%  - add_state adds another node and recurses.

ordering([OpenF,OpenB,Closed,Size,C,C_star,List,Found,Return,New_Return]) :-
    %    writeln("found"),writeln(Found),
    not(C_star == 0),
    not(length(List,Size)),%writeln("clause3"),write(List),write(C),
    add_state(OpenF,OpenB,Closed,New_OpenF,New_OpenB,New_Closed,List,New_list,C,Found,C_star,Return,New_Return_List),%write("here2"),write(New_list),
%        prolog_current_frame(Frame),
%    format("~w~n", [Frame]),
      ordering([New_OpenF,New_OpenB,New_Closed,Size,C,C_star,New_list,Found,New_Return_List,New_Return]).                   %i think this fails because of then and then goes to the next line and increases C!!!
%    ; (C < C_star, not(node_left(OpenF,OpenB,List,C)), New_C is C + 1,  %%add a new helping function here to make sure do not raise C unless there is nothing left
%       ordering([OpenF,OpenB,Closed,Size,New_C,C_star,List,Found]))).



% add_state removes a node from the open list and adds it to the closed list.
% - this can only happen if the solution is not found OR C < C_star
% - !!!!!!what if both are true????
% - Then it checks to see if a solution has been found or not.

add_state(OpenF,OpenB,Closed,New_OpenF,New_OpenB,New_Closed,List,New_list,C,Found,C_star,Return,New_Return) :-
  
 %   ((not(solution_found(Node,Closed,C_star)),writeln("troll"),writeln(Found),writeln(Node))
 %   ; (writeln("found1"),write(Found),solution_found(Node,Closed,C_star), write("molly"),writeln(C),writeln(Found),C < C_star)),
   ((Found == true, C < C_star) ; not(Found == true)),
    %%%fixing this took us from 80 to 18  %%%ODD node not bound
             %%different answer should it be 4 or 6???
    %    write("here3"),
%    (Found == true; (not(Found == true),C < C_star)),
    %    write("here4"),
      find_node(OpenF,OpenB,Closed,Node,Direct,C,New_G),
    New_lista = [Node|List], %Succeed = True, %%%
    New_list = [C|New_lista],
    ((Direct =:= "F",delete(OpenF,Node,New_OpenF),New_OpenB = OpenB)
    ;
     ((Direct =:= "B"),delete(OpenB,Node,New_OpenB),New_OpenF = OpenF)),
    New_Closed = [[Node,New_G]|Closed],    %
%    writeln("new_closed"),writeln(New_Closed),
    ((solution_found(Node,New_Closed,C_star),Found=true,New_Return=New_list)
    ; not(solution_found(Node,New_Closed,C_star))). %%%at least one problem here!!!!    %%%not sure this helped at all????


% node_left returns true if there is at least one pair of nodes in the Open list that are in a lower bound pair at C or less than C
% - this tells us we cannot raise C

node_left(OpenF,OpenB,List,C) :-
    member(Node1,OpenF),member(Node2,OpenB),
     lower_bound_pair(Node1,Node2,Test_C),
%    ;
%    (member(Node1,OpenB),member(Node2,OpenF),lower_bound_pair(Node2,Node1,Test_C))),
   Test_C =< C.%,writeln("nodes"),writeln(Node1),writeln(Node2),writeln(Test_C),writeln("and"),writeln(C).
    

% find_node returns true and the bound variable Node if there is a lower_bound_pair at the current C level containing Node and any other node from the open list.
% - it returns the New_G value that will go in the closed list with Node.

find_node(OpenF,OpenB,Closed,Node,Direct,C,New_G) :-
     member(NodeF,OpenF),
     member(NodeB,OpenB),
    lower_bound_pair(NodeF,NodeB,X),
%    ;
%    lower_bound_pair(Other,Node,X)),    %%need this get ~ 59 answers
%    member(Other,Open),
     X =:= C, %write("pair-used"),write(NodeF),write(NodeB),
     ((Node = NodeF,Direct = "F")
      ;
      (Node = NodeB, Direct = "B")),
    ((parent(Parent,Node,Cost), member([Parent,G],Closed), New_G is G + Cost)
    ;
     (special_nodes(Special_list),member(Node, Special_list),New_G = 0)).
    

% solution_found should return true if there is a solution path "that Node participates" in in the Closed list
% - if there is another solution in the closed list it might return false

solution_found(Node,Closed,C_star):-
	parent(Node,Child,Cost),
	member([Node,G2],Closed),
	(opposite(Child,Other_Child)
	;  opposite(Other_Child,Child)),  %%need this get ~ 120 answers
    ((parent(Other_Node,Other_Child,Cost2),
    member([Other_Node,G1],Closed),
    Test is G1 + G2 + Cost + Cost2,
    Test == C_star)
     %writeln("one"),writeln(Child))
    ;
    (special_nodes(Special_list),member(Other_Child,Special_list), Test2 is G2 + Cost,Test2 == C_star )
    ;
    (special_nodes(Special_list),member(Child,Special_list), Test2 is G2 + Cost,Test2 == C_star )
     ).
