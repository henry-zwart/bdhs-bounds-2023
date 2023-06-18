:- use_module(library(clpfd)).

%[X,Y] ins 10..20, labeling([max(X),min(Y)],[X,Y]).


%%%run like this
%%% starting_openlist(Y),length(Y,Size),New_Size is Size/2,X in 1..New_Size, labeling([min(X)],[X]),c_star(Z),writeln("X"),writeln(X),ordering(Y,[],X,1,Z,[],Found).

%% two testing methods from Mike
test_bag(Bag) :-
   bagof(List,
         (X=4,
          starting_openlist(Y),
          c_star(Z),
          ordering(Y,[],X,1,Z,[],Found,List)
         ),
         Bag),writeln(Bag),length(Bag,Size),writeln(Size).

test_set(Set) :-
   setof(List,
         (X=4,
          starting_openlist(Y),
          c_star(Z),
          ordering(Y,[],X,1,Z,[],Found,List)
         ),
         Set),writeln(Set),length(Set,Size),writeln(Size).

% this ordering clause tells us we have raise C past C_star - this should never occur.

ordering(Open,CLosed,Size,C,C_star,List,Found,Return) :-
    C > C_star, writeln("bug"),trace,writeln(C),fail.


% this ordering clause tells us that we have suceed in
% - (1) putting "Size" nodes in the closed list,
% - (2) have Found at solution, and
% - (3) C is a C_star

ordering(Open,CLosed,Size,C,C_star,List,Found,Return) :-  %right now recurses until it runs out of nodes and then raises C level
    %   ((List == [bd, be, fb, fa], trace) ; true),
%    writeln("size"),writeln(Size),
    length(List,Size),  %%might remove when use labeling - !!!!! this is the thing that makes it add states when a "reasonable algorithm" would not %%need this!
    Found == true,
    C == C_star,writeln("nodes"),write(List),writeln("c"),writeln(C).


% this ordering clause is telling us
% - (1) C is less than C_star
%   (2) there are no more nodes left of expand at this C level
%   so we increase C by 1

ordering(Open,Closed,Size,C,C_star,List,Found,Return) :-
%    Found == true,
    C < C_star,
    not(node_left(Open,List,C)),
    New_C is C + 1,
    ordering(Open,Closed,Size,New_C,C_star,List,Found,Return).

% this ordering clause is telling us
% - that the List has list than Size number of nodes, so we can add more
%  - add_state adds another node and recurses.

ordering(Open,Closed,Size,C,C_star,List,Found,Return) :-
%    writeln("found"),writeln(Found),
    not(length(List,Size)),
    add_state(Open,Closed,New_Open,New_Closed,List,New_list,C,Found,C_star,Return),
      ordering(New_Open,New_Closed,Size,C,C_star,New_list,Found,Return).                   %i think this fails because of then and then goes to the next line and increases C!!!
%    ; (C < C_star, not(node_left(Open,List,C)), New_C is C + 1,  %%add a new helping function here to make sure do not raise C unless there is nothing left
%       ordering(Open,Closed,Size,New_C,C_star,List,Found))).



% add_state removes a node from the open list and adds it to the closed list.
% - this can only happen if the solution is not found OR C < C_star
% - !!!!!!what if both are true????
% - Then it checks to see if a solution has been found or not.

add_state(Open,Closed,New_Open,New_Closed,List,New_list,C,Found,C_star,Return) :-
    (not(solution_found(Node,Closed,C_star))
    ; (solution_found(Node,Closed,C_star), C < C_star)),                           %%%fixing this took us from 80 to 18
    find_node(Open,Closed,Node,C,New_G),
    New_list = [Node|List], %Succeed = True, %%%
    delete(Open,Node,New_Open),
    New_Closed = [[Node,New_G]|Closed],    %
%    writeln("new_closed"),writeln(New_Closed),
    ((solution_found(Node,New_Closed,C_star),Found=true,Return=New_list)
    ; not(solution_found(Node,New_Closed,C_star))). %%%at least one problem here!!!!    %%%not sure this helped at all????


% node_left returns true if there is at least one pair of nodes in the Open list that are in a lower bound pair at C or less than C
% - this tells us we cannot raise C

node_left(Open,List,C) :-
    member(Node1,Open),
    member(Node2,Open),
    (lower_bound_pair(Node1,Node2,Test_C)
    ;
    lower_bound_pair(Node2,Node1,Test_C)),
   Test_C =< C.%,writeln("nodes"),writeln(Node1),writeln(Node2),writeln(Test_C),writeln("and"),writeln(C).
    

% find_node returns true and the bound variable Node if there is a lower_bound_pair at the current C level containing Node and any other node from the open list.
% - it returns the New_G value that will go in the closed list with Node.

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


%%graph specific%%


%graph 1
%/*
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
lower_bound_pair(fe,bf,4).


    opposite(fa,ba).
    opposite(fb,bb).
    opposite(fc,bc).
    opposite(fd,bd).
    opposite(fe,be).
opposite(ff,bf).
%*/
%%graph 2
/*
starting_openlist([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj1,fj2,fj3,fj4,fj5,fj6,fj7,fk1,fk2,fk3,fk4,fk5,fk6,fk7,fk8,bj1,bj2,bj3,bj4,bj5,bj6,bj7,bk1,bk2,bk3,bk4,bk5,bk6,bk7,bk8,bi,bh,bg,bf,be,bd,bc,bb,ba]).

c_star(8).

special_nodes([fa,bi]).

parent(fa,fb,1).
    parent(fb,fc,1).
    parent(fc,fd,1).
    parent(fd,fe,1).
    parent(fe,ff,1).
    parent(ff,fg,1).
    parent(fg,fh,1).
    parent(fh,fi,1).
    parent(fa,fj1,1).
    parent(fa,fj2,1).
    parent(fa,fj3,1).
    parent(fa,fj4,1).
    parent(fa,fj5,1).
    parent(fa,fj6,1).
    parent(fa,fj7,1).
    
    parent(bi,bh,1).
    parent(bh,bg,1).
    parent(bg,bf,1).
    parent(bf,be,1).
    parent(be,bd,1).
    parent(bd,bc,1).
    parent(bc,bb,1).
    parent(bc,ba,1).

    parent(bi,bk1,1).
    parent(bi,bk2,1).
    parent(bi,bk3,1).
    parent(bi,bk4,1).
    parent(bi,bk5,1).
    parent(bi,bk6,1).
    parent(bi,bk7,1).
    parent(bi,bk8,1).

    


lower_bound_pair(fa,bi,1).
lower_bound_pair(fa,bh,2).
lower_bound_pair(fa,bg,3).
lower_bound_pair(fa,bf,4).
lower_bound_pair(fa,be,5).
lower_bound_pair(fa,bd,6).
lower_bound_pair(fa,bc,7).
lower_bound_pair(fa,bb,8).

lower_bound_pair(fa,bk1,2).
lower_bound_pair(fa,bk2,2).
lower_bound_pair(fa,bk3,2).
lower_bound_pair(fa,bk4,2).
lower_bound_pair(fa,bk5,2).
lower_bound_pair(fa,bk6,2).
lower_bound_pair(fa,bk7,2).
lower_bound_pair(fa,bk8,2).

lower_bound_pair(fb,bi,2).
lower_bound_pair(fb,bh,3).
lower_bound_pair(fb,bg,4).
lower_bound_pair(fb,bf,5).
lower_bound_pair(fb,be,6).
lower_bound_pair(fb,bd,7).
lower_bound_pair(fb,bc,8).

lower_bound_pair(fb,bk1,3).
lower_bound_pair(fb,bk2,3).
lower_bound_pair(fb,bk3,3).
lower_bound_pair(fb,bk4,3).
lower_bound_pair(fb,bk5,3).
lower_bound_pair(fb,bk6,3).
lower_bound_pair(fb,bk7,3).
lower_bound_pair(fb,bk8,3).

lower_bound_pair(fc,bi,3).
lower_bound_pair(fc,bh,4).
lower_bound_pair(fc,bg,5).
lower_bound_pair(fc,bf,6).
lower_bound_pair(fc,be,7).
lower_bound_pair(fc,bd,8).


lower_bound_pair(fc,bk1,4).
lower_bound_pair(fc,bk2,4).
lower_bound_pair(fc,bk3,4).
lower_bound_pair(fc,bk4,4).
lower_bound_pair(fc,bk5,4).
lower_bound_pair(fc,bk6,4).
lower_bound_pair(fc,bk7,4).
    lower_bound_pair(fc,bk8,4).

    lower_bound_pair(fd,bi,4).
lower_bound_pair(fd,bh,5).
lower_bound_pair(fd,bg,6).
lower_bound_pair(fd,bf,7).
lower_bound_pair(fd,be,8).


lower_bound_pair(fd,bk1,5).
lower_bound_pair(fd,bk2,5).
lower_bound_pair(fd,bk3,5).
lower_bound_pair(fd,bk4,5).
lower_bound_pair(fd,bk5,5).
lower_bound_pair(fd,bk6,5).
lower_bound_pair(fd,bk7,5).
lower_bound_pair(fd,bk8,5).

lower_bound_pair(fe,bi,5).
lower_bound_pair(fe,bh,6).
lower_bound_pair(fe,bg,7).
lower_bound_pair(fe,bf,8).

lower_bound_pair(fe,bk1,6).
lower_bound_pair(fe,bk2,6).
lower_bound_pair(fe,bk3,6).
lower_bound_pair(fe,bk4,6).
lower_bound_pair(fe,bk5,6).
lower_bound_pair(fe,bk6,6).
lower_bound_pair(fe,bk7,6).
lower_bound_pair(fe,bk8,6).

lower_bound_pair(ff,bi,6).
lower_bound_pair(ff,bh,7).
lower_bound_pair(ff,bg,8).

lower_bound_pair(ff,bk1,7).
lower_bound_pair(ff,bk2,7).
lower_bound_pair(ff,bk3,7).
lower_bound_pair(ff,bk4,7).
lower_bound_pair(ff,bk5,7).
lower_bound_pair(ff,bk6,7).
lower_bound_pair(ff,bk7,7).
lower_bound_pair(ff,bk8,7).

    lower_bound_pair(fg,bi,7).
lower_bound_pair(fg,bh,8).

lower_bound_pair(fg,bk1,8).
lower_bound_pair(fg,bk2,8).
lower_bound_pair(fg,bk3,8).
lower_bound_pair(fg,bk4,8).
lower_bound_pair(fg,bk5,8).
lower_bound_pair(fg,bk6,8).
lower_bound_pair(fg,bk7,8).
lower_bound_pair(fg,bk8,8).

        lower_bound_pair(fh,bi,8).

lower_bound_pair(fg,bk1,9).
lower_bound_pair(fg,bk2,9).
lower_bound_pair(fg,bk3,9).
lower_bound_pair(fg,bk4,9).
lower_bound_pair(fg,bk5,9).
lower_bound_pair(fg,bk6,9).
lower_bound_pair(fg,bk7,9).
lower_bound_pair(fg,bk8,9).

lower_bound_pair(fj1,bi,2).
lower_bound_pair(fj1,bh,3).
lower_bound_pair(fj1,bg,4).
lower_bound_pair(fj1,bf,5).
lower_bound_pair(fj1,be,6).
lower_bound_pair(fj1,bd,7).
lower_bound_pair(fj1,bc,8).

lower_bound_pair(fj1,bk1,8).
lower_bound_pair(fj1,bk2,8).
lower_bound_pair(fj1,bk3,8).
lower_bound_pair(fj1,bk4,8).
lower_bound_pair(fj1,bk5,8).
lower_bound_pair(fj1,bk6,8).
lower_bound_pair(fj1,bk7,8).
    lower_bound_pair(fj1,bk8,8).

    lower_bound_pair(fj2,bi,2).
lower_bound_pair(fj2,bh,3).
lower_bound_pair(fj2,bg,4).
lower_bound_pair(fj2,bf,5).
lower_bound_pair(fj2,be,6).
lower_bound_pair(fj2,bd,7).
lower_bound_pair(fj2,bc,8).

lower_bound_pair(fj2,bk1,8).
lower_bound_pair(fj2,bk2,8).
lower_bound_pair(fj2,bk3,8).
lower_bound_pair(fj2,bk4,8).
lower_bound_pair(fj2,bk5,8).
lower_bound_pair(fj2,bk6,8).
lower_bound_pair(fj2,bk7,8).
    lower_bound_pair(fj2,bk8,8).

    lower_bound_pair(fj3,bi,2).
lower_bound_pair(fj3,bh,3).
lower_bound_pair(fj3,bg,4).
lower_bound_pair(fj3,bf,5).
lower_bound_pair(fj3,be,6).
lower_bound_pair(fj3,bd,7).
lower_bound_pair(fj3,bc,8).

lower_bound_pair(fj3,bk1,8).
lower_bound_pair(fj3,bk2,8).
lower_bound_pair(fj3,bk3,8).
lower_bound_pair(fj3,bk4,8).
lower_bound_pair(fj3,bk5,8).
lower_bound_pair(fj3,bk6,8).
lower_bound_pair(fj3,bk7,8).
    lower_bound_pair(fj3,bk8,8).

    lower_bound_pair(fj4,bi,2).
lower_bound_pair(fj4,bh,3).
lower_bound_pair(fj4,bg,4).
lower_bound_pair(fj4,bf,5).
lower_bound_pair(fj4,be,6).
lower_bound_pair(fj4,bd,7).
lower_bound_pair(fj4,bc,8).

lower_bound_pair(fj4,bk1,8).
lower_bound_pair(fj4,bk2,8).
lower_bound_pair(fj4,bk3,8).
lower_bound_pair(fj4,bk4,8).
lower_bound_pair(fj4,bk5,8).
lower_bound_pair(fj4,bk6,8).
lower_bound_pair(fj4,bk7,8).
    lower_bound_pair(fj4,bk8,8).

    lower_bound_pair(fj5,bi,2).
lower_bound_pair(fj5,bh,3).
lower_bound_pair(fj5,bg,4).
lower_bound_pair(fj5,bf,5).
lower_bound_pair(fj5,be,6).
lower_bound_pair(fj5,bd,7).
lower_bound_pair(fj5,bc,8).

lower_bound_pair(fj5,bk1,8).
lower_bound_pair(fj5,bk2,8).
lower_bound_pair(fj5,bk3,8).
lower_bound_pair(fj5,bk4,8).
lower_bound_pair(fj5,bk5,8).
lower_bound_pair(fj5,bk6,8).
lower_bound_pair(fj5,bk7,8).
    lower_bound_pair(fj5,bk8,8).

    lower_bound_pair(fj6,bi,2).
lower_bound_pair(fj6,bh,3).
lower_bound_pair(fj6,bg,4).
lower_bound_pair(fj6,bf,5).
lower_bound_pair(fj6,be,6).
lower_bound_pair(fj6,bd,7).
lower_bound_pair(fj6,bc,8).

lower_bound_pair(fj6,bk1,8).
lower_bound_pair(fj6,bk2,8).
lower_bound_pair(fj6,bk3,8).
lower_bound_pair(fj6,bk4,8).
lower_bound_pair(fj6,bk5,8).
lower_bound_pair(fj6,bk6,8).
lower_bound_pair(fj6,bk7,8).
    lower_bound_pair(fj6,bk8,8).

    lower_bound_pair(fj7,bi,2).
lower_bound_pair(fj7,bh,3).
lower_bound_pair(fj7,bg,4).
lower_bound_pair(fj7,bf,5).
lower_bound_pair(fj7,be,6).
lower_bound_pair(fj7,bd,7).
lower_bound_pair(fj7,bc,8).

lower_bound_pair(fj7,bk1,8).
lower_bound_pair(fj7,bk2,8).
lower_bound_pair(fj7,bk3,8).
lower_bound_pair(fj7,bk4,8).
lower_bound_pair(fj7,bk5,8).
lower_bound_pair(fj7,bk6,8).
lower_bound_pair(fj7,bk7,8).
    lower_bound_pair(fj7,bk8,8).
    



    opposite(fa,ba).
    opposite(fb,bb).
    opposite(fc,bc).
    opposite(fd,bd).
    opposite(fe,be).
    opposite(ff,bf).
    opposite(fg,bg).
    opposite(fh,bh).
    opposite(fi,bi).
    opposite(fj1,bj1).
    opposite(fj2,bj2).
    opposite(fj3,bj3).
    opposite(fj4,bj4).
    opposite(fj5,bj5).
    opposite(fj6,bj6).
    opposite(fj7,bj7).

        opposite(fk1,bk1).
        opposite(fk2,bk2).
        opposite(fk3,bk3).
        opposite(fk4,bk4).
        opposite(fk5,bk5).
        opposite(fk6,bk6).
        opposite(fk7,bk7).
        opposite(fk8,bk8).

    */

/* graph 3 smaller*/

/*
starting_openlist([fa,fb,fc,fd,fe,fj1,fj2,fj3,fk1,fk2,fk3,fk4,bj1,bj2,bj3,bk1,bk2,bk3,bk4,be,bd,bc,bb,ba]).

c_star(4).

special_nodes([fa,be]).

parent(fa,fb,1).
    parent(fb,fc,1).
    parent(fc,fd,1).
    parent(fd,fe,1).

    parent(fa,fj1,1).
    parent(fa,fj2,1).
    parent(fa,fj3,1).
  
    
    parent(be,bd,1).
    parent(bd,bc,1).
    parent(bc,bb,1).
    parent(bb,ba,1).
 

    parent(be,bk1,1).
    parent(be,bk2,1).
    parent(be,bk3,1).
    parent(be,bk4,1).


    



lower_bound_pair(fa,be,1).
lower_bound_pair(fa,bd,2).
lower_bound_pair(fa,bc,3).
lower_bound_pair(fa,bb,4).

lower_bound_pair(fa,bk1,2).
lower_bound_pair(fa,bk2,2).
lower_bound_pair(fa,bk3,2).
lower_bound_pair(fa,bk4,2).


lower_bound_pair(fb,be,2).
lower_bound_pair(fb,bd,3).
lower_bound_pair(fb,bc,4).

lower_bound_pair(fb,bk1,3).
lower_bound_pair(fb,bk2,3).
lower_bound_pair(fb,bk3,3).
lower_bound_pair(fb,bk4,3).


lower_bound_pair(fc,be,3).
lower_bound_pair(fc,bd,4).

lower_bound_pair(fc,bk1,4).
lower_bound_pair(fc,bk2,4).
lower_bound_pair(fc,bk3,4).
lower_bound_pair(fc,bk4,4).


lower_bound_pair(fd,be,4).

lower_bound_pair(fd,bk1,5).
lower_bound_pair(fd,bk2,5).
lower_bound_pair(fd,bk3,5).
lower_bound_pair(fd,bk4,5).


lower_bound_pair(fj1,be,2).
lower_bound_pair(fj1,bd,3).
lower_bound_pair(fj1,bc,4).

lower_bound_pair(fj1,bk1,4).
lower_bound_pair(fj1,bk2,4).
lower_bound_pair(fj1,bk3,4).
lower_bound_pair(fj1,bk4,4).


lower_bound_pair(fj2,be,2).
lower_bound_pair(fj2,bd,3).
lower_bound_pair(fj2,bc,4).

lower_bound_pair(fj2,bk1,4).
lower_bound_pair(fj2,bk2,4).
lower_bound_pair(fj2,bk3,4).
lower_bound_pair(fj2,bk4,4).

lower_bound_pair(fj3,be,2).
lower_bound_pair(fj3,bd,3).
lower_bound_pair(fj3,bc,4).

lower_bound_pair(fj3,bk1,4).
lower_bound_pair(fj3,bk2,4).
lower_bound_pair(fj3,bk3,4).
lower_bound_pair(fj3,bk4,4).



    



    opposite(fa,ba).
    opposite(fb,bb).
    opposite(fc,bc).
    opposite(fd,bd).
    opposite(fe,be).

    opposite(fj1,bj1).
    opposite(fj2,bj2).
    opposite(fj3,bj3).
 

        opposite(fk1,bk1).
        opposite(fk2,bk2).
        opposite(fk3,bk3).
        opposite(fk4,bk4).
*/

    


    

    

