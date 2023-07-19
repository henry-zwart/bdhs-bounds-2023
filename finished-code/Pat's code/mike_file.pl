:- initialization(main).


    %this is the main for bdhs
  main:-(catch(bdhs,error(resource_error(Stack,Context)),(nl,write("here1"),nl,write(Stack),nl,write("here2"),nl,halt))).

    %this is the main for nbs
%  main:-(catch(nbs,error(resource_error(Stack,Context)),(nl,write("here1"),nl,write(Stack),nl,write("here2"),nl,halt))).


