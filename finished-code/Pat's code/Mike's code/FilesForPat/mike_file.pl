%%mike_file.pl
:- use_module(library(record)).
:- use_module(library(plunit)).  %% library for unit testing
:- use_module(library(statistics)). %% for doing timings
:- use_module(library(lists)).
:- use_module(library(pldoc)).  %% library for prolog documnetation package
:- use_module(library(statistics)).
:- use_module(library(ordsets)).

:- multifile version/3.

:- initialization(main).


%%%%lbComputation(f2e_lower_bound_pair).
%%%%lb(Node1, Node2, LBVal) :- once(f2e_lower_bound_pair(Node1,Node2,LBVal)).

%%%%problem("3_pancake_2_f2eHenry").


%this is the main for bdhs
% main:-(catch(bdhs,error(resource_error(Stack,Context)),(nl,write("here1"),nl,write(Stack),nl,write("here2"),nl,halt))).

   %this is the main for nbs
%  main:-(catch(nbs,error(resource_error(Stack,Context)),(nl,write("here1"),nl,write(Stack),nl,write("here2"),nl,halt))).

main() :- solutionEpisodes().
