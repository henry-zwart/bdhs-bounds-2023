:- multifile version/3.
version(total,5,"2").

%%%%  this version came from totalV04R3B

    %% version(ProgramBaseName, VersionNumber,RevisionNumber)

    %%%%%    # is where I left off

/*
%%%%:- semidet(openList/3).



%%%%:- det(stateRegister_forwardNodes/2).
%%%%:- det(lbAux/4).
%%%%:- det(nodeLB/3).
%%%%:- det(directionalLB/2).
%%%%:- det(globalLB/2).
%%%%:- det(addChildrenNodes/6).
%%%%:- det(stateRegister_lowestSolutionCostSoFar/2).

%%%%:- det(extractSolutions/2).
%%%%:- det(extractClosedSet/2).


%%%%:- det(extractSolutionsAux/2).

%%%%:- det(solutionEpisodeAux/3).
*/

%%%%:- print("g2"), nl.
/*
Gloabal variables:
Non backtracking:
- currentHeuristic
- currentLB
- jobStats
- closedSets
- episodeStats

Backtracking:
- expansionNumber
- currentEpisodeStats

*/ 
%%%%  Version History is at end of File
%% to jump there search for "Version History"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-  print("mid up 5"),nl.


/*

design of statistics data flow
------------------------------

a JOB is a combination of problem, search algorithm and an heuristic,
and its statistics cover all the ways to optimally solve that problem
using that algorithm and heuristic.  one way to optimally solve the problem
is called an EPISODE.  a job stat is a list of all possible episodes for
that job.

currentEpisodeStats is backtrackable 

OBSOLETE:
solutionEpisodes returns the final job stats (stored in global nb
 variable jobStats)
solutionEpisode returns the final episode stats (stored in global
backtrackable variable episodeStats)
solutionEpisodeAux builds up episode stats in currentEpisodeStats
(a backtrackable global variable)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

*/

:- dynamic opposite/2.
%% opposite(ForwardStateName, BackwardStateName)
%% opposite/2 ties together the forward and backward name of a specific state
%% if the state only has one name then there is no opposite/2 clause for it
%% 

:- use_module(library(record)).
:- use_module(library(plunit)).  %% library for unit testing
:- use_module(library(statistics)). %% for doing timings
:- use_module(library(lists)).
:- use_module(library(pldoc)).  %% library for prolog documnetation package
:- use_module(library(assoc)).
/* questions:
- can we update lb when we process children??

*/
directions([forward, backward]).
oppositeDirection(forward, backward).
oppositeDirection(backward, forward).

% :-  print("mid up 5 down 1"),nl.

/* %% --  nb_setval(currentHeuristic, hF2E). %% set the heuristic */

h(State, Goal, HValue) :-
    nb_getval(currentHeuristic, H),
    Call =.. [H, State, Goal, HValue],
    Call.

%%%% lb(Node1, Node2, LB) :-
%%%%     nb_getval(currentLB, CLB),
%%%%     Call =.. [CLB, Node1, Node2, LB],
%%%%     Call.

softFailure(1000). %% represents an open list being empty or the
              %% LB value of a node when the opposite openlist is empty

% :-  print("mid up 4"),nl.

:- dynamic aJobInfo/6.
:- record aJobInfo(domainName, problemName, problemSize, mode, optimalCost, heuristicName, algorithmName).
%%%% in json these fields are:
%%%% domain, problem, domainSize, cStar, heuristic, algorithm






/*

also want to record all collision information
i.e., how many collisions occured for what solution costs and at what point on that
solution path, will do this like we do stat counts


*/

:- dynamic problem/4.
:- record problem(name, initialState, goalState, fileName).
%% the information in problem will be stored in stateRegister


/* Top Level Code */


%! solutionEpisodes(+Initialstate:state, +GoalState:state, -Stats, -ClosedSets) is nondet
%
%  this explores every possible unique meta-level search path to an optimal solution

/*
solutionEpisodes(+InitialState, +GoalState, -Stats, -SizeOrderedSets)
Stats is a list of the episode stats generated from doing all the episodes
for a problem


*/
solutionEpisodes() :-
    %% make_aJobInfo([], JobInfo),
    %% nb_setval(thisJobInfo, JobInfo),
    special_nodes([InitialState, GoalState]),
    nb_setval(jobStats, []),
    b_setval(currentEpisodeStats, []),
    (once(findall(L2/ClosedSet,
                  (solutionEpisode(InitialState, GoalState, Solutions, ClosedSet),
                   length(ClosedSet, L),
                   L2 is L/2
                           %%  b_current(currentEpisodeStats, CurrentEpisodeStats),
                           %%  nb_setval(currentEpisodeStats, [Stats | CurrentEpisodeStats])
                  ),
                  ClosedSets))),
    empty_assoc(StatsIn),
    countSizes(ClosedSets, StatsIn, _Stats), %% what is _Stats supposed to do??
    empty_assoc(SetsIn),
    setSizedOrderedSets(ClosedSets, SetsIn, SizeOrderedSets),

    domain(DomainName),
    problem(ProblemName),
    size(ProblemSize),
    mode(Mode),
    c_star(OptimalCost),
    heuristic(HeuristicName),
    lbComputation(AlgorithmName),
    
    make_aJobInfo([domainName(DomainName), problemName(ProblemName), problemSize(ProblemSize),
                  mode(Mode), optimalCost(OptimalCost), heuristicName(HeuristicName),
                  algorithmName(AlgorithmName)], JobInfo),
    nb_getval(jobStats, JobStats),
    sort([JobInfo | JobStats], FinalJobStats),
%%%%%%    FinalJobStats = [JobInfo | JobStats],
    
    nb_setval(jobStats, FinalJobStats),
    transJobStatsIntoJSON(FinalJobStats).



 %! countSizes(+OrderedSets, -SizeCounts) is nondet
%
%  counts how many closed lists were of the different closed list sizes

countSizes([], StatsIn, Stats) :-
    (findall((Size/Count),
             gen_assoc(Size, StatsIn, Count),
             Stats)).

countSizes([Size/_Set | Rest], SizeCountsIn, SizeCountsOut) :-
    (   get_assoc(Size, SizeCountsIn, Count),
        NewCount is Count + 1,
        put_assoc(Size, SizeCountsIn, NewCount, NewSizeCountsIn)
    ;
        \+(get_assoc(Size, SizeCountsIn, _Count)),
        put_assoc(Size, SizeCountsIn, 1, NewSizeCountsIn)
    ),
    countSizes(Rest, NewSizeCountsIn, SizeCountsOut).



%! setSizedOrderedSets(+OrderedSets, -InSizeOrderedSets, OutSizeOrderedSets) is nondet
%
%  Input: OrderedSets is a list of closed lists that have the size of the
%  closed prepended, e.g., a closed list C of size 5 becomes 5/C
%  Output: SizeOrderedSets is a list of sets of closed lists where each
%  set all have the same size and each closed list is in a unique set

:- empty_assoc(SizeOrderedSets), nb_setval(closedSets, SizeOrderedSets).

setSizedOrderedSets([], InSizeOrderedSets, OutSizeOrderedSets) :-
    findall((Size/Set),
            gen_assoc(Size, InSizeOrderedSets, Set),
            OutSizeOrderedSets).

setSizedOrderedSets([Size/Set | Rest], InSizeOrderedSets,OutSizeOrderedSets) :-
    (   get_assoc(Size, InSizeOrderedSets, Sets),
        put_assoc(Size, InSizeOrderedSets, [Set | Sets], NewInSizeOrderedSets)
    ;
        \+(get_assoc(Size, InSizeOrderedSets, _Sets)),
        put_assoc(Size, InSizeOrderedSets, [Set], NewInSizeOrderedSets)
    ),
    setSizedOrderedSets(Rest, NewInSizeOrderedSets, OutSizeOrderedSets).

/*
solutionEpisode(+InitState, +GoalState, -Solutions,-_ClosedSet)


*/

solutionEpisode(InitState, InitState, [InitState], _ClosedSet).

solutionEpisode(InitialState, GoalState, Solutions, ClosedSet) :-
    % housekeeping to set up the episode
    InitialState \= GoalState,
%%    printJobInfo,
    b_setval(expansionNumber, 0),
    h(InitialState, GoalState, InHVal),
    make_node([state(InitialState), direction(forward),
               g(0), h(InHVal), lb(LBInit), lbExpandedAt(-1)],
              InitialNode),
    make_id([global_lb(-1),action(generate),direction(forward),
             g(0), h(InHVal),lb(LBInit)],
            InitGenId),
    make_id([global_lb(-1),action(open),direction(forward),
             g(0), h(InHVal),lb(LBInit)],
            InitOpenId),
    h(GoalState, InitialState, GoHVal),
    make_node([state(GoalState), direction(backward),
               g(0), h(GoHVal), lb(LBInit), lbExpandedAt(-1)],
              GoalNode),
    make_id([global_lb(-1),action(generate), direction(backward),
             
             g(0), h(GoHVal), lb(LBInit)],
            GoalGenId),
    make_id([global_lb(-1),action(open), direction(backward),
             
             g(0), h(GoHVal), lb(LBInit)],
            GoalOpenId),

    once(lb(InitialState, GoalState, LBInit)),
    
    makeStatList(EpisodeStats),
    incStatCount(EpisodeStats, 1, InitGenId, NewEpisodeStats_1),
    incStatCount(NewEpisodeStats_1, 1, GoalGenId, NewEpisodeStats_2),
    incStatCount(NewEpisodeStats_2, 1, GoalOpenId, NewEpisodeStats_3),
    incStatCount(NewEpisodeStats_3, 1, InitOpenId, NewestEpisodeStats),
    once(softFailure(SoftFailure)),
    %% nb_getval(solNum, SolNum),
    
    make_stateRegister([globalLB(LBInit),
                        forwardNodes([InitialNode]),
                        lowestSolutionCostSoFar(SoftFailure),
                        backwardNodes([GoalNode])],
                       StateRegister),
    b_setval(currentEpisodeStats, NewestEpisodeStats),
    solutionEpisodeAux(StateRegister, Solutions, ClosedSet).




%% solutionEpisodeAux(StateRegister, _Solutions, _ClosedSet) :-
%%     % checks for end of all productive episodes, i.e., episodes with a guaranteed optimal solution
%%     % checks whether either side's open list is empty
%%     %%
    
%%     emptyOpenList(StateRegister, _Direction).

solutionEpisodeAux(StateRegister, Solutions, ClosedSet) :-
    % checks for end of a productve episode
    % checks whether the current globalLB >= LowestSolutionCostSoFar
    %\+(emptyOpenList(StateRegister, _)),
    globalLB(StateRegister, GlobalLB),
    stateRegister_lowestSolutionCostSoFar(StateRegister,
                                              LowestSolutionCostSoFar), 
    (GlobalLB >= LowestSolutionCostSoFar),
    once(extractSolutions(StateRegister, Solutions)),
    extractClosedSet(StateRegister, ClosedSet),
    extractExpansionSequence(StateRegister, ExpansionSeq),
    %% nl,print("** Expansion Sequence **"),
    %% print(ExpansionSeq),
    ExpSeq = expansionSeq(ExpansionSeq),
    
    b_getval(currentEpisodeStats, NewestEpisodeStats),
    %%    print("** Stats **"), nl,
    sort(NewestEpisodeStats, SortedEpisodeStats),
    nb_getval(jobStats, JobStats),
    %% nl,print("JobsStats"),
    %% print(JobStats),
    nb_setval(jobStats, [SortedEpisodeStats | JobStats]),
    nb_getval(jobStats, NewJobStats),
    %% nl,print("New Job Stats"),
    %% print(NewJobStats),
    %%trace,
    NewJobStats = [ JobStat | Rest],
    NewerJobStats = [[ExpSeq, solution(Solutions) | JobStat] | Rest],
    nb_setval(jobStats, NewerJobStats).
    
    %% %% NewerJobStats = [[ExpSeq | JobStat] | Rest],
    
    %% %% ,
    %% %% %% nl,print("Newer Job Stats"),
    %% %% %% print(NewerJobStats),
    %% %% nb_getval(thisJobInfo, ThisJobInfo),
    %% %% set_optimalCost_of_jobInfo(LowestSolutionCostSoFar, ThisJobInfo),
    %% %% nb_setval(thisJobInfo, ThisJobInfo),
    %% %% print(SortedEpisodeStats), nl,
    %% %% print("** StateRegister **"), nl,
    %% %% print(StateRegister), nl.

solutionEpisodeAux(StateRegister, Solutions, ClosedSet) :-
    % handles the processing of an episode
    % guard conditions
    %\+(emptyOpenList(StateRegister, _)),
    
    globalLB(StateRegister, GlobalLB),
    %% testing
    %%print(GlobalLB), nl,
    %% end testing
    stateRegister_lowestSolutionCostSoFar(StateRegister, LowestSolutionCostSoFar),
    (GlobalLB < LowestSolutionCostSoFar),
    % end of guard conditions
    expandableNodes(StateRegister, GlobalLB, ExpandableNodes),
    %% nl,print("DEBUG - ExpandableNodes 1"),
    %% nl,print_term(ExpandableNodes,[]),nl,
    %% nl,print("** DEBUG - X"),nl,
    member(ParentNode, ExpandableNodes),
    %% nl,print("** DEBUG - picked "),print(ParentNode), nl,
    %% nl,print("** DEBUG - A"),nl,
    %% stopHere,
   %%% can ExpandableNodes be empty???

    node_lb(ParentNode, OldParentLB),
    %% nl,print("** DEBUG - B"),nl,
    nodeLB(ParentNode, StateRegister, NewParentLB),
    %% nl,print("** DEBUG - C"),nl,
    (NewParentLB > OldParentLB ->
         set_lb_of_node(NewParentLB, ParentNode, NewParentNode),
         stateRegisterRemoveNode(StateRegister, ParentNode, NewStateRegister),
         stateRegisterAddNode(NewStateRegister, NewParentNode, NewerStateRegister)
    ;
         expandNode(ParentNode, ChildrenStates),
         NewParentNode = ParentNode,
         processExpansion(NewParentNode, ChildrenStates, StateRegister, NewerStateRegister)
    ),
    %% nl,print("** DEBUG - D"),nl,
    calcGlobalLB(NewerStateRegister, NewGlobalLB),
    %% nl,print("** DEBUG - E"),nl,
    set_globalLB_of_stateRegister(NewGlobalLB, NewerStateRegister),
    %% nl,print("** DEBUG - old globalLB "),
    %% print(GlobalLB),nl,
    %% nl,print("** DEBUG - new globalLB "),
    %% print(NewGlobalLB),nl,
    %% nl,print("DEBUG - ExpandableNodes 2"),
    %% nl,print_term(ExpandableNodes,[]),nl,
    %% %%  checkBug1(NewestStateRegister),
    %% nl,print("** DEBUG - stateRegister"),
    %% print_term(NewerStateRegister,[]), nl,
    solutionEpisodeAux(NewerStateRegister, Solutions, ClosedSet).


%%% processExpansion(+ParentNode, +ChildStates,
%%                   +StateRegister, -NewestStateRegister)
processExpansion(ParentNode, ChildStates, StateRegister, NewestStateRegister) :-
    b_getval(expansionNumber, ExpansionNumber),
    NewExpansionNumber is ExpansionNumber + 1,
    b_setval(expansionNumber, NewExpansionNumber),
    globalLB(StateRegister, GlobalLB),
    set_node_fields([status(closed), expansionNumber(NewExpansionNumber),
                     lbExpandedAt(GlobalLB)],
                    ParentNode, NewParentNode),
    %% ACTION: expand
    %%   nb_getval(currentLB, GLB),
    node_h(NewParentNode, ParentH),
    node_g(NewParentNode, ParentG),
    node_lb(NewParentNode, ParentLB),
    node_direction(NewParentNode, Direction),
    make_id([global_lb(GlobalLB),action(expand),direction(Direction),
             g(ParentG), h(ParentH), lb(ParentLB)],
            ParentId),
    b_getval(currentEpisodeStats, CurrentEpisodeStats),
    incStatCount(CurrentEpisodeStats, 1, ParentId, UpdatedEpisodeStats),
    b_setval(currentEpisodeStats, UpdatedEpisodeStats),

    stateRegisterRemoveNode(StateRegister, ParentNode,
                            NewStateRegister),
    stateRegisterAddNode(NewStateRegister, NewParentNode, NewerStateRegister),
    %%% replace old parent node with new parent node
    addChildrenNodes(ChildStates, NewerStateRegister, NewParentNode,
                     NewestStateRegister).




%%% printJobInfo()

printJobInfo() :-
    once(version(ProgramBaseName, Version,Revision)),
    print("Program Root Name "), print(ProgramBaseName),
    print("Version "), print(Version),
    print(" Revision "), print(Revision),
    nl,
    once(problem(Name, InitialState, GoalState, _ProblemFileName)),
    print("Problem "), print(Name),
    print(" Initial State = "), print(InitialState),
    print(" Problem Goal = "), print(GoalState),
    nl.    

%% calcGlobalLB(+StateRegister, -GlobalLB)

calcGlobalLB(StateRegister, GlobalLB) :-
    stateRegister_globalLB(StateRegister, OldGlobalLB),
    openList(forward, StateRegister, FOpenList),
    directionalLB(FOpenList, FLB),
    openList(backward, StateRegister, BOpenList),
    directionalLB(BOpenList, BLB),
    max_list([OldGlobalLB, FLB, BLB], GlobalLB).




/*
extractExpansionSequence(+StateRegister, -ExpansionSeq)

*/

extractExpansionSequence(StateRegister, ExpansionSeq) :-
    stateRegister_forwardNodes(StateRegister, FNodes),
    findall((FExpansionNumber, FState),
            (member(FNode, FNodes),
             node_status(FNode, closed),
             node_state(FNode, FState),
             node_expansionNumber(FNode, FExpansionNumber)
            ),
            ClosedFNodes),
    stateRegister_backwardNodes(StateRegister, BNodes),
    findall((BExpansionNumber, BState),
            (member(BNode, BNodes),
             node_status(BNode, closed),
             node_state(BNode, BState),
             node_expansionNumber(BNode, BExpansionNumber)
            ),
            ClosedBNodes),
    append(ClosedFNodes, ClosedBNodes, ClosedNodes),
    list_to_ord_set(ClosedNodes, ExpansionSeq).



/*

extractClosedSet(+StateRegister, -ClosedSet)

collects both closed lists and puts them into a set that is returned

*/
extractClosedSet(StateRegister, RevClosedSet) :-
    stateRegister_forwardNodes(StateRegister, FNodes),
    findall((FExpansionNumber/FLBExpandedAt, FState),
            (member(FNode, FNodes),
             node_status(FNode, closed),
             node_state(FNode, FState),
             node_expansionNumber(FNode, FExpansionNumber),
             node_lbExpandedAt(FNode, FLBExpandedAt)
            ),
            ClosedFNodes),
    stateRegister_backwardNodes(StateRegister, BNodes),
    findall((BExpansionNumber/BLBExpandedAt, BState),
            (member(BNode, BNodes),
             node_status(BNode, closed),
             node_state(BNode, BState),
             node_expansionNumber(BNode, BExpansionNumber),
             node_lbExpandedAt(BNode, BLBExpandedAt)
            ),
            ClosedBNodes),
    append(ClosedFNodes, ClosedBNodes, ClosedNodes),
    list_to_ord_set(ClosedNodes, ClosedSet),
    stripExpansionNumber(ClosedSet, RevClosedSet)
%%    print(RevClosedSet)
.


/* stripExpansionNumber(+ClosedSet, -RevClosedSet)
*/

stripExpansionNumber(ClosedSet, RevClosedSet) :-
    stripExpansionNumber(ClosedSet, [], RevClosedSet).

stripExpansionNumber([], RevClosedSet, RevClosedSet).
stripExpansionNumber([(ExpansionNumber/LBExpandedAt, State) | Rest], RevClosedSetIn, RevClosedSetOut) :-
    stripExpansionNumber(Rest, [(ExpansionNumber, LBExpandedAt, State) | RevClosedSetIn], RevClosedSetOut).

% :- print("mid up 2"), nl.

/*

extractSolutions(+StateRegister, -Solutions)
*/
extractSolutions(StateRegister, FinalSolutions) :-
    stateRegister_collisionStates(StateRegister, CollisionStates),
%    nl,print("eS 1"),
    %%stateRegister_forwardCollisionNode(StateRegister, ForwardNode),
    %%stateRegister_backwardCollisionNode(StateRegister, BackwardNode),
    extractSolutionsAux(StateRegister, CollisionStates, [], FinalSolutions).


extractSolutionsAux(_StateRegister, [], FinalSolutions, FinalSolutions).

extractSolutionsAux(StateRegister, [CollisionState | Rest], Solutions, FinalSolutions):-
    stateDirectionalNames(CollisionState, ForwardName, BackwardName),
%    nl, print("eS 3"),
    extractPath(StateRegister, forward, ForwardName,
                [], ForwardStatePath),
    extractPath(StateRegister, backward, BackwardName,
                [], BackwardStatePath),
    reverse(BackwardStatePath, [_CollisionState | ReverseBackwardPath]),
    append(ForwardStatePath, ReverseBackwardPath, Solution),
    extractSolutionsAux(StateRegister, Rest, [Solution | Solutions], FinalSolutions).

/*
stateForwardName(+StateName, -ForwardName)

logic:
  if opposite(Name, StateName)
  then StateName is in the backward direction
  else StateName is forward
*/
stateForwardName(StateName, ForwardName) :-
    (opposite(Name, StateName) *->
         ForwardName = Name
    ;
         ForwardName = StateName).

/*
stateDirectionalNames(+State, -ForwardName, -BackwardName)

When a state has different forward and backward names, i.e.,
opposite(ForwardName, BackwardName),
stateDirectionalNames will instantiate the two names
When there's only one namefor the state, i.e., no opposite clauses,
then ForwardName and BackwardName are simply State

*/


stateDirectionalNames(State, ForwardName, BackwardName) :-
    (   \+(opposite(_,_)) *->   % there are no opposite / 2 clauses
         (ForwardName = State,
          BackwardName = State)
    ;
        opposite(State, BackwardName) *->
          ForwardName = State
    ;
        opposite(ForwardName, State) *->
             BackwardName = State
    )
.

%% %% stateForwardNameTestSetup :-
%% %%     retractall(opposite(_,_)),
%% %%     assert(opposite("x","y")).

%% %% stateForwardNameTestCleanUp :-
%% %%     retractall(opposite(_,_)).

%% %% :- begin_tests(stateForwardName).
%% %% test(stateForwardName) :-
%% %%     stateForwardNameTestSetup,
%% %%     stateForwardName("x",FN),
%% %%     print(FN), nl,
%% %%     stateForwardName("y", FN2),
%% %%     print(FN2), nl,
%% %%     stateForwardName("z", FN3),
%% %%     print(FN3), nl,
%% %%     stateForwardNameTestCleanUp.

%% %% :- end_tests(stateForwardName).
               
/*
extractPath(+StateRegister, +Direction, +State, +Path, -NewPath)

StateRegister contains both a forward and a backward part of the solution
pat, which collide at State, 

Direction == the direction of the part of the solution being extracted,
i.e., the forward or the backward part of the solution.

State == the collision state for the solution path (i.e., where the forward and the
backward parts of the solution meet up, in Pat's domains, each state has 2 names, a forward
name and a backward name, State could be either the forward name or the backward name,
but in StateRegister states in collisionStates use their forward names

Path / NewPath, the accumulator pair for this direction part of the solution

extracts the states along the Path from State to terminus
in StateRegister

we have augmented the state to be a pair: (state, expansionNumber)

logic:
    find the forward name, CollisionState, of the collision state
    get the CollisionState node, Node, in Direction in StateRegister
    if Node does not have a parent state
    then return the part of the solution path in this Direction
            with State as the first element in the path
    else recurse extractPath on the parent state with the Path updated


*/
extractPath(StateRegister, Direction, State, Path, FinalPath) :-
    inStateRegister(State, StateRegister, Direction, Node),
    node_parent(Node, nil),
    node_expansionNumber(Node, ExpansionNumber),
    stateForwardName(State, ForwardName),
    FinalPath = [(ForwardName, ExpansionNumber) | Path].

extractPath(StateRegister, Direction, State, Path, NewPath) :-
    inStateRegister(State, StateRegister, Direction, Node),
    node_parent(Node, ParentState),
    ParentState \== nil,
    node_expansionNumber(Node, ExpansionNumber),
    %%node_state(ParentNode, ParentState),
    %%inStateRegister(ParentState, StateRegister, Direction, ParentNode),
    stateForwardName(State, ForwardName),
    extractPath(StateRegister, Direction, ParentState,
                [(ForwardName, ExpansionNumber) | Path], NewPath). 

/* test cases

1. Node.parent == nil
   then return [Node.state | Path]

2. Node.parent == non-nil
   then recurse with ParentNode and [NodeState | Path]

extractPath(stateRegister(_5720,
[node("nexp", "f2", forward, open, "f1", 1, 0, 2, _5096)],
[node(0, "f1", forward, closed, nil, 0, 0, 1, 1), node("nexp", "f2", forward, open, "f1", 1, 0, 2, _5096)],
[node(3, "b1", forward, closed, nil, 0, 0, 2, 2), node("nexp", "b2", forward, open, "b1", 1, 0, 2, _5526)],
2),
forward,
"f2",
[],
_5792) ? 

*/

:- begin_tests(extractPath).
   % % test(extractPath1) :-
   % %     make_node([state(a)], A),
   % %     make_stateRegister([forwardNodes([A])], SR),
% %     extractPath(SR, A, [], _P).

test(extractPath2) :-
    make_node([expansionNumber(0), state("f1"),status(closed),
               g(0),lb(1),lbExpandedAt(1)], F1),
    make_node([state("f2"), status(open),g(1), lb(2), parent("f1")], F2),
    make_node([expansionNumber(1), state("b1"), status(closed),
               direction(backward) ,g(0), lb(2), lbExpandedAt(2)],
              B1),
    make_node([state("b2"), status(open), g(1),
               direction(backward),lb(2), parent("b1")], B2),
    make_stateRegister([collisionStates(["f2"]),
                        solNum(1),
                        forwardNodes([F1,F2]),
                        backwardNodes([B1,B2]),
                        lowestSolutionCostSoFar(2)],
                       SR),
    extractPath(SR, forward, "f2", [], Solutions),
    print(Solutions),
    extractPath(SR, backward, "b2", [], Sol2),
    print(Sol2),
    nl,nl,
    extractSolutions(SR,Sss),
    print(Sss)
.



:- end_tests(extractPath).


addChildrenNodes([], StateRegister, _ParentNode, StateRegister).
addChildrenNodes([ChildState | ChildrenStates],
                 StateRegister, 
                 ParentNode, NewestStateRegister)   :-
    once((
           node_lbExpandedAt(ParentNode, GLB),  %% the Global Lower Bound that the child is
           %%  generated and added to the open list is what it was when its parent was expanded
           node_direction(ParentNode, Direction),
           childGValue(Direction, ChildState, ParentNode, ChildG),
           node_state(ParentNode, PS),
           h(ChildState, _, ChildH),
           make_node([state(ChildState), direction(Direction),
                      parent(PS), g(ChildG), h(ChildH), lbExpandedAt(GLB)],
                      ChildNode),
           collisionUpdate(ChildNode, StateRegister, NewStateRegister),    
           nodeLB(ChildNode, NewStateRegister, NodeLB),
           node_lb(ChildNode, NodeLB), %% this should set ChildNode's LB!!!!!
           %% ACTION: generate
           %%   globalLB(StateRegister, GLB),
           %%   nb_getval(currentLB, GLB),
           %%   node_h(ChildNode, ChildH),
           %%   node_lb(ChildNode, ChildLB)
           make_id([global_lb(GLB),action(generate),direction(Direction),
                    g(ChildG), h(ChildH), lb(NodeLB)],
                   ChildId),
           %           node_state(ChildNode, State),
           %          nl, print(State), print(ChildId), nl,
           b_getval(currentEpisodeStats, CurrentEpisodeStats),
           incStatCount(CurrentEpisodeStats, 1, ChildId, UpdatedEpisodeStats),
           b_setval(currentEpisodeStats, UpdatedEpisodeStats),
           processChildNode(ChildNode, NewStateRegister, NewerStateRegister),


           
           %% softFailure(SoftFailure),
           %% (SoftFailure = NodeLB  *->
           %%      NewStateRegister = NewerStateRegister
           %% ;
           %%      node_lb(ChildNode, NodeLB), %% this should set ChildNode's LB!!!!!
           %%      %% ACTION: generate
           %%      %%   globalLB(StateRegister, GLB),
           %%      %%   nb_getval(currentLB, GLB),
           %%      %%   node_h(ChildNode, ChildH),
           %%      %%   node_lb(ChildNode, ChildLB)
           %%      make_id([global_lb(GLB),action(generate),direction(Direction),
           %%               g(ChildG), h(ChildH), lb(NodeLB)],
           %%              ChildId),
           %%      %           node_state(ChildNode, State),
           %%      %          nl, print(State), print(ChildId), nl,
           %%      b_getval(currentEpisodeStats, CurrentEpisodeStats),
           %%      incStatCount(CurrentEpisodeStats, 1, ChildId, UpdatedEpisodeStats),
           %%      b_setval(currentEpisodeStats, UpdatedEpisodeStats),
           %%      processChildNode(ChildNode, NewStateRegister, NewerStateRegister)
           %% ),
           addChildrenNodes(ChildrenStates,
                            NewerStateRegister, 
                            ParentNode, NewestStateRegister))
        ).

:- begin_tests(addChildrenNodes).
/*
test cases:
1. nodes to the forward nodes

2. adding nodes to the backward nodes
*/
test(addChildrenNodes1) :-
    tmake_SR14(SR14, ChildStates, ParentNode),
    addChildrenNodes(ChildStates, SR14, ParentNode,
                     _, _,
                     NSR),
    stateRegister_forwardNodes(NSR, [node(c,forward,open,a,1,0,2,_2776),
                                    node(b,forward,open,a,1,0,2,_2326),
                                    node(a,forward,open,nil,0,0,1,
                                         _1998)]),
    stateRegister_backwardNodes(NSR, [node(a,forward,open,nil,0,0,1,_1998)])
                     %% stateRegister([node(c,forward,open,a,1,0,2,_2776),
                     %%                node(b,forward,open,a,1,0,2,_2326),
                     %%                node(a,forward,open,nil,0,0,1,_1998)],
                     %%               [node(a,forward,open,nil,0,0,1,_1998)]))
.

test(addChildrenNodes2) :-
    tmake_SR13(SR13, ChildStates, ParentNode),
    addChildrenNodes(ChildStates, SR13, ParentNode,
                     _, _,
                     NSR),
    stateRegister_forwardNodes(NSR, [node(f,backward,open,nil,0,0,1,_13318),
                                    node(b,backward,closed,a,2,0,3,
                                         _13500)]),
    stateRegister_backwardNodes(NSR, [node(d,backward,open,f,1,0,2,_14502),
                                    node(e,backward,open,f,1,0,2,_14040)])
                     %% stateRegister([node(f,backward,open,nil,0,0,1,_13318),
                     %%                node(b,backward,closed,a,2,0,3,_13500)],
                     %%               [node(d,backward,open,f,1,0,2,_14502),
                     %%                node(e,backward,open,f,1,0,2,_14040)]))
.

:- end_tests(addChildrenNodes).

/* processChildNode(+ChildNode, +StateRegister, -NewStateRegister)

always returns true
might "update" StateRegister
  if ChildNode state already in StateRegister in the same direction  
  then it is a duplicate
       if its g-value is lower than the old one
       then remove the old one (regardless of whether it is open or closed)
            add the new one as open
       else  ignore new one
  else
       add new one as open


*/
processChildNode(ChildNode, StateRegister, NewerStateRegister) :-
    node_state(ChildNode, ChildState),
    node_direction(ChildNode, ChildDirection),
    node_g(ChildNode, ChildG),
    (  inStateRegister(ChildState, StateRegister, ChildDirection,
                       InNode),
       node_g(InNode, InG),
       (  ChildG < InG,
          %% ACTION: reOpen
          globalLB(StateRegister, GLB),
          %% nb_getval(currentLB, GLB),
          node_h(ChildNode, ChildH),
          node_lb(ChildNode, ChildLB),
          make_id([global_lb(GLB),action(reOpen),direction(ChildDirection),
                   g(ChildG), h(ChildH), lb(ChildLB)],
                  ChildId),
          b_getval(currentEpisodeStats, CurrentEpisodeStats),
          incStatCount(CurrentEpisodeStats, 1, ChildId, UpdatedEpisodeStats),
          b_setval(currentEpisodeStats, UpdatedEpisodeStats),

          stateRegisterRemoveNode(StateRegister,InNode, NewStateRegister),
          stateRegisterAddNode(NewStateRegister, ChildNode, NewerStateRegister)
       ;
          ChildG >= InG,
          %% ACTION: ignore
          globalLB(StateRegister, GLB),
          %% nb_getval(currentLB, GLB),
          node_h(ChildNode, ChildH),
          node_lb(ChildNode, ChildLB),
          make_id([global_lb(GLB),action(ignore),direction(ChildDirection),
                   g(ChildG), h(ChildH), lb(ChildLB)],
                  ChildId),
          
%          node_state(ChildNode, State),
%          nl, print(State), print(ChildId), nl,
          b_getval(currentEpisodeStats, CurrentEpisodeStats),
          incStatCount(CurrentEpisodeStats, 1, ChildId, UpdatedEpisodeStats),
          b_setval(currentEpisodeStats, UpdatedEpisodeStats),

          NewerStateRegister = StateRegister
       )
    ;
       \+(inStateRegister(ChildState, StateRegister, ChildDirection,
                          _InNode)),
       %% ACTION: open
       globalLB(StateRegister, GLB),
       %% nb_getval(currentLB, GLB),
       node_h(ChildNode, ChildH),
       node_lb(ChildNode, ChildLB),
       make_id([global_lb(GLB),action(open),direction(ChildDirection),
                g(ChildG), h(ChildH), lb(ChildLB)],
               ChildId),
       
%       node_state(ChildNode, State),
%       nl, print(State), print(ChildId), nl,
       b_getval(currentEpisodeStats, CurrentEpisodeStats),
       incStatCount(CurrentEpisodeStats, 1, ChildId, UpdatedEpisodeStats),
       b_setval(currentEpisodeStats, UpdatedEpisodeStats),

       stateRegisterAddNode(StateRegister, ChildNode, NewerStateRegister)
    )
.

% :- print("mid up 1"),nl.

:- begin_tests(processChildNode).
/* test cases:
1. child already in register as open node & child has lower g
   old one deleted from register
   new one added as open node

2. child already in register as closed node & child has lower g
   old one deleted from register
   new one added as open node

3. child already in register & child has higher g
   no change to register

4. child already in register & child has equal g
   no change to register

5. child not in register
   child added as open node to register

*/
test(processChildNode_1) :-
    tmake_SR4(SR),
    make_node([state(b),status(open), direction(backward), g(0)], CHN),
    processChildNode(CHN, SR, NSR),
    stateRegister_forwardNodes(NSR, [node(d,forward,open,nil,1,0,2,_2522),
                                    node(e,forward,closed,nil,1,0,4,
                                         _2694)]),
    stateRegister_backwardNodes(NSR, [node(b,backward,open,nil,0,0,_2928,_2930),
                                    node(a,backward,open,nil,1,0,2,_2350),
                                    node(c,backward,closed,nil,1,0,2,_2006)])
                     %% stateRegister([node(d,forward,open,nil,1,0,2,_2522),
                     %%                node(e,forward,closed,nil,1,0,4,_2694)],
                     %%               [node(b,backward,open,nil,0,0,_2928,_2930),
                     %%                node(a,backward,open,nil,1,0,2,_2350),
                     %%                node(c,backward,closed,nil,1,0,2,_2006)]))
.

test(processChildNode_2) :-
    tmake_SR4(SR),
    make_node([state(c),status(open), direction(backward), g(0)], CHN),
    processChildNode(CHN, SR, NSR),
    stateRegister_forwardNodes(NSR, [node(d,forward,open,nil,1,0,2,_5262),
                                    node(e,forward,closed,nil,1,0,4,
                                         _5434)]),
    stateRegister_backwardNodes(NSR, [node(c,backward,open,nil,0,0,_5668,_5670),
                                    node(a,backward,open,nil,1,0,2,_5090),
                                    node(b,backward,open,nil,1,0,2,_4918)])
                     %% stateRegister([node(d,forward,open,nil,1,0,2,_5262),
                     %%                node(e,forward,closed,nil,1,0,4,_5434)],
                     %%               [node(c,backward,open,nil,0,0,_5668,_5670),
                     %%                node(a,backward,open,nil,1,0,2,_5090),
                     %%                node(b,backward,open,nil,1,0,2,_4918)]))
.
                    
test(processChildNode_3) :-
    tmake_SR4(SR),
    make_node([state(c),status(open), direction(backward), g(3)], CHN),
    processChildNode(CHN, SR, SR)
.

test(processChildNode_4) :-
    tmake_SR4(SR),
    make_node([state(c),status(open), direction(backward), g(1)], CHN),
    processChildNode(CHN, SR, SR)
.

test(processChildNode_5) :-
    tmake_SR4(SR),
    make_node([state(z),status(open), direction(backward), g(1)], CHN),
    processChildNode(CHN, SR, NSR),
    stateRegister_backwardNodes(NSR, [node(z,backward,open,nil,1,0,_28666,_28668),
                                    node(a,backward,open,nil,1,0,2,_28088),
                                    node(b,backward,open,nil,1,0,2,_27916),
                                    node(c,backward,closed,nil,1,0,2,
                                         _27744)]),
    stateRegister_forwardNodes(NSR, [node(d,forward,open,nil,1,0,2,_28260),
                                    node(e,forward,closed,nil,1,0,4,_28432)])
                     %% stateRegister([node(d,forward,open,nil,1,0,2,_28260),
                     %%                node(e,forward,closed,nil,1,0,4,_28432)],
                     %%               [node(z,backward,open,nil,1,0,_28666,_28668),
                     %%                node(a,backward,open,nil,1,0,2,_28088),
                     %%                node(b,backward,open,nil,1,0,2,_27916),
                     %%                node(c,backward,closed,nil,1,0,2,_27744)]))
.


:- end_tests(processChildNode).


stateRegisterNodeList(backward, StateRegister, NodeList) :-
    stateRegister_backwardNodes(StateRegister, NodeList).

stateRegisterNodeList(forward, StateRegister, NodeList) :-
    stateRegister_forwardNodes(StateRegister, NodeList).


:- begin_tests(stateRegisterNodeList).
test(stateRegisterNodeList1f) :-
    make_stateRegister([forwardNodes([]), backwardNodes([])],
                       SR),
    stateRegisterNodeList( forward, SR, []).

test(stateRegisterNodeList1b) :-
    make_stateRegister([forwardNodes([]), backwardNodes([])],
                       SR),
    stateRegisterNodeList( backward ,SR, []).

test(stateRegisterNodeList2f) :-
    make_stateRegister([forwardNodes([a,b,c]), backwardNodes([x,y,z])],
                       SR),
    stateRegisterNodeList(forward, SR, [a,b,c]).

test(stateRegisterNodeList2b) :-
    make_stateRegister([forwardNodes([a,b,c]), backwardNodes([x,y,z])],
                       SR),
    stateRegisterNodeList(backward, SR, [x,y,z]).


:- end_tests(stateRegisterNodeList).



/*
inStateRegister(+ChildState, +StateRegister, +ChildDirection, -InNode) 

returns true if ChildState is already in StateRegister in ChildDirection
with InNode bound to the node with that state
else fails

Note:
1. the bagof should succeed if it only finds exactly one such node

2. translateIntoTargetState is for translating state names from Pat's
system to this one, in Pat's a state has a forward name and a backward name,
while this system assumes the state name is the same in both directions
*/

inStateRegister(State, StateRegister, Direction, Node) :-
%    translateIntoTargetState(ChildDirection, ChildState, TargetState),
    stateRegisterNodeList(Direction, StateRegister, NodeList),
    bagof(Node,
          (member(Node, NodeList),
           %           node_state(Node, TargetState)),
           node_state(Node, State)),
          [Node])
.

:- begin_tests(inStateRegister).
/*
test for both directions
and test for both the state being in and not in the stateRegister
*/
test(inStateRegisterInB) :-
    tmake_SR4(SR),
    stateRegister_backwardNodes(SR, [_, BN | _]),    
    inStateRegister(b, SR, backward, BN).

test(inStateRegisterOutB) :-
    tmake_SR4(SR),
    stateRegister_backwardNodes(SR, [_, BN, _]),    
    \+(inStateRegister(z, SR, backward, BN)).

test(inStateRegisterInF) :-
    tmake_SR4(SR),
    stateRegister_forwardNodes(SR, [FN |_]),    
    inStateRegister(d, SR, forward, FN).

test(inStateRegisterOutF) :-
    tmake_SR4(SR),
    stateRegister_forwardNodes(SR, _BNs),    
    \+(inStateRegister(z, SR, backward, _)).


:- end_tests(inStateRegister).

% :- print("mid 0"),nl.

    /*
    stateRegisterRemoveNode(+StateRegister, +InNode, -UpdatedStateRegister)

    always returns true
    deletes InNode from stateRegister nodes in its direction
    ?? what should we do if InNode not in stateRegister?
    I would say that is a bug and we should throw an error
    */

stateRegisterRemoveNode(StateRegister, InNode, NewStateRegister) :-
    node_direction(InNode, Direction),
    stateRegisterNodeList(Direction, StateRegister, NodeList),
    subtract(NodeList, [InNode], NewNodeList),
    (  Direction == forward,
       set_forwardNodes_of_stateRegister(NewNodeList, StateRegister,
                                         NewStateRegister)
    ;
       Direction == backward,
       set_backwardNodes_of_stateRegister(NewNodeList, StateRegister,
                                       NewStateRegister)
    )
.    

:- begin_tests(stateRegisterRemoveNode).
/* test cases:
- InNode is open in NodeList
- InNode is closed in NodeList
- InNode is not in NodeList
- NodeList is empty
*/
test(stateRegisterRemoveNodeIONL) :-
    tmake_SR4(SR),
    make_node([state(d), status(open), direction(forward), g(1), lb(2)],
              D),
    stateRegisterRemoveNode(SR, D, NSR),    
    stateRegister_forwardNodes(NSR, FNs),
    FNs = [node(e,forward,closed,nil,1,0,4,_5064)],
    stateRegister_backwardNodes(NSR, BNs),
    stateRegister_backwardNodes(SR, BNs)
    %%  [node(e,forward,closed,nil,1,0,4,_5064)],
    %%                   [node(a,backward,open,nil,1,0,2,_4720),
    %%                    node(b,backward,open,nil,1,0,2,_4548),
    %%                    node(c,backward,closed,nil,1,0,2,_4140)])
    %%     = NSR
    %%
.

test(stateRegisterRemoveNodeICNL) :-
    tmake_SR4(SR),
    make_node([state(c), status(closed), direction(backward), g(1), lb(2)], C),
    stateRegisterRemoveNode(SR, C, NSR),
    stateRegister_forwardNodes(NSR, FNs),
    stateRegister_forwardNodes(SR, FNs),
    stateRegister_backwardNodes(NSR, BNs),
    BNs = [node(a,backward,open,nil,1,0,2,_4858),
           node(b,backward,open,nil,1,0,2,_4686)]
                   %% stateRegister([node(d,forward,open,nil,1,0,2,_5030),
                   %%                node(e,forward,closed,nil,1,0,4,_5202)],
                   %%               [node(a,backward,open,nil,1,0,2,_4858),
                   %%                node(b,backward,open,nil,1,0,2,_4686)]))
.

test(stateRegisterRemoveNodeINNL) :-
    tmake_SR4(SR),
    make_node([state(z), status(closed), direction(backward), g(1), lb(2)], C),
    stateRegisterRemoveNode(SR, C, SR)
.

test(stateRegisterRemoveNodeINoL) :-
    tmake_SR12(SR, C),
    stateRegisterRemoveNode(SR, C, SR)
.


:- end_tests(stateRegisterRemoveNode).


/* collisionUpdate(+ChildNode, +StateRegister, -NewStateRegister)
this predicate just updates LowestSolutionCostSoFar and the
 collision nodes if necessary

Note: this pred always succeeds!!
if no collision
then Newest_lowestSolutionCostSoFar is set to
       LowestSolutionCostSoFar
       Note: this means that a collision where no lower cost path is found
             looks the same as no collision
             may need to change this some time

     if ChildState already in opposite direction in stateRegister
     then if its collision solution length is < current LowestSolutionCostSoFar
          then set Newest_lowestSolutionCostSoFar to new solution cost 
          else set Newest_lowestSolutionCostSoFar to LowestSolutionCostSoFar
     else set Newest_lowestSolutionCostSoFar to LowestSolutionCostSoFar

*/  
/*

oppositeStateForNode(+State, -CollisionState, , -OppositeOpenState)

this predicate is used to say what state name we are looking for in the opposite open list,
i.e., the collision node

if we are in a domain where the same state has different names for the forward and backward directions
then the forward state name is used in the collisionStates list of the stateRegister
     and in the opposite/2 clauses, the forward name is always the first argument
else the state names in the two directions are the same

if opposite(State, BackwardState)
then BackwardState is what we will look for in the opposite open list,
     but State is what will be stored in collisionStates
else either State is opposite(BackwardState, State)
     then BackwardState is what we will look for
          and BackwardState is what we will store
     else State is not in any opposite clause
          then State is used in both looking in the opposite open list
               and is stored in collisionStates


oppositeStateForNode(State, CollisionState) :-
    (   opposite(State, BackwardState) ->
        CollisionState = BackwardState
    ;
        CollisionState = State
    ).
*/
oppositeStateForNode(State, State, State) :-
    opposite(State, State).

oppositeStateForNode(State, CollisionState, OppositeOpenState) :-
    \+(opposite(State, State)),
    opposite(State, BackwardState),
    CollisionState = State,
    OppositeOpenState = BackwardState.

oppositeStateForNode(State, CollisionState, OppositeOpenState) :-
    \+(opposite(State, State)),
    opposite(BackwardState, State),
    CollisionState = BackwardState,
    OppositeOpenState = BackwardState.

oppositeStateForNode(State, CollisionState, OppositeOpenState) :-
    \+(opposite(State, State)),
    \+(opposite(_, State)),
    \+(opposite(State, _)),
    CollisionState = State,
    OppositeOpenState = State.

collisionUpdate(ChildNode, StateRegister, NewStateRegister) :-
    node_direction(ChildNode,  ChildDirection),
    oppositeDirection(ChildDirection, OppDirection),
    node_g(ChildNode, ChildG),
    stateRegister_lowestSolutionCostSoFar(StateRegister,
                                              LowestSolutionCostSoFar),
    node_state(ChildNode, ChildState),
/* problem, why when opposite fails doesn't the else occur */

    oppositeStateForNode(ChildState, CollisionState, OppositeOpenState),
    (   inStateRegister(OppositeOpenState, StateRegister, OppDirection,
                        CollisionNode),
        node_g(CollisionNode, CollisionNodeGValue),
        PathCost is ChildG + CollisionNodeGValue,
        (  PathCost < LowestSolutionCostSoFar, %% found new "lowest" cost solution
           NewestLowestSolutionCostSoFar = PathCost,
           set_stateRegister_fields([collisionStates([CollisionState]),
                                     lowestSolutionCostSoFar(NewestLowestSolutionCostSoFar)],
                                    StateRegister,
                                    NewStateRegister)
        ;
           PathCost = LowestSolutionCostSoFar,
           stateRegister_collisionStates(StateRegister, CollisionStates),
           set_stateRegister_fields([collisionStates([CollisionState
                                                     | CollisionStates])],
                                    StateRegister,
                                    NewStateRegister)
        ;
           PathCost > LowestSolutionCostSoFar,
           NewStateRegister = StateRegister
        )
    ;
        (
           \+(inStateRegister(CollisionState, StateRegister, OppDirection,
                           _CollisionNode)),
           NewStateRegister = StateRegister
        )
    )
.

:- begin_tests(collisionUpdate).
/*
following cases:
- ChildState in opposite direction and path is new lowest
- ChildState in opposite direction and path is same as lowest
- ChildState in opposite direction & path is greater than lowest
- ChildState not in opposite direction
*/
test(collisionUpdateNewLow) :-
    tmake_SR4(SR),
    make_node([state(c), g(1)], C),
    %% node_direction(C,D),
    collisionUpdate(C, SR, NSR),
    stateRegister_lowestSolutionCostSoFar(NSR, 2),
    stateRegister_forwardCollisionNode(NSR, C),
    stateRegister_backwardCollisionNode(NSR, BCN),
    node_state(BCN, c).

test(collisionUpdateSameLow1) :-
    tmake_SR5(SR),
    make_node([state(b), g(1)], B),
    collisionUpdate(B, SR, NSR),
    stateRegister_lowestSolutionCostSoFar(NSR, 1),
    stateRegister_forwardCollisionNode(NSR, B),
    stateRegister_backwardCollisionNode(NSR, BCN),
    node_state(BCN, b).

test(collisionUpdateSameLow2) :-
    tmake_SR5(SR),
    make_node([state(b), g(2)], B),
    collisionUpdate(B, SR, SR).


test(collisionUpdateSameLow3) :-
    tmake_SR5(SR),
    make_node([state(x), g(2)], X),
    collisionUpdate(X, SR, SR).



:- end_tests(collisionUpdate).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*   - stateRegister . pl */


/*

stateRegister Internal Data Structure
=============================================

Let stateRegister simply be of the format
stateRegister(ForwardNodes, BackwardNodes)
     where ForwardNodes is a list of all forward nodes
       and BackwardNodes is a list of all backward nodes


stateRegister data structure API
=============================================
emptyOpenList(+StateRegister, ?Direction)
    returns true if Direction's open list is empty

globalLB(+StateRegister, ?GlobalLB)
    returns true if StateRegister's global lower bound is GlobalLB
    which is true if the max LB in either Direction is GlobalLB
    

directionalLB(+StateRegister, +Direction, ?DirectionalLB)
    the LB in Direction is the min LB of any node in Direction' open list

nodeLB(+StateRegister, +Direction, ?Node ?NodeLB)
    the LB of a node is the min lb of any pair of must expand nodes involved with that node

expandableNodes(+StateRegister, +GlobalLB, -ExpandableNodes)
    selects from both open lists the open nodes which can be expanded right now

addChildrenNodes(+ChildrenStates, +StateRegister, +ParentNode,
              +LowestSolutionCostSoFar, +Stats, 
              -Newest_lowestSolutionCostSoFar,
              -NewStats, -NewStateRegister)
     updates LowestSolutionCostSoFar if necessary
     updates Stats, and StateRegister
     this is where the computation of LBs are done, both for the children
     and for the nodes on the other side that got their LBs because they paired
     with the ParentNode  


*/

/*
after optimal solution is proved, lowestsolutioncostsofar will be C*

if an optimal solution is found below C*, then we may find more than
one optimal solution, we would like to capture all the solution paths
that we find.

since we are doing duplicate pruning, collisions will occur at unique
nodes, i.e., for if one state is in a collision , that will be the
only collision it is in.  So, to capture all the collisions, we only
need to collect the optimal collision states.  Since there will only
one forward and one backward node for that state in the state
register, we can recover the solution paths from those states.

We will need to make sure those collision nodes are in their
respective closed lists.
*/
:-  record stateRegister(solNum, collisionStates =  [],
                         globalLB, %% highest global lower bound yet
                         forwardNodes = [], %%forwardCollisionNode = nil,
                         backwardNodes = [], %%backwardCollisionNode = nil,
                         lowestSolutionCostSoFar = 1000) % softwareFailure value
.

stateRegisterAddNode(StateRegister, Node, NewStateRegister) :-
    (   node_direction(Node, forward),
        stateRegister_forwardNodes(StateRegister,
                                   ForwardNodes),
        set_forwardNodes_of_stateRegister([Node| ForwardNodes],
                                          StateRegister, 
                                          NewStateRegister)
    ;
        node_direction(Node, backward),
        stateRegister_backwardNodes(StateRegister,
                                   BackwardNodes),
        set_backwardNodes_of_stateRegister([Node | BackwardNodes],
                                           StateRegister, 
                                           NewStateRegister)
    ).


:- begin_tests(stateRegisterAddNode).
/* test cases:
1.  add a node to forward nodes

2. add a node to backward nodes
*/

test(stateRegisterAddNode1) :-
    tmake_SR12(SR12, C),
%%    print(C),
    stateRegisterAddNode(SR12, C, NewSR12),
    stateRegister_backwardNodes(NewSR12, BNs),
    member(FN, BNs),
    node_state(FN, c)
%%  ,  print(NewSR12)
.



:- end_tests(stateRegisterAddNode).


expandableNodes(StateRegister, GlobalLB, ExpandableNodes) :-
    stateRegister_forwardNodes(StateRegister, ForwardNodes),
    findall(FNode,
            (member(FNode, ForwardNodes),
             node_status(FNode, open),
             node_lb(FNode, FNLB),
             FNLB =< GlobalLB),
            FExpandableNodes),
    stateRegister_backwardNodes(StateRegister, BackwardNodes),
    findall(BNode,
            (member(BNode, BackwardNodes),
             node_status(BNode, open),
             node_lb(BNode, BNLB),
             BNLB =< GlobalLB),
            BExpandableNodes),
    append(FExpandableNodes, BExpandableNodes, ExpandableNodes)    
.

:- begin_tests(expandableNodes).
test(eN_1) :-
    tmake_SR4(SR4),
    expandableNodes(SR4, 2, Nodes),
    findall(State,
           (member(Node, Nodes),
            node_state(Node, State)),
           [d,a,b]).


:- end_tests(expandableNodes).
/*
emptyOpenList(StateRegister, Direction) :-
    (    (Direction = forward
         openList(forward, StateRegister, [])
    ;    Direction = backward
         openList(backward, StateRegister, [])
    ).
emptyOpenList(StateRegister, backward) :-
    openList(backward, StateRegister, []).
*/

emptyOpenList(StateRegister, _Direction) :-
    %% \+( (\+(openList(forward, StateRegister, [])),
    %%     \+(openList(backward, StateRegister, [])))
    %%   ).
    %% once(   openList(forward, StateRegister, [])
    %%     ;
    %%         openList(backward, StateRegister, [])
    %%     ).
    openList(forward, StateRegister, [])
    ;
    (\+(openList(forward, StateRegister, [])),
     openList(backward, StateRegister, [])).


/*
emptyOpenList(StateRegister, Direction)  :-
    once((   Direction = forward,
            openList(forward, StateRegister, [])
        ;
            Direction = backward,
            openList(backward, StateRegister, [])
        )).
*/
/* openList(+Direction, +StateRegister, ?OpenList) [semidet]

returns the open nodes from StateRegister in Direction
*/

openList(Direction, StateRegister, OpenList) :-
    once((  Direction = forward *->
            (stateRegister_forwardNodes(StateRegister, Nodes),  
             getOpenList(Nodes, OpenList))
         ;
           Direction = backward *->
           (stateRegister_backwardNodes(StateRegister, Nodes),
           getOpenList(Nodes, OpenList))
        )).
/*
openList(forward, StateRegister, OpenList) :-
    stateRegister_forwardNodes(StateRegister, Nodes),  %% det
    getOpenList(Nodes, OpenList).

openList(backward, StateRegister, OpenList) :-
    stateRegister_backwardNodes(StateRegister, Nodes),
    getOpenList(Nodes, OpenList).
*/


getOpenList(Nodes, OpenList) :-  %% det
    %!:- getOpenList(+Nodes, -OpenList)  is det.
    findall(Node,
                 (member(Node, Nodes),
                  node_status(Node, open)),
                 OpenList).

:- begin_tests(t_openList).


test(openList1) :-
    tmake_SR2(R),
    openList(forward, R, [])
.

test(openList2) :-
    tmake_SR1(R)
    , openList(forward, R, [P])
    , node_state(P, a)
.

:- end_tests(t_openList).

/*
globalLB(+StateRegister, ?GlobalLB)
    GlobalLB is the lower bound for StateRegister

*/

globalLB(StateRegister, GlobalLB) :-
    stateRegister_globalLB(StateRegister, GlobalLB).


:- begin_tests(globalLB).

test(globalLB_1) :-
    tmake_SR3(R),
    globalLB(R, 4).



:- end_tests(globalLB).


directionalLB(DirectionOpenList, DirectionalLB) :-
    findall(LB,
            (member(Node, DirectionOpenList),
             node_lb(Node, LB)),
            DirectionLBs),
    (DirectionLBs = [] *->
         0 = DirectionalLB
    ;
         min_member(DirectionalLB, DirectionLBs)
    ).

:- begin_tests(directionalLB).

test(directionalLB_1) :-
    tmake_SR2(SR2),
    stateRegister_forwardNodes(SR2, FOL),
    directionalLB(FOL, 1).

test(directionalLB_2) :-
    tmake_SR1(R),
    stateRegister_backwardNodes(R, BOL),
    directionalLB(BOL, 2).


:- end_tests(directionalLB).

%% nodeLB(Node, StateRegister, LB)
%%     node_direction(Node, Direction),
%%     oppositeDirection(Direction, OppositeDirection),
%%     openList(OppositeDirection, StateRegister, OpenList),
%%     softFailure(SoftFailure),
%%     lbAux(OpenList, Node, SoftFailure, LB).  %% assuming min LB starts
%% out as softFailure and lbAux keeps lowering it

%% nodeLB(+Node, +StateRegister, -LB) 

nodeLB(Node, StateRegister, LB) :-
    node_direction(Node, Direction),
    oppositeDirection(Direction, OppositeDirection),
    openList(OppositeDirection, StateRegister, OpenList),
    node_state(Node, StateA),
    softFailure(SoftFailure),
    findall(LBAUX,
            ((Direction = forward *->
                  lb(StateA, StateB, LBAUX)
             ;
                  lb(StateB, StateA, LBAUX)
             ),
             member(OppNode, OpenList),
             node_state(OppNode, StateB)
            ),
            LBs),
    (LBs = [] *->
         LB = SoftFailure
    ;
         min_list(LBs, LB)).


/*
lbAux(+OppositeOpenList, +NodeThisSide, +LBIn, -LBOut) [det]
though technically (because of the predicates used) it is only semidet,
it is semantically det, because given a Node, it should always
be able to compute its lower bound (LB)

compute the least lower bound for NodeThisSide when pairing up
with nodes from OppositeOpenList using accumulator pair
(LBIn, LBOut).
Assuming in lb(N, M), N is always going forward and M is going
backward.
*/

lbAux([], _Node, LB, LB).
lbAux([OpenNode | OpenList], Node, LBIn, LBOut) :-
    node_state(OpenNode, OpenState),
    node_state(Node, NodeState),
    once(((   node_direction(Node, forward),
              lb(NodeState, OpenState, LBVal)
          ;
              node_direction(Node, backward),
              lb(OpenState, NodeState, LBVal)
          ),
          (   LBVal < LBIn,
              NewLBIn = LBVal
          ;
              LBVal >= LBIn,
              NewLBIn = LBIn
          ))),
    lbAux(OpenList, Node, NewLBIn, LBOut)
.
    
:- begin_tests(lbt).
test(lb1) :-
    tmake_SR11(SR),
    stateRegister_forwardNodes(SR, [Node | _Nodes]),
    nodeLB(Node, SR, 1)
.

:- end_tests(lbt).

:- begin_tests(lbtt).
test(lb2) :-
    tmake_SR11(SR),
    stateRegister_forwardNodes(SR, [_Node1, Node | _Nodes]),
    nodeLB(Node, SR, 2)
.
    
              
:- end_tests(lbtt).

:- begin_tests(lbt3).
test(lb2) :-
    tmake_SR11(SR),
    stateRegister_backwardNodes(SR, [Node | _Nodes]),
    nodeLB(Node, SR, 1)
%%    , nl, writeln(Node),
%%    nl, writeln(LB)
.
    
              
:- end_tests(lbt3).

:- begin_tests(lbt4).
test(lb2) :-
    tmake_SR11(SR),
    stateRegister_forwardNodes(SR, [_N1, _N2, _N3, Node | _Nodes]),
    nodeLB(Node, SR, 3)
%%    , nl, writeln(Node),
%%    nl, writeln(LB)
.
    
               
:- end_tests(lbt4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  - node . pl  */


/*
node Internal Data Structure

let node have format
node(ExpansionNumber, State, Direction,  Status, Parent, G, H, LB, LBExpandedAt)

most fields are self-explanatory except for PairedWithForLB,
this is the state/node on the opoosite open list that this node
paired up with to create this node's lb, we used this to detect which nodes
on the other side need to be re-calculated when this node is expanded

node API
=======================

expandNode(+ParentNode, -ChildrenStates)
    returns list of all children states connected ParentNode's state

openNodes(+ListOfNodes, -ListOfOpenNodes)
    returns true if ListOfOpenNodes is empty
    returns true if the list of all open nodes in ListOfNodes
    == ListOfOpenNodes
    ** problem check what happens if ListOfOpenNodes is already instantiated but is not
       in the same order as they occur in ListOfNodes
       I think I need to think about this *****************************
*/

:- record node(expansionNumber="nexp", state, direction=forward,
               status = open, parent=nil,
               g:integer, h:integer=0,
               lb, lbExpandedAt).

expandNode(ParentNode, ChildrenStates) :-
    node_direction(ParentNode, forward),
    node_state(ParentNode, ParentState),
    findall(ChildState,
          edge(ParentState, ChildState, _ChildCost),
          ChildrenStates).


expandNode(ParentNode, ChildrenStates) :-
    node_direction(ParentNode, backward),
    node_state(ParentNode, ParentState),
    %%    findall(child(ChildState, ChildCost),
    findall(ChildState,
          edge(ChildState, ParentState, _ChildCost),
          ChildrenStates).

:- begin_tests(expandNode).

test(expandNode_1) :-
    tmake_SR3(SR),
    stateRegister_forwardNodes(SR, [M | _FNs]),
%%    member(M, FNs),
    expandNode(M, [b,c]).
    
test(expandNode_2) :-
    tmake_SR3(SR),
    stateRegister_forwardNodes(SR, FNs),
    last(FNs, M),
    expandNode(M, [c]).

test(expandNode_3) :-
    tmake_SR3(SR),
    stateRegister_backwardNodes(SR, BNs),
    last(BNs, M),
    expandNode(M, [a,b]).

:- end_tests(expandNode).


openNodes([], []).

openNodes(ListOfNodes, ListOfOpenNodes) :-
    ListOfNodes \== [],
    findall((Node),
          (member(Node, ListOfNodes),
           node_status(Node, open)),
           ListOfOpenNodes).


childGValue(Direction, ChildState, ParentNode, ChildG) :-
    node_direction(ParentNode, Direction),
    node_g(ParentNode, PG),
    node_state(ParentNode, PS),
    (   Direction == forward,
        edge(PS, ChildState, EdgeCost)
    ;
        Direction == backward,
        edge(ChildState, PS, EdgeCost)
    ),
    ChildG is PG + EdgeCost.

:- begin_tests(childGValue).

test(childGValue1) :-
    make_node([state(d), status(open), direction(forward), g(1), lb(2)],
              D),
 %%   make_node([state(e), status(closed), direction(forward), g(1), lb(4)],
 %%             E),
    childGValue(forward, f,D, 2)
 %%   writeln(ChildG)
.

:- end_tests(childGValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  - StateRegs . pl

tmake_SR1(SR1) :-
    make_node([state(a), g(0), lb(1)], A),
    make_node([state(b), status(closed), g(2), parent(a), lb(3)], B),
    make_node([state(c), direction(backward), g(1), lb(2)], C),
    make_stateRegister([forwardNodes([A,B]), backwardNodes([C])], SR1).

tmake_SR2(SR2) :-
    make_node([state(a), status(closed), g(0), lb(1)], A),
    make_node([state(b), status(closed), g(2), parent(a), lb(3)], B),
    make_node([state(c), status(closed), direction(backward), g(1), lb(2)], C),
    make_stateRegister([forwardNodes([A,B]), backwardNodes([C])], SR2).

tmake_SR3(SR3) :-
    make_node([state(a), status(open), g(0), lb(1)], A),
    make_node([state(b), status(closed), g(2), parent(a), lb(3)], B),
    make_node([state(c), status(open), direction(backward), g(1), lb(4)], C),
    make_stateRegister([forwardNodes([A,B]), backwardNodes([C])], SR3).

tmake_SR4(SR4) :-
    make_node([state(c), status(closed), direction(backward), g(1), lb(2)], C),
    make_node([state(b), status(open), direction(backward), g(1), lb(2)], B),
    make_node([state(a), status(open), direction(backward), g(1), lb(2)], A),
    make_node([state(d), status(open), direction(forward), g(1), lb(2)], D),
    make_node([state(e), status(closed), direction(forward), g(1),
               lb(4)], E),
    softFailure(SoftFailure),
    make_stateRegister([forwardNodes([D,E]),
                        backwardNodes([A,B,C]),
                        lowestSolutionCostSoFar(SoftFailure)],
                       SR4)
.

tmake_SR5(SR5) :-
    make_node([state(c), status(closed), direction(backward), g(1), lb(2)], CB),
    make_node([state(b), status(open), direction(backward), g(0), lb(2)], B),
    make_node([state(a), status(open), direction(backward), g(1), lb(2)], A),
    make_node([state(d), status(open), direction(forward), g(1), lb(2)], D),
    make_node([state(e), status(closed), direction(forward), g(1),
               lb(4)], E),
    make_node([state(c), g(1)], CF),
    make_stateRegister([forwardNodes([D,E, CF]),
                        forwardCollisionNode(CF),
                        backwardNodes([A,B,CB]),
                        backwardCollisionNode(CB),
                        lowestSolutionCostSoFar(2)],
                       SR5).

tmake_SR11(SR11) :-
    make_node([state(a), direction(forward), g(0), parent(nil)], A),
    make_node([state(b), direction(forward), g(1), parent(a)], B),
    make_node([state(c), direction(forward), g(1), parent(a)], C),
    make_node([state(f), direction(backward), g(0), parent(nil)], F),
    make_node([state(e), direction(backward), g(1), parent(f)], E),
    make_node([state(d), direction(forward), g(2), parent(c)], D),
    make_stateRegister([forwardNodes([A,B,C,D]),
                        backwardNodes([F,E])], SR11).


tmake_SR12(SR12, C) :-
    make_node([state(a), status(open), g(0), lb(1)], A),
    make_node([state(b), status(closed), g(2), parent(a), lb(3)], B),
    make_node([state(c), status(open), direction(backward), g(1), lb(4)], C),
    make_stateRegister([forwardNodes([A,B]), backwardNodes([])], SR12).


tmake_SR13(SR13, ChildrenStates, ParentNode) :-
    
    make_node([state(f), direction(backward), status(open), g(0), lb(1)],
              ParentNode),
    make_node([state(b), direction(backward), status(closed), g(2),
               parent(a), lb(3)], B),
    make_node([state(a), status(open), direction(backward), g(1), lb(4)], _C),
    expandNode(ParentNode, ChildrenStates),
    make_stateRegister([forwardNodes([ParentNode,B]), backwardNodes([])], SR13).


tmake_SR14(SR14, ChildrenStates, ParentNode) :-
    make_node([state(a), status(open), g(0), lb(1)],
              ParentNode),
    expandNode(ParentNode, ChildrenStates),
    make_stateRegister([forwardNodes([ParentNode]), backwardNodes([ParentNode])], SR14).



/*    Version History   */

/*


  total version 4 revision 2
------------------------------

Domain(pancake)
Size(4-pancakes)
Problem(p2)
Heuristic(gap)
OptimalCost(cost)

  total version 4 revision 0
------------------------------

This version sees the addition of code to count search events

adds makeStatList, incStatCount, and fetchStatRec

- will eventually need to extend code to run multiple problems, right 


  total version 3 revision 3
------------------------------

the main improvement is that all optimal collisions are collected, but
only one solution will be returned, namely the first one (because this
is what I would expect standard algorithms to do).


  total version 3 revision 2
------------------------------
instead of returning closed list as "lb when expanded" and state, now
returning "nth node expanded", then "lb when expanded" and finally state


  total version 3 revision 1 branch a
-------------------------------------
a little tidy up
but mainly correcting the collisionUpdate code to look for the collision state in
the opposite open list and putting the right (i.e., forward) state into collisionStates
in the stateRegister


 total version 3 revision 1
 ------------------------------
3.0 doesn't correctly handle multiple optimal solutions found when C < C*
instead of having forward and backward collision nodes, we are adding collisionStates to stateRegister

we will need to extract the solutions from these collision states

these mods will come in steps:
- change the "solution" argument to be "solutions" as we will now b e getting a
list of solutions back "extractSolutions"


because 3.1 has removed the directional collion node pairs, it no longer handles
the problem files from Pat where each state in the problem space has both a
forward state and a backward state (e.g., "f1" and "b45").  Pat's probems have
"oppsite" clauses which tie the two search space states to their forward search
space states, e.g., both "f1" and "b45" to "f1".  The code for this is:

(opposite(ChildState, BackwardState)
     *-> CollisionState = BackwardState
     ;   CollisionState = ChildState)
    
It is found in several places in the code.  However, a "dynamic opposite2." will
be needed in  files where there are no actual opposite/2 definitions.

====================

 total version 3 revision 0
 ------------------------------
modified the load file so that you need to set the problemFile predicate there to
the problem file name, later when we want to do multiple problems, we can change that
to a problemFiles predicate with a list of file names.

 total version 2 revision 12

now returns list of Sz/LCL's where Sz is the size of the closed list LCL, which
has alist of the closed lists with that size

do statistics and data display

the basics seem to be working with no duplicate closed lists

total version 2 revision 11

fixed the last duplicate, it was lb which wasn't determinate, so wrapped a once around it in total

also got the output from a single run into pat's format


total version 2 revision 10

31 July 2023  Get 1 complete set of answers and then a repeat so, problem is that solutionEpisode is being repeated, need to look into this more but first need to do statisitics, etc


still get twice as many solution/closed lists as I should, so want to get
rid of that so that the stats will be right



total version 2 revision 9

looking for why so many duplicates, e.g., for p3f2e, bagof
solutionEpisode produces 288 solutions/closed lists.

revising lbAux/4/4 to be enclosed by once/21, caused
bagof p3f2e to only produce 56 solutions/closed lists, setof produces
28, which is the same as Pat's code

revising openList/3 to be det made no difference at all as far as I can tell



total version 2 revision 8

found the bug: the guard condition for the 3rd clause for solutionEpisodeAux had a
=< while the 2nd clause had a >= and so the 2nd and 3rd clauses were not mutually
exclusive so after fidnig it had a solution in the 2nd clause it would then backtrack
to the 3rd clause to extend that solution

still having same problem

total: version 2 revision 7

23/7/23 17:25

not getting duplicate paths, what seems to be happening is that
it goes beyond finding an optimal solution on backtracking

found case where on 3rd  node expanded have a solution, f exp,
d exp,  a exp, and have soltn path a,c,d,f, but after that, it
goes on to expand b and then return same solution

changing checkbug1 to trace from 1st time that solution is found

22/7/23 
hunting down why getting duplicate paths thru the search space, i.e.,
the sequence that nodes are expanded




total: version 2 revision 6

problem is that it seems to return an infinite number of solutions
in prob2, it turns out that bagof returns 284 solutions and setof returns
30 solutions.




total: version 2 revision 5

now it seems to always return correct solution

returns correct first solution but then on ";", it should find another
way to find that same solution but instead it returns a non-solution
the problem was that on backtracking the extract solution was trying
to get rid of one of the two collision nodes, but got it wrong


total: version 2 revision 4

insufficiently instantiated argument error
the problem was in inStateRegister, the head has InNode and
the body uses Node, changed InNode to Node, and that seems
to have fixed it

total: version 2 Revision 3

made if's more backtrackable

changed test for expandable from lb == globalLB to lb =< globalLB

& misc other changes

 total: version 2 Revision 2

all 40 tests in total now succeed

  total:  version 2 Revision 1

fixed tests for stateRegisterRemoveNode

    total:  version 2 base

the new feature in version 2 is that the solution will now be returned
from solutionEpisodeAux/?  in version 1, the idea was that we were
just interested in the statistics (which we weren't collecting yet).
but to do debugging, it makes sense to also retutn the solution
on individual episodes

since we won't know if a solution is optimal until it is guaranteed
to be optimal, and we can discover this long after the collision
occurs, we need to save not only the current
LowestSolutionCostSoFar but also what the forward and the
backward nodes that collided

In order to avoid adding even more arguments to the call, we will
put the nodes into the stateRegister and while we're at it, we
will put in LowestSolutionCostSoFar, that will get rid on 4
arguments.


*/
