:- use_module(library(record)).
:- use_module(library(plunit)).  %% library for unit testing
:- use_module(library(statistics)). %% for doing timings
:- use_module(library(lists)).
:- use_module(library(pldoc)).  %% library for prolog documnetation package
:- use_module(library(assoc)).

:- multifile version/3.
%% version(ProgramBaseName, VersionNumber,RevisionNumber)
version(stats,1,2).

/* stats interface
records:
id
stat
collisionId
collisionStat


preds other than record generated ones:
makeStatList([])
fetchStatRec(+StatList, +Id, -stat(Id, Count))
fetchUniqueStatRec(+StatList, +Id, -stat(Id, Count))
incStatCount(+OldStatList, +N, +Id, -NewStatList)
*/
/*
episode stat code

an episode is a run that finishes when it finds a guaranteed optimal solution

this code collects the stats on the episode

all the stat records are stored in a list, which will be the stat argument
passed around

*/
:- record id(global_lb, action, direction, g, h, lb).
/*
this rec identifies the context in which these counts took place
arguments:
global_lb is the global lower bound in effect when these actions occured
action is the action that occured
direction is the direction of the node
g, h, and lb are the respective values for the nodes


action can be:
generate, ignore, open, expand, reOpen, or reExpand (not implemented)

reExpand cannot be detected as the search is currently implemented,
will need to extend the node data to include a flag for states that
have already been expanded to detect re-expanded states


direction can be:
forward, backward

global_lb, g, h, and lb are all integers
*/

:- record stat(id, count=1).

/*
arguments:
id is the appropriate id rec
count is the count of these actions specified in the id
*/
makeStatList([]).  % creates the empty stat list

/*
fetchStatRec(+StatList, +Id, ?StatRec)
arguments:
StatList is the list of stat recs
Id is an id rec that reflects which stat rec we want
StatRec is a stat rec in StatList with that id

note: that Id need to be only partially instantiated, in which case,
it can backtrack over all the stat recs that unify with that id

fetch can fail if there are no stat recs with a id that unify with ID

*/

fetchStatRec(StatList, Id, stat(Id, Count)) :-
    member(stat(Id, Count), StatList).

/*
fetchUniqueStatRec(+StatList, +Id, -stat(Id, Count))

fetchUniqueStatRec only succeeds if there is exactly stat rec with that id

*/

fetchUniqueStatRec(StatList, Id, stat(Id, Count)) :-
    include(isCorrectStat(Id), StatList, [stat(Id, Count)]).

isCorrectStat(Id, stat(Id, _Count)).

/*

IncStatCount(+OldStatList, +N, ?Id, -NewStatList)

outputs a new StatList whose stat rec in OldStatList with Id,
whose count has been incremented by N

arguments:
OldStatList - obvious
N - is the increment amount
Id - is the id of the stat rec whose count is to be incremented
NewStatList - is the resultant stat list whose count has been incremented

we check that there is only one stat rec whose id unifies with Id
if there is only one
then get the stat rec
     increment its count by N
else create stat rec with count N
create new statList with updated/new stat rec

*/

incStatCount(OldStatList, N, Id, NewStatList) :-
    (   member(stat(Id,_), OldStatList)
    *->
        partition(isCorrectStat(Id), OldStatList, [stat(Id, Count)], Rest),
        NewCount is Count + N,
        NewStatList = [stat(Id, NewCount) | Rest]
    ;
        NewStatList = [stat(Id, N) | OldStatList]
    ).


/*  Tests */
:- begin_tests(stats_1).

test(makeStatList_1) :-
    makeStatList([]).

test(id_1) :-
    make_id([global_lb(3), action(generate), direction(forward),
             g(0), h(2), lb(2)], Id),
    Id == id(3, generate, forward, 0, 2, 2).


test(stat_1) :-
    make_id([global_lb(3), action(generate), direction(forward),
             g(0), h(2), lb(2)], Id),
    make_stat([id(Id)], S),
    S == stat(id(3, generate, forward, 0, 2, 2),1).

test(stat_2) :-
    make_id([global_lb(3), action(generate), direction(forward),
             g(0), h(2), lb(2)], Id),
    makeStatList(StatList1),
    StatList1 == [],
    incStatCount(StatList1, 1, Id, StatList2),
    StatList2 == [stat(id(3, generate, forward, 0, 2, 2),1)].

test(stat_3) :-
    make_id([global_lb(3), action(generate), direction(forward),
             g(0), h(2), lb(2)], Id1),
    make_id([global_lb(3), action(expand), direction(forward),
             g(0), h(2), lb(2)], Id2),
    makeStatList(StatList1),
    StatList1 == [],
    incStatCount(StatList1, 1, Id1, StatList2),
    StatList2 == [stat(id(3, generate, forward, 0, 2, 2),1)],
    incStatCount(StatList2, 1, Id2, StatList3),
    incStatCount(StatList3, 3, Id1, StatList4),
    print(StatList4).

:- end_tests(stats_1).
