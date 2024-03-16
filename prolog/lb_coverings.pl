:- use_module(library(clpz)).
:- use_module(library(atts)).
:- use_module(library(assoc)).
:- use_module(library(reif)).
:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(pairs)).
:- use_module(library(lambda)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(si)).

:- attribute
        state/2,
        index/1,
        terminal/1,
        parents/1,
        lb_partners/1,
        expanded/1,
        expansion_level/1,
        expansion_time/1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Problem parsing

parse_problem_description(
    description(Cstar, [I, G], FwSccs, BwSccs, LBs, Solutions), 
    Vs, 
    Assoc,
    Cstar, 
    CollisionStates
) :-
    % Get collision states by concatenating Solutions and removing duplicates
    append(Solutions, AppendedSols), sort(AppendedSols, CollisionStates),

    % If the opposite terminal is included in successors, remove it
    del_vertices(FwSccs, [G], FwSccsNG),
    del_vertices(BwSccs, [I], BwSccsNI),

    % Create fresh variables for each directional state
    state_vars_and_assoc(FwSccsNG, BwSccsNI, FwStates, BwStates, Vs, Assoc),

    % Assign indexes to the states
    attach_indexes(Vs, FwStates, BwStates),

    % Label the terminals
    terminals(Assoc, I, G),
    
    % Add parent information
    transpose_ugraph(FwSccsNG, FwParents), 
    transpose_ugraph(BwSccsNI, BwParents),
    maplist(attach_parents(fw, Assoc), FwParents),
    maplist(attach_parents(bw, Assoc), BwParents),

    % Add lower bound information. Format is an associative array per var, LB-[Partners]
    attach_lower_bounds(Assoc, LBs).


state_vars_and_assoc(FwSuccessors, BwSuccessors, FwStates, BwStates, Vs, Assoc) :-
    successors_to_states(FwSuccessors, FwStates), 
    successors_to_states(BwSuccessors, BwStates),
    maplist(state_var_pair(fw), FwStates, VFs, PFs),
    maplist(state_var_pair(bw), BwStates, VBs, PBs),
    append([VFs, VBs], Vs), append([PFs, PBs], Ps),
    list_to_assoc(Ps, Assoc).

successors_to_states(Successors, States) :-
    pairs_keys_values(Successors, Parents, Childrens), append(Childrens, Children),
    append([Parents, Children], StatesWithDuplicates),
    list_to_set(StatesWithDuplicates, UnsortedStates),
    sort(UnsortedStates, States).

state_var_pair(Direction, S, V, SD-V) :- SD = state(S,Direction), put_atts(V, SD).

attach_indexes(Vs, FSs, BSs) :-
    length(FSs, MaxFwState), BwIndexOffset #= MaxFwState + 1,
    list_max(BSs, MaxBwState),
    maplist(attach_index(BwIndexOffset, MaxBwState), Vs).


attach_index(BwIndexOffset, MaxBwState, V) :-
    get_atts(V, state(S, D)),
    attach_index_(D, S, V, BwIndexOffset, MaxBwState).

attach_index_(fw, S, V, _, _) :- put_atts(V, index(S)).
attach_index_(bw, S, V, Offset, MaxState) :-
    Index #= Offset + (MaxState - S),
    put_atts(V, index(Index)).


terminals(Assoc, Initial, Goal) :-
    get_assoc(state(Initial,fw), Assoc, IV),
    put_atts(IV, terminal(1)),
    get_assoc(state(Goal,bw), Assoc, GV),
    put_atts(GV, terminal(1)).

attach_parents(Direction, Assoc, S-Ps) :-
    get_assoc(state(S, Direction), Assoc, VS),
    maplist(get_state(Direction), Ps, PSs),
    maplist(get_assoc_(Assoc), PSs, VPs),
    put_atts(VS, parents(VPs)).

v_lb_partners(V, LBsV) :- 
    (
        (   get_atts(V, lb_partners(LBsV))) -> true
        ;   LBsV = []
    ).

attach_lower_bounds(Assoc, LBs) :-
    maplist(attach_lower_bound(Assoc), LBs),
    map_assoc(lower_bound_list_to_assoc, Assoc).

attach_lower_bound(Assoc, lb(FwName, BwName, LB)) :-
    get_assoc(state(FwName,fw), Assoc, VF), v_lb_partners(VF, LBsVF),
    get_assoc(state(BwName,bw), Assoc, VB), v_lb_partners(VB, LBsVB),
    put_atts(VF, lb_partners([LB-VB|LBsVF])),
    put_atts(VB, lb_partners([LB-VF|LBsVB])).

lower_bound_list_to_assoc(V) :-
    get_atts(V, lb_partners(LBsList)),
    keysort(LBsList, LBsListSorted),
    group_pairs_by_key(LBsListSorted, PartnersByLB),
    list_to_assoc(PartnersByLB, LBsAssoc),
    put_atts(V, lb_partners(LBsAssoc)).

get_state(Direction, S, state(S,Direction)).

get_assoc_(Assoc, Key, Value) :- get_assoc(Key, Assoc, Value).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

num_to_dom(N, D0, D0\/N) :- indomain(N).

% fds_to_dom(N, D0, D0\/D1) :- fd_dom(N, D1).

% expanded(=,_,[],[],0) :- write(111).
% expanded(Vs, Expandeds, PsDoms, N) :-
%     % N #> 0,
%     *same_length(Vs, Includeds),
%     *Includeds ins 0..1,
%     % sum(Includeds, #=, N),
%     *maplist(index_cardinality_pair, Vs, Includeds, IndexIncludedPairs),
%     *length(Expandeds, N),
%     *global_cardinality(Expandeds, IndexIncludedPairs),
%     *chain(#<, Expandeds),
%     *maplist(parent_var, Vs, Ps),
%     *maplist(mul, Ps, Includeds, PsDoms).

% closed_parentage_expansions(D, Expandeds, Ps) :-
%     parse_problem_description(D, Vs, _, _, _),

%     max_index(Vs, MaxIndex),
%     length(Vs, NVars), 
%     ExpandedCount in 1..NVars,
%     write(1),nl,nl,
%     indomain(ExpandedCount),
%     length(Expandeds, ExpandedCount),
%     Expandeds ins 1..MaxIndex,
%     all_distinct(Expandeds),
%     chain(#<, Expandeds),

%     *directional_vs(Vs, FVs, BVs),
%     *length(FVs, NFVars), *length(BVs, NBVars),
%     *ExpandedCountFw in 0..NFVars, *ExpandedCountBw in 0..NBVars,
%     *ExpandedCount #= ExpandedCountFw + ExpandedCountBw,
%     *indomain(ExpandedCountFw),

%     % indomain(ExpandedCountFw),
%     % zcompare(CF, ExpandedCountFw, 0), zcompare(CB, ExpandedCountBw, 0),
%     *expanded(FVs, ExpandedsFw, PsFw, ExpandedCountFw),
%     *expanded(BVs, ExpandedsBw, PsBw, ExpandedCountBw),

%     *append([ExpandedsFw, ExpandedsBw], Expandeds),
%     *append([PsFw, PsBw], Ps),

%     *foldl(num_to_dom, Expandeds, 0, ExpandedsIndexDom),
%     *Ps ins ExpandedsIndexDom.





closed_parentage_expansions2(D, Expandeds, Ps) :-
    parse_problem_description(D, Vs, _, _, _),
    length(Vs, NVars),
    ExpandedCount in 1..NVars,
    indomain(ExpandedCount),
    length(Expandeds, ExpandedCount),
    chain(#<, Expandeds),
    closed_parentage_expansions_(Vs, ExpandedCount, Expandeds, Ps).

closed_parentage_expansions_(Vs, NEx, Exps, Ps) :-
    same_length(Vs, Includeds),
    Includeds ins 0..1,
    sum(Includeds, #=, NEx),
    maplist(index_cardinality_pair, Vs, Includeds, IndexIncludedPairs),
    global_cardinality(Exps, IndexIncludedPairs),
    chain(#<, Exps),

    maplist(parent_var, Vs, Ps_),
    maplist(mul, Ps_, Includeds, Ps),
    foldl(num_to_dom, Exps, 0, ExpsIndexDom),
    Ps ins ExpsIndexDom.


    % max_index(Vs, MaxIndex),
    % length(Expandeds, ExpandedCount),
    % Expandeds ins 1..MaxIndex,
    % all_distinct(Expandeds),
    % chain(#<, Expandeds),

    % same_length(Vs, Includeds),
    % Includeds ins 0..1,
    % maplist(index_cardinality_pair, Vs, Includeds, IndexIncludedPairs),
    % global_cardinality(Expandeds, IndexIncludedPairs),

    % maplist(parent_var, Vs, Ps_),
    % maplist(mul, Ps_, Includeds, Ps),
    % write(Expandeds),nl,
    % foldl(num_to_dom, Expandeds, 0, ExpandedsIndexDom),
    % Ps ins ExpandedsIndexDom.


closed_parentage_expansions(D, Expandeds, Ps) :-
    parse_problem_description(D, Vs, _, _, _),
    max_index(Vs, MaxIndex),
    length(Vs, NVars),
    ExpandedCount in 1..NVars,
    indomain(ExpandedCount),
    length(Expandeds, ExpandedCount),
    Expandeds ins 1..MaxIndex,
    all_distinct(Expandeds),
    chain(#<, Expandeds),

    same_length(Vs, Includeds),
    Includeds ins 0..1,
    maplist(index_cardinality_pair, Vs, Includeds, IndexIncludedPairs),
    global_cardinality(Expandeds, IndexIncludedPairs),

    maplist(parent_var, Vs, Ps_),
    maplist(mul, Ps_, Includeds, Ps),
    write(Expandeds),nl,
    foldl(num_to_dom, Expandeds, 0, ExpandedsIndexDom),
    Ps ins ExpandedsIndexDom.



max_index(Vs, MaxIndex) :-
    maplist(\V^Idx^(get_atts(V, index(Idx))), Vs, Indexes),
    list_max(Indexes, MaxIndex).

% closed_parentage_expansions(D, Solution, Includeds, PsIncluded) :-
%     parse_problem_description(D, Vs, _, _, _),
%     maplist(parent_var, Vs, Ps),
%     directional_vs(Vs, FVs, BVs),


%     length(Vs, NVs), 
%     N in 1..NVs,
%     indomain(N),
%     length(Solution, N),

%     length(Includeds, NVs),
%     Includeds ins 0..1,
%     maplist(index_cardinality_pair, Vs, Includeds, IndexIncludedPairs),
%     global_cardinality(Solution, IndexIncludedPairs),

%     % solution_contains_parents(Solution, PsIncluded),
%     % Solution = [SI|SIs], 
%     foldl(num_to_dom, Solution, 0, SolutionIndexDom),
%     maplist(mul, Ps, Includeds, PsIncluded),
%     % SolutionIndexDomWithZero = 0\/SolutionIndexDom,
%     PsIncluded ins SolutionIndexDom.

index_cardinality_pair(V, Included, Idx-Included) :-
    get_atts(V, index(Idx)).

mul(A,B,C) :- C #= A * B.

parent_var(V, P) :-
    get_atts(V, parents(ParentVars)),
    if_(
        ParentVars = [],
        P #= 0,
        (
            maplist(\PV^Idx^(get_atts(PV, index(Idx))), ParentVars, [PI0|PIs]),
            foldl(num_to_dom, PIs, PI0, ParentDomain),
            P in ParentDomain
        )
    ).

parents_indexes(Parents, Indexes) :-
    maplist(\P^Idx^(get_atts(P, index(Idx))), Parents, Indexes).

directional_vs(Vs, FVs, BVs) :- 
    directional_vs(Vs, [], [], FVs, BVs).


% directional_vs_(Vs, DVs, Direction) :- forward_state_vars(Vs, [], FVs).

directional_vs([], FVs, BVs, FVs, BVs).
directional_vs([V|Vs], PrevFVs, PrevBVs, FVs, BVs) :-
    get_atts(V, state(_,D)),
    if_(
        D = fw,
        directional_vs(Vs, [V|PrevFVs], PrevBVs, FVs, BVs),
        directional_vs(Vs, PrevFVs, [V|PrevBVs], FVs, BVs)
    ).