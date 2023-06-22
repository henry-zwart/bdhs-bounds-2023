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

parse_problem_description(description(_, [I, G], FwSccs, BwSccs, LBs), Vs, Assoc) :-
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    Bidirectional search expansion patterns.

    Defining constraints on state expansion (is-expanded, lb-level, time) relative to
    other states, which describe a valid sequence of state expansions in a search
    problem.

    Each directional state has the following domain variables:
        - Expanded: 1 if the state is expanded, 0 otherwise
        - ExpansionLevel: The lb-level at which a state is expanded
            - Domain is the distinct LBs (<= C*) in the state's lb-partners, as well
                as C* + 1
            - Attains the value C* + 1 iff the state is not expanded
        - ExpansionTime: n such that the state is the n'th state expanded
            - Domain is 1..len(states)
            - Deterministic for a set of not-expanded states. Assigned in order of 
                state index. This maintains isomorphism of solutions given the same
                set of expanded nodes


    Constraints include:
        1. ExpansionLevel is C* + 1, iff, Expanded is 0
        2. Not-expanded states' ExpansionTimes are monotonic on state index
        3. ExpansionTime values are distinct
        4. Parentage: Each expanded state is expanded after one of its parents
        5. LB-partners: Each expanded state S with ExpansionLevel = L is expanded before
            some other state T, where lb(S,T) = L
        6. LevelTimeConsistency: For any two states, S and T, if S's ExpansionLevel is 
            less than T's ExpansionLevel, then S's ExpansionTime must be less than
            T's ExpansionTime
        7. LB-graph-covering: For any pair of states S and T with lb(S,T) < C*, at least
            one of S and T is expanded

    Not-yet-implemented:
        - Collisions: Either
            - Any number of collisions below C* and no states expanded at C*, or
            - No collisions below C*, and exactly one collision at C*
    
    Problem instances are phrased so as to be parsed by parse_problem_description/3.

    Attributed variables (library: "atts") are used for efficiency and cleanliness.
    parse_problem_description/3 creates a variable for each states with attributes
    describing:
        - State name and direction
        - State index
        - Whether a state is a terminal
        - A state's parents (a list of the parents' attributed variables)
        - A state's LB-partners (an associative array, LB-[PartnersVars])
    
    During the constraint posting process, we add constrained domain variables 
    (described earlier) to each state. The attributed variables enable efficient
    state attribute lookups during the constraint process, and reduce variable clutter.

    Usage: 
        Access a problem from test_problems.pl, post constraints, and label. 
        For example:
            problem(ProblemName, Description), 
            search_as_constraints(Description, Vs),
            label(Vs).

*/

search_as_constraints(
    Description,
    StatesInfo,
    Vs,
    Expandeds,
    ExpansionLevels,
    ExpansionTimes
) :-
    catch(
        (
            search_as_constraints_(
                Description,
                StatesInfo,
                Vs,
                Expandeds,
                ExpansionLevels,
                ExpansionTimes),
            throw(outputs(Vs, Expandeds, ExpansionLevels, ExpansionTimes))
        ),
        outputs(Vs, Expandeds, ExpansionLevels, ExpansionTimes),
        true
    ).

search_as_constraints_(
    Description, 
    StatesInfo, 
    Vs, 
    Expandeds, 
    ExpansionLevels, 
    ExpansionTimes
) :-
    % Want to lose the atts at end but keep constraints. Maybe define constraints at 
    %   start, and then have everything else in catch.
    Description = description(Cstar,_,_,_,_),
    CstarPlusOne #= Cstar + 1,
    parse_problem_description(Description, SVs, Assoc),

    % Set up domains for each state variable
    attribute_expandeds(SVs, Expandeds),
    attribute_expansion_levels(SVs, CstarPlusOne, ExpansionLevels),
    attribute_expansion_times(SVs, ExpansionTimes),

    

    % ====== Constraints ======
    % 1.
    unexpanded_is_cstar_plus_one(CstarPlusOne, Expandeds, ExpansionLevels),
    % maplist(unexpanded_is_cstar_plus_one(CstarPlusOne), Expandeds, ExpansionLevels),
    % 2.
    unexpandeds_ordered_by_index(SVs),
    % 3. (done at attribution)
    % 4. 
    parentage(SVs),
    % 5.
    lb_partners_are_available(SVs),
    % 6.
    expansion_times_increase_with_levels(ExpansionTimes, ExpansionLevels),
    % 7.
    lower_bound_pairs_covered(SVs, Cstar),
    

    append([ExpansionLevels, ExpansionTimes], Vs),
    maplist(get_atts, SVs, _, StatesInfo).


lower_bound_pairs_covered(Vs, Cstar) :-
    lbs(Vs, LBs, Cstar),
    maplist(lb_covered, LBs).

lbs(Vs, LBs, Cstar) :-
    forward_state_vars(Vs, FVs),
    lbs(FVs, [], LBs, Cstar).

lb_covered(lb(X,Y,LB)) :-
    get_atts(X, expansion_level(XL)),
    get_atts(Y, expansion_level(YL)),
    XL #=< LB #\/ YL #=< LB.



lbs([], LBs, LBs, _).
lbs([V|Vs], PrevLBs, LBs, Cstar) :-
    get_atts(V, lb_partners(LBPartnersAssoc)),
    assoc_to_list(LBPartnersAssoc, LBPartnersPairs),
    tfilter(lb_less_than_cstar(Cstar), LBPartnersPairs, MustExpandLBPartnersPairs),
    if_(
        MustExpandLBPartnersPairs = [],
        lbs(Vs, PrevLBs, LBs, Cstar),
        (
            maplist(lbs_from_pairs(V), MustExpandLBPartnersPairs, VLBss),
            append([PrevLBs|VLBss], NewPrevLBs),
            lbs(Vs, NewPrevLBs, LBs, Cstar)
        )
    ).

lbs_from_pairs(V, LBval-Partners, LBs) :-
    maplist((LBval,V)+\P^LB^(LB=lb(V,P,LBval)), Partners, LBs).
    
lb_less_than_cstar(Cstar, LB-_, T) :-
    (   LB #=< Cstar -> T = true
    ;   T = false
    ).

forward_state_vars(Vs, FVs) :- forward_state_vars(Vs, [], FVs).

forward_state_vars([], FVs, FVs).
forward_state_vars([V|Vs], PreviousFVs, FVs) :-
    get_atts(V, state(_,D)),
    if_(
        D = fw,
        forward_state_vars(Vs, [V|PreviousFVs], FVs),
        forward_state_vars(Vs, PreviousFVs, FVs)
    ).

expansion_times_increase_with_levels([],[]).
expansion_times_increase_with_levels([_],[_]).
expansion_times_increase_with_levels([T0,T1|Ts], [L0,L1|Ls]) :-
    maplist(level_and_time_pair_consistent(T0,L0), [T1|Ts], [L1|Ls]),
    expansion_times_increase_with_levels([T1|Ts], [L1|Ls]).

level_and_time_pair_consistent(T,L,OT,OL) :-
    L #< OL #==> T #< OT,
    OL #< L #==> OT #< T.

unexpanded_is_cstar_plus_one(CstarPlusOne, Expandeds, ExpansionLevels) :-
    maplist(unexpanded_is_cstar_plus_one_(CstarPlusOne), Expandeds, ExpansionLevels).

unexpanded_is_cstar_plus_one_(CstarPlusOne, Expanded, Level) :-
    #\Expanded #<==> Level #= CstarPlusOne.

unexpandeds_ordered_by_index(Vs) :-
    maplist(\V^ID^(get_atts(V, index(ID))), Vs, Indexes),
    pairs_keys_values(Pairs, Indexes, Vs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, VsByID),
    maplist(\V^E^(get_atts(V, expanded(E))), VsByID, Expandeds),
    maplist(\V^T^(get_atts(V, expansion_time(T))), VsByID, Times),
    pairs_keys_values(ExpandedWithTimesByID, Expandeds, Times),
    variables_signature(ExpandedWithTimesByID, Sigs),
    automaton(
        Sigs,
        [source(s), sink(s)],
        [arc(s,1,s)]).

variables_signature([], []).
variables_signature([V|Vs], Sigs) :-
    V = Expanded-Time,
    B #<==> Expanded #= 0,
    PrevMax #= B * Time,
    variables_signature_(Vs, PrevMax, Sigs).

variables_signature_([], _, []).
variables_signature_([V|Vs], PrevMax0, [S|Sigs]) :-
    V = Expanded-Time,
    GtLast #<==> #\Expanded #/\ (Time #>= PrevMax0),
    S #<==> Expanded #\/ GtLast,
    NotExpanded #<==> #\ Expanded,
    max(PrevMax0, NotExpanded * Time, PrevMax),
    variables_signature_(Vs, PrevMax, Sigs).

parentage(Vs) :-
    maplist(parentage_, Vs).

parentage_(V) :-
    get_atts(V, parents(Ps)),
    get_atts(V, expansion_time(T)),
    if_(
        Ps = [],
        true,
        (
            maplist(\V^PT^(get_atts(V, expansion_time(PT))), Ps, [PT|PTs]),
            foldl(min, PTs, PT, MinParentTime),
            T #> MinParentTime
        )
    ).

lb_partners_are_available(Vs) :-
    maplist(unexpanded_or_lb_partner_is_available, Vs).

unexpanded_or_lb_partner_is_available(V) :-
    get_atts(V, expanded(Expanded)),
    lb_partner_is_available(V, PartnerAvailable),
    #\Expanded #\/ PartnerAvailable.

lb_partner_is_available(V, B) :-
    get_atts(V, expansion_level(L)),
    get_atts(V, expansion_time(T)),
    get_atts(V, lb_partners(PartnersByLB)),
    assoc_to_list(PartnersByLB, Pairs),
    pairs_keys_values(Pairs, LBs, PartnersLists),
    maplist(l_is_lb_and_partner_available(L,T), LBs, PartnersLists, PerLBConstraints),
    length(PerLBConstraints, N), 
    sum(PerLBConstraints, #=, C), 
    C #= N #<==> B.



    

l_is_lb_and_partner_available(L, T, LB, Partners, B) :-
    maplist(\PV^PT^(get_atts(PV, expansion_time(PT))), Partners, [PT|PTs]),
    foldl(max, PTs, PT, MaxPartnerTime),
    (L #= LB #==> T #=< MaxPartnerTime - 1) #<==> B.


    
attribute_expandeds(Vs, Es) :-
    same_length(Vs, Es),
    Es ins 0..1,
    maplist(\V^E^(put_atts(V, expanded(E))), Vs, Es).

attribute_expansion_levels(Vs, CstarPlusOne, Ls) :-
    same_length(Vs, Ls),
    maplist(level_in_domain(CstarPlusOne), Vs, Ls),
    maplist(\V^L^(put_atts(V, expansion_level(L))), Vs, Ls).

attribute_expansion_times(Vs, Ts) :-
    length(Vs, NumStates), length(Ts, NumStates),
    Ts ins 1..NumStates,
    all_distinct(Ts),                                       % Constraint 3.
    maplist(\V^T^(put_atts(V, expansion_time(T))), Vs, Ts).

level_in_domain(CstarPlusOne, V, L) :-
    get_atts(V, lb_partners(LBGroups)),
    assoc_to_keys(LBGroups, LBs),
    sort([CstarPlusOne|LBs], AllowedLevels),
    maplist(L+\LB^T^(T = (L #= LB)), AllowedLevels, Ts),
    list_to_disj(Ts, Disj),                                 % Leftover choicepoint...
    call(Disj).


list_to_disj([Disj], Disj).
list_to_disj([A,B|Rest], Disj) :- C = (A #\/ B), list_to_disj([C|Rest], Disj).


min(X,Y,Z) :- Z #= min(X,Y).
max(X,Y,Z) :- Z #= max(X,Y).


all_true([B|Bs]) :-
    length([B|Bs], N),
    sum([B|Bs], #=, BSum),
    BSum #= N.

all_true([B|Bs], T) :-
    length([B|Bs], N),
    sum([B|Bs], #=, BSum),
    BSum #= N #<==> T.