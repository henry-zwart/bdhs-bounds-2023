:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(between)).
:- use_module(library(debug)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(atts)).
:- use_module(library(lambda)).


% ======================================================================================
% Data Access

% state_record(st(ID, Direction), Expanded, ExpansionLevel, ExpansionTime)
state_record(State, state_record(State,_,_,_)).
state_record_pair(state_record(State,Exp,EL,ET), State-state_record(State,Exp,EL,ET)).
et_pair(state_record(State,_,_,ExpansionTime), State-ExpansionTime).

% get record fields
record_state(state_record(S,_,_,_), S).
record_state_expanded(state_record(_,Exp,_,_),Exp).
record_expansion_level(state_record(_,_,EL,_),EL).
record_expansion_time(state_record(_,_,_,ET),ET).


% ======================================================================================
% Helpers

% Define min/3 so we can use it in foldl
min(X,Y,Z) :- Z #= min(X,Y).
max(X,Y,Z) :- Z #= max(X,Y).

% Define get_assoc_ so we can use it in maplist with parameterised Assoc
get_assoc_(Assoc, Key, Value) :- get_assoc(Key, Assoc, Value).

% Gets the set of LB values associated with a directional state
state_lbs(S, LBs) :-
    setof(Z, T^lb(S,T,Z), LBs).

% Converts a list of integers into a CLPZ domain
list_to_domain([L|[]], Dom) :- Dom = L.
list_to_domain([L1,L2|Ls], Dom) :-
    list_to_domain([L2|Ls], Dom0),
    Dom = L1 \/ Dom0.

% Retrieves the LB domain for a directional state
% C* + 1 is included in the domain as a "not-expanded" default value for expansion level
state_lb_domain(AboveCstar, S, Dom) :-
    state_lbs(S, LBs),
    list_to_domain(LBs, Dom0),
    Dom = Dom0 \/ AboveCstar.

% Produces a list of states in order of expansion time
records_to_state_order(Records, StateOrder) :-
    maplist(expansiontime_state_pair, Records, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, StateOrder).

expansiontime_state_pair(state_record(State,_,ET), ET-State).

% ======================================================================================
% Constraints

% ================ Parentage =================
% Succeeds if the expansion time of state in Record is greater than at least one of
% it's parents
parent_before(state_record(State,_,_,ExpansionTime), RecordAssoc) :-
    findall(Parent, p(Parent, State), Parents),
    length(Parents, NumParents), NumParents #> 0,
    maplist(get_assoc_(RecordAssoc), Parents, ParentRecords),
    maplist(record_expansion_time, ParentRecords, [Pet|Pets]),
    foldl(min, Pets, Pet, MinParentExpansionTime),
    ExpansionTime #> MinParentExpansionTime.

% Succeeds if parentage constraint holds. RecordAssoc used for quick attribute lookup.
parentage([], _).
parentage([Record|Records], RecordAssoc) :-
    Record = state_record(State,_,_,_),
    (
        parent_before(Record, RecordAssoc)
        ; terminal(State)
    ),
    parentage(Records, RecordAssoc).


% ================ Before some LB partner at LB =================
before_lb_partner_(ExpansionLevel, ExpansionTime, LB-[ET|ETs], T) :-
    foldl(max, ETs, ET, MaxPartnerExpansionTime),
    (ExpansionLevel #= LB #==> ExpansionTime #=< MaxPartnerExpansionTime - 1) #<==> T.


before_lb_partner(Record, RecordAssoc, T) :-
    Record = state_record(State,_,ExpansionLevel,ExpansionTime),
    findall(LB-Partner, lb(State, Partner, LB), LBPartnerPairs),
    pairs_keys(LBPartnerPairs, LBs),
    pairs_values(LBPartnerPairs, Partners),
    maplist(get_assoc_(RecordAssoc), Partners, PartnerRecords),
    maplist(record_expansion_time, PartnerRecords, PartnerExpansionTimes),
    pairs_keys_values(LBETs, LBs, PartnerExpansionTimes),
    group_pairs_by_key(LBETs, ByLB),
    maplist(before_lb_partner_(ExpansionLevel, ExpansionTime), ByLB, Constraints),
    length(Constraints, N),
    sum(Constraints, #=, NTrue),
    NTrue #= N #<==> T.

% From lb-pairs, get a list of records for each unique lb
% For lb_records(LB, Records), say ExpansionLevel #= LB #==> foldl(max, ETS, ET, MaxET), ExpansionTime #< MaxET.
lb_partners([],_).
lb_partners([Record|Records], RecordAssoc) :-
    Record = state_record(_,Expanded,_,_),
    T1 #<==> Expanded #= 0,
    before_lb_partner(Record, RecordAssoc, T2),
    T1 #\/ T2,
    lb_partners(Records, RecordAssoc).


% ================ LB-graph is covered =================
% Ensures that each state is expanded at its lowest possible LB, so there aren't gaps
% in the LB-graph

lb_graph_covered_([],_).
lb_graph_covered_([lb(X,Y,LB)|LBs], RecordAssoc) :-
    maplist(get_assoc_(RecordAssoc), [X,Y], Records),
    maplist(record_expansion_level, Records, ExpansionLevels),
    maplist(LB+\EL^T^(T #<==> EL #=< LB), ExpansionLevels, Constraints),
    sum(Constraints, #>=, 1),
    lb_graph_covered_(LBs, RecordAssoc).


lb_graph_covered(RecordAssoc) :-
    solution_cost(Cstar),
    findall(
        lb(State,Partner,LB), 
        (
            lb(State,Partner,LB),
            LB #=< Cstar,
            State = st(S,_), Partner = st(P,_), S #< P
        ), 
        LBs),
    lb_graph_covered_(LBs, RecordAssoc).


% ================ LB(X) < LB(Y) ==> Time(X) < Time(Y) =================
el_et_consistent_(EL,ET,OtherEL,OtherET,T) :-
    T #<==> (EL #>= OtherEL #\/ ET #< OtherET).

el_et_consistent(Record, RecordAssoc, T) :-
    Record = state_record(_,_,EL,ET),
    assoc_to_values(RecordAssoc, Records),
    maplist(record_expansion_level, Records, AllELs),
    maplist(record_expansion_time, Records, AllETs),
    maplist(el_et_consistent_(EL,ET), AllELs, AllETs, Ts),
    length(Ts, N),
    sum(Ts, #=, TSum),
    TSum #= N #<==> T.

% Might be able to do this better with automata
expansion_levels_and_times_consistent([],_).
expansion_levels_and_times_consistent([Record|Records], RecordAssoc) :-
    Record = state_record(_,Expanded,_,_),
    Expanded #= 0 #<==> T1,
    el_et_consistent(Record, RecordAssoc, T2),
    T1 #\/ T2,
    expansion_levels_and_times_consistent(Records, RecordAssoc).

% ======================================================================================
    
% Constraints the expansion ordering of States to be valid given
%    - Parentage
%    - Lower bound partners
%    - Lower bound level order
%    - LB-graph is covered
ordering(States, Records, Expandeds, ExpansionLevels, ExpansionTimes) :-
    % Get a variable record for each state, and collect lists of fields
    maplist(state_record, States, Records),
    maplist(record_state_expanded, Records, Expandeds),
    maplist(record_expansion_level, Records, ExpansionLevels),
    maplist(record_expansion_time, Records, ExpansionTimes),

    % Create association list for efficient state -> record lookup
    maplist(state_record_pair, Records, RecordPairs),
    list_to_assoc(RecordPairs, RecordAssoc),

    % Each state is either expanded or not expanded
    Expandeds ins 0..1,
    solution_cost_plus_one(AboveCstar),
    
    % A state is not expanded, iff, its expansion level is C* + 1
    maplist(AboveCstar+\B^X^(#\B #<==> X #= AboveCstar), Expandeds, ExpansionLevels),

    % Each state is expanded at one of its lower bounds
    maplist(state_lb_domain(AboveCstar), States, StateLBDomains),
    maplist(in, ExpansionLevels, StateLBDomains),

    % Expansion times must be unique elements in 1..NumStates
    length(States, NumStates),
    ExpansionTimes ins 1..NumStates,
    all_distinct(ExpansionTimes),

    % Constraints
    parentage(Records, RecordAssoc),
    lb_partners(Records, RecordAssoc),
    expansion_levels_and_times_consistent(Records, RecordAssoc),
    lb_graph_covered(RecordAssoc).

    % Canonical ordering:
%     - Get list of states sorted by state ID
%     - Map to pairs of "Expanded-ExpansionTime" sorted by state ID
%     - Run automata over list, keeping track of maximum not-expanded expansion time
%     - If we see a not-expanded with expansion time less than the max, return false
%     - Otherwise, return true
