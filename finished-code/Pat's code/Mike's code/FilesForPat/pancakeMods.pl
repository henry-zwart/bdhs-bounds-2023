/* epsilon is the least edge cost in this problem */

epsilon(1).


:- discontiguous parent/3.
:- discontiguous g/2.
:- discontiguous h/2. 
:- discontiguous d/2.
:- discontiguous opposite/2.
:- discontiguous h_f2f/3.


:- set_prolog_flag(stack_limit, 17_179_869_184 ).

/*  HEURISTICS

here are the  F2E and the F2F heuristics for
pancake domain, only one of them should have currentHeuristic
set to it.
*/
hF2EPancake(State, _, HVal) :-
    h(State, HVal).
:- nb_setval(currentHeuristic, hF2EPancake).

hF2FPancake(ForwardState, BackwardState, HVal) :-
    h_f2f(ForwardState, BackwardState, HVal).
/* :- nb_setval(currentHeuristic, hF2FPancake).  */


/*  LB

lb(N, M, LBVal)

should be using the f2f heuristic,
but just using Pat's recorded lb (lower_bound_pair/3)


lb(N1, N2, LBVal) :
if N1 forward
then max(f_F(N1), f_B(N2), g_F(N1) + g_B(N2) + epsilon)
else max(f_F(N2), f_B(N1), g_F(N1) + g_B(N2) + epsilon)
*/

%%%%:- det(lbPancake/3).
%%%%:- det(node_g/2).
%%%%:- det(node_h/2).
%%%%:- det(epsilon/1).
%%%%%%%%:- det(is/2).
%%%%:- det(max/2).

%%%%% lbPancake(Node1,Node2,LBVal) :- once(lb(Node1,Node2,LBVal)).

%%%%% lbPancake(Node1, Node2, LBVal) :- once(f2e_lower_bound_pair(Node1,Node2,LBVal)).

%% %% lbPancake(Node1, Node2, LBVal) 
%% %%     once((node_g(Node1, N1G),
%% %%           node_h(Node1, N1H),
%% %%           N1F is N1G + N1H,
%% %%           node_g(Node2, N2G),
%% %%           node_h(Node2, N2H),
%% %%           N2F is N2G + N2H,
%% %%           epsilon(Epsilon),
%% %%           (   sameState(Node1, Node2) *->
%% %%               GSum is N1G + N2G 
%% %%           ;
%% %%               GSum is N1G + N2G + Epsilon
%% %%           ),
%% %%           LBVal1 is max(N1F, N2F),
%% %%           LBVal is max(LBVal1, GSum)
%% %%         ))
%% %% .

sameState(Node1, Node2) :-
    node_state(Node1, State1),
    node_state(Node2, State2),
    (   State1 == State2
    ;
        opposite(State1, State2)
    ;
        opposite(State2, State1)
    )
    .



:- nb_setval(currentLB, lbPancake).

/*
%% lbPancake(Node1, Node2, LBVal) :-
%%     (   node_direction(Node1, forward),
%%         lower_bound_pair(Node1, Node2, LBVal)
%%     ;
%%         node_direction(Node1, backward),
%%         lower_bound_pair(Node2, Node1, LBVal)
%%     )
%% .
%% 
*/

/*  LB

lb(N, M, LBVal)

should be using the f2f heuristic,
but just using Pat's recorded lb (lower_bound_pair/3)


lb(N1, N2, LBVal) :
if N1 forward
then max(f_F(N1), f_B(N2), g_F(N1) + g_B(N2) + epsilon)
else max(f_F(N2), f_B(N1), g_F(N1) + g_B(N2) + epsilon)
*/

%% %% lbPancake(Node1, Node2, LBVal) :-
%% %%     node_g(Node1, N1G),
%% %%     node_h(Node1, N1H),
%% %%     N1F is N1G + N1H,
%% %%     node_g(Node2, N2G),
%% %%     node_h(Node2, N2H),
%% %%     N2F is N2G + N2H,
%% %%     epsilon(Epsilon),
%% %%     GSum is N1G + N2G + Epsilon,
%% %%     LBVal1 is max(N1F, N2F),
%% %%     LBVal is max(LBVal1, GSum).

:- nb_setval(currentLB, lbPancake).

edge(FromState, ToState, EdgeCost) :-
    (   nonvar(FromState),
        var(ToState),
        sub_string(FromState,0,1,_,"f"),
        parent(FromState, ToState, EdgeCost)
    ;
        var(FromState),
        nonvar(ToState),
        sub_string(ToState,0,1,_,"b"),
        parent(ToState, FromState, EdgeCost)
    ;
        nonvar(FromState),
        nonvar(ToState),
        (   sub_string(FromState,0,1,_,"f"),
            parent(FromState, ToState, EdgeCost)
        ;
            sub_string(ToState,0,1,_,"b"),
            parent(ToState, FromState, EdgeCost)
        )    
    )
.


%% %% edge(FromState, ToState, EdgeCost) :-
%% %%     (   nonvar(FromState),
%% %%         sub_string(FromState,0,1,_,"f"),
%% %%         parent(FromState, ToState, EdgeCost)
%% %%     ;
%% %%         %% var(FromState),
%% %%         nonvar(ToState),
%% %%         sub_string(ToState,0,1,_,"b"),
%% %%         parent(ToState, FromState, EdgeCost)
%% %%     )
%% %% .

/*
translateIntoTargetState(+Direction, +State, ?TranslatedState)

*/
translateIntoTargetState(forward, State, TranslatedState) :-
    (   sub_string(State, 0,1,_, "f"),
        State = TranslatedState
    ;
        sub_string(State, 0,1,_, "b"),
        opposite(TranslatedState, State)
    )
.
translateIntoTargetState(backward, State, TranslatedState) :-
    (   sub_string(State, 0, 1, _, "f"),
        opposite(State, TranslatedState)
    ;
        sub_string(State, 0, 1, _, "b"),
        TranslatedState = State
    )
.

:- begin_tests(translateIntoTargetState).
/* test cases
1. given forward name want forward name
2. given forward name want backward name
3. given backward name want forward name
4. given backward name want backward name

*/
test(translateIntoTargetState1) :-
    translateIntoTargetState(forward, "f10", "f10").
test(translateIntoTargetState2) :-
    translateIntoTargetState(backward, "f10", "b16").
test(translateIntoTargetState3) :-
    translateIntoTargetState(forward, "b16", "f10").
test(translateIntoTargetState4) :-
    translateIntoTargetState(backward, "b16", "b16").

:- end_tests(translateIntoTargetState).