:- use_module(library(clpfd)).

% Predicate to generate valid graphs with heuristics
graph(States, Edges, Initial, Goal, Heuristics) :-
    length(States, N),          % Number of nodes in the graph
    length(Heuristics, N),      % Number of heuristics


    % Constraints
    all_different(States),      % Each state must be unique
    Initial in States,          % Initial state must be in the list of states
    Goal in States,             % Goal state must be in the list of states

    % Generate edges as pairs of states
    generate_edges(States, Edges),

    % Additional constraints (optional)
    % You can add more constraints based on your specific requirements

    % Heuristic constraints
    admissible_heuristics(States, Heuristics, Goal),
    consistent_heuristics(States, Edges, Heuristics),

    % Labeling
    labeling([], States),       % Labeling to generate valid states

    % Print the resulting graph and heuristics
    print_graph(States, Edges),
    print_heuristics(Heuristics).

% Predicate to generate edges as pairs of states
generate_edges([], []).
generate_edges([State|States], Edges) :-
    generate_edges(States, Edges1),
    add_edges(State, States, Edges2),
    append(Edges2, Edges1, Edges).

% Helper predicate to add edges for a single state
add_edges(_, [], []).
add_edges(State, [OtherState|States], [edge(State, OtherState)|Edges]) :-
    add_edges(State, States, Edges).

% Predicate to ensure the edges form a connected graph
connected_graph([], _).
connected_graph([Edge|Edges], States) :-
    Edge = edge(State1, State2),
    member(State1, States),
    member(State2, States),
    connected_graph(Edges, States).

% Predicate to check if heuristics are admissible
admissible_heuristics([], [], _).
admissible_heuristics([State|States], [Heuristic|Heuristics], Goal) :-
    heuristic_cost(State, Goal, Cost),
    Heuristic #>= Cost,
    admissible_heuristics(States, Heuristics, Goal).

% Predicate to check if heuristics are consistent
consistent_heuristics([], [], _).
consistent_heuristics([State|States], [Edge|Edges], Heuristics) :-
    Edge = edge(State1, State2),
    heuristic_cost(State, State1, Cost1),
    heuristic_cost(State, State2, Cost2),
    Heuristic1 #= Cost1 + 1,
    Heuristic2 #= Cost2 + 1,
    nth1(Index1, States, State1),
    nth1(Index2, States, State2),
    nth1(Index1, Heuristics, Heuristic1),
    nth1(Index2, Heuristics, Heuristic2),
    consistent_heuristics(States, Edges, Heuristics).

% Example heuristic cost function (replace with your own)
heuristic_cost(State1, State2, Cost) :-
    % Calculate the heuristic cost between State1 and State2
    % Replace this with your own admissible and consistent heuristic function
    Cost #= abs(State1 - State2).

% Print the resulting graph
print_graph(States, Edges) :-
    write("Graph:\n"),
    write("States: "),
    writeln(States),
    write("Edges: ").
   