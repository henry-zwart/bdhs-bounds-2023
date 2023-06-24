from abc import abstractmethod

from .domain import Domain


class PancakeDomain(Domain):
    def __init__(self, initial, goal):
        self.epsilon_global = 1
        self.iota_global = 1
        table = [len(initial) + 1]
        initial = tuple([int(i) for i in initial] + table)
        goal = tuple([int(i) for i in goal] + table)
        super().__init__(initial, goal)  # assumes the goal state is [1,2,3,...,n]

    def actions(self, state):
        """Returns the index of the pancake that is under the flipper. This pancake will not be flipped. DO NOT MOVE THE TABLE (THE HIGHEST NUM)!"""
        possible_actions = [
            pancake_under_flipper for pancake_under_flipper in range(2, len(state))
        ]
        return possible_actions

    def result(self, state, action):
        """Given an index directly below the flipper flip the stack."""
        state = list(state)
        flipped_portion = state[0:action]
        flipped_portion.reverse()
        return tuple(flipped_portion + state[action:])

    def state_repr(state: str | list[str] | list[int]) -> str:
        pancakes = [f" {x}" for x in state]
        pancake_part = "\n".join(pancakes)
        table_val = len(pancakes) + 1
        table_part = "\n".join(["---", f" {table_val}"])
        return "\n".join([pancake_part, table_part])


class PancakeDomainUnit(PancakeDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        return c + 1


class PancakeDomainArbitrary(PancakeDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        return c + state1[action]


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - -  Heuristics


# def gap_unit_cost_helper(state_1, state_2):
#     # * before degradation in parameters means we must pass degradation as a kewword argument
#     # This is here to avoid errors (i.e. if we forget to set degradation)
#     # In practice it shouldn't make usage any different, since we have aliased the heuristic using functools in main.py

#     heuristic_value = 0

#     for i in range(0, len(state_1) - 1):
#         j = i + 1
#         pancake_j = state_1[j]
#         pancake_i = state_1[i]

#         goal_position_i = state_2.index(
#             pancake_i
#         )  # goal postion is the index of the pancake in the goal state

#         # Test if pancake j is adjacent to pancake i in the state_2
#         if goal_position_i != 0 and state_2[goal_position_i + -1] == pancake_j:
#             continue
#         elif (
#             goal_position_i != len(state_1) - 1
#             and state_2[goal_position_i + 1] == pancake_j
#         ):
#             continue

#         heuristic_value += 1

#     return heuristic_value


def gap_unit_cost_helper(state_1, state_2):
    # * before degradation in parameters means we must pass degradation as a kewword argument
    # This is here to avoid errors (i.e. if we forget to set degradation)
    # In practice it shouldn't make usage any different, since we have aliased the heuristic using functools in main.py

    heuristic_value = 0
    goal_positions = {pancake: i for i, pancake in enumerate(state_2)}
    n_pancakes = len(state_1) - 1

    for i in range(n_pancakes):
        pancake_i = state_1[i]
        pancake_j = state_1[i + 1]

        goal_position_i = goal_positions[
            pancake_i
        ]  # goal postion is the index of the pancake in the goal state

        # Test if pancake j is adjacent to pancake i in the state_2
        if goal_position_i != 0 and state_2[goal_position_i - 1] == pancake_j:
            continue
        elif (
            goal_position_i != n_pancakes and state_2[goal_position_i + 1] == pancake_j
        ):
            continue

        heuristic_value += 1

    return heuristic_value


def gap_arbitrary_cost_helper(state_1, state_2):
    # * before degradation in parameters means we must pass degradation as a kewword argument
    # This is here to avoid errors (i.e. if we forget to set degradation)
    # In practice it shouldn't make usage any different, since we have aliased the heuristic using functools in main.py

    heuristic_value = 0
    for i in range(0, len(state_1) - 1):
        j = i + 1
        pancake_j = state_1[j]
        pancake_i = state_1[i]

        goal_position_i = state_2.index(
            pancake_i
        )  # goal postion is the index of the pancake in the goal state

        # Test if pancake j is adjacent to pancake i in the state_2
        if goal_position_i != 0 and state_2[goal_position_i + -1] == pancake_j:
            continue
        elif (
            goal_position_i != len(state_1) - 1
            and state_2[goal_position_i + 1] == pancake_j
        ):
            continue

        heuristic_value += min(pancake_i, pancake_j)

    return heuristic_value


def gap_unit(node, state_2):
    if type(state_2) is not tuple:
        state_2 = state_2.state
    try:
        state_1 = node.state
    except AttributeError:
        state_1 = node
    return max(
        gap_unit_cost_helper(state_1, state_2),
        gap_unit_cost_helper(state_2, state_1),
    )


def gap_arbitrary(node, state_2):
    # * before degradation in parameters means we must pass degradation as a kewword argument
    # This is here to avoid errors (i.e. if we forget to set degradation)
    # In practice it shouldn't make usage any different, since we have aliased the heuristic using functools in main.py
    if type(state_2) is not tuple:
        state_2 = state_2.state
    try:
        state_1 = node.state
    except AttributeError:
        state_1 = node
    return max(
        gap_arbitrary_cost_helper(state_1, state_2),
        gap_arbitrary_cost_helper(state_2, state_1),
    )
