import math
from abc import abstractmethod

from pybound.exceptions import ProblemFormatError

from .domain import Domain


class TileDomain(Domain):
    def __init__(self, initial, goal):
        self.epsilon_global = 1
        self.iota_global = 1
        initial = tuple((int(loc) % 3, int(loc) // 3) for loc in initial)
        goal = tuple((int(loc) % 3, int(loc) // 3) for loc in goal)
        super().__init__(initial, goal)

    def actions(self, state):
        possible_actions = []
        blank_postion = state[0]
        if blank_postion[0] - 1 >= 0:
            possible_actions.append("LEFT")
        if blank_postion[0] + 1 <= 2:
            possible_actions.append("RIGHT")
        if blank_postion[1] + 1 <= 2:
            possible_actions.append("DOWN")
        if blank_postion[1] - 1 >= 0:
            possible_actions.append("UP")
        return possible_actions

    def result(self, state, action):
        if action == "LEFT":
            new_blank_position = (state[0][0] - 1, state[0][1])
        elif action == "RIGHT":
            new_blank_position = (state[0][0] + 1, state[0][1])
        elif action == "DOWN":
            new_blank_position = (state[0][0], state[0][1] + 1)
        elif action == "UP":
            new_blank_position = (state[0][0], state[0][1] - 1)

        swap_tile_index = state.index(new_blank_position)

        return (
            (new_blank_position,)
            + state[1:swap_tile_index]
            + state[0:1]
            + state[swap_tile_index + 1 :]
        )

    def state_repr(state: str | list[str] | list[int]) -> str:
        n_tiles = len(state)
        side_length = int(math.sqrt(n_tiles))
        if side_length**2 != n_tiles:
            raise ProblemFormatError(
                "Sliding tile representation in invalid. "
                "Number of tiles is not a perfect square."
            )

        tiles = [[None for _ in range(side_length)] for _ in range(side_length)]
        for tile, idx in enumerate(state):
            row = idx // side_length
            col = idx % side_length
            tiles[row][col] = str(tile)

        row_strings = [" ".join(row) for row in tiles]
        return "\n".join(row_strings)


class TileDomainUnit(TileDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        return c + 1


class TileDomainArbitrary(TileDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        new_position_of_blank_postion = state2[0]
        swapped_tile = state1.index(new_position_of_blank_postion)
        return c + swapped_tile


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - -  Heuristics


def manhattan_distance(currentLocation, goalLocation):
    return abs(currentLocation[0] - goalLocation[0]) + abs(
        currentLocation[1] - goalLocation[1]
    )


def manhattan_unit(node, goal_state):
    if type(goal_state) is not tuple:
        goal_state = goal_state.state
    state = node.state
    h = 0
    for i in range(1, len(state)):  # skip the blank tile
        h += manhattan_distance(state[i], goal_state[i])
    return h


def manhattan_arbitrary(node, goal_state):
    if type(goal_state) is not tuple:
        goal_state = goal_state.state
    state = node.state
    h = 0
    for i in range(1, len(state)):  # skip the blank tile
        h += i * manhattan_distance(state[i], goal_state[i])
    return h
