import itertools
import math

from pybound.exceptions import ProblemFormatError
from rust_bindings import unit_cyclic_manhattan

from .domain import Domain
from .tile import count_inversions


class CyclicTileDomain(Domain):
    def __init__(self, initial, goal):
        self.epsilon_global = 1
        self.iota_global = 1
        initial = tuple((int(loc) % 3, int(loc) // 3) for loc in initial)
        goal = tuple((int(loc) % 3, int(loc) // 3) for loc in goal)
        super().__init__(initial, goal)

    def enumerate(size: int):
        problems = []
        goal = "801234567"
        for p in itertools.permutations((8, 0, 1, 2, 3, 4, 5, 6, 7), r=len(goal)):
            if count_inversions(p) % 2 != 0:
                continue
            problems.append(("".join(map(str, p)), goal))
        return problems

    def actions(self, state):
        return ["LEFT", "RIGHT", "DOWN", "UP"]

    def result(self, state, action):
        match action:
            case "LEFT":
                new_blank_position = ((state[0][0] - 1) % 3, state[0][1])
            case "RIGHT":
                new_blank_position = ((state[0][0] + 1) % 3, state[0][1])
            case "DOWN":
                new_blank_position = (state[0][0], (state[0][1] + 1) % 3)
            case "UP":
                new_blank_position = (state[0][0], (state[0][1] - 1) % 3)

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


class CyclicTileDomainUnit(CyclicTileDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        return c + 1


class CyclicTileDomainArbitrary(CyclicTileDomain):
    def path_cost(self, c, state1, action, state2) -> int:
        new_position_of_blank_postion = state2[0]
        swapped_tile = state1.index(new_position_of_blank_postion)
        return c + swapped_tile


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - -  Heuristics


def cyclic_manhattan_distance(currentLocation, goalLocation):
    return (currentLocation[0] - goalLocation[0]) % 3 + (
        currentLocation[1] - goalLocation[1]
    ) % 3


def cyclic_manhattan_unit(node, state_2):
    if type(state_2) is not tuple:
        state_2 = state_2.state
    try:
        state_1 = node.state
    except AttributeError:
        state_1 = node

    return unit_cyclic_manhattan(state_1, state_2)


def cyclic_manhattan_arbitrary(node, goal_state):
    if type(goal_state) is not tuple:
        goal_state = goal_state.state
    try:
        state = node.state
    except AttributeError:
        state = node
    h = 0
    for i in range(1, len(state)):  # skip the blank tile
        h += i * cyclic_manhattan_distance(state[i], goal_state[i])
    return h
