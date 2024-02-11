import itertools

from .memoize import memoize
from .node import Node


def calculate_d_value(nodeX, g_value_node_x, h_value_node_y):
    return g_value_node_x - h_value_node_y


def calculate_b_value(node):
    if hasattr(node, "d") and hasattr(node, "f"):
        return node.f + node.d
    else:
        raise Exception(
            "One or more of the prerequisite properties have not been calculated."
        )


def d_value(closed_list_forward, closed_list_backward, heuristic, problem):
    # note: memoize only sets the attribute for the object passed as the first argument to the memoized function
    # TODO: check if works when not heuristic is not symmetric
    d = memoize(calculate_d_value, "d")

    for node_forward in closed_list_forward.values():
        if node_forward.state in closed_list_backward:
            node_backward = closed_list_backward[node_forward.state]
            d(node_forward, node_forward.g, node_backward.h)
            d(node_backward, node_backward.g, node_forward.h)
        else:
            temp_h_value = heuristic(node_forward, problem.initial)
            d(node_forward, node_forward.g, temp_h_value)

    for node_backward in closed_list_backward.values():
        if not hasattr(node_backward, "d"):
            temp_h_value = heuristic(node_backward, problem.goal)
            d(node_backward, node_backward.g, temp_h_value)


def b_value(closed_list_forward, closed_list_backward):
    b = memoize(calculate_b_value, "b")

    for node_forward in itertools.chain(
        closed_list_forward.values(), closed_list_backward.values()
    ):
        b(node_forward)


def post_process(closed_list_forward, closed_list_backward, heuristic, problem):
    d_value(closed_list_forward, closed_list_backward, heuristic, problem)
    b_value(closed_list_forward, closed_list_backward)
