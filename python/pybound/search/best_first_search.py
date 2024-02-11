from .memoize import memoize
from .node import Node
from .openlist import Open_List


def best_first_search(problem, f):
    f = memoize(f, "f")
    solution_nodes = []

    open_list = Open_List(f)
    closed_list = {}

    node = Node(problem.initial)
    open_list.append(node)

    while open_list:
        node = open_list.pop()

        if solution_nodes and node.g > solution_nodes[0].g:
            break

        closed_list[node.state] = node

        if problem.goal_test(node.state):
            solution_nodes.append(node)
        else:
            for child in node.expand(problem):
                child_state = child.state

                if child_state not in closed_list and child_state not in open_list:
                    open_list.append(child)
                # for the case that there are multiple optimal paths to that node
                elif child_state in closed_list:
                    closed_list_node = closed_list[child_state]

                    if child.g == closed_list_node.g:
                        parent_action_pair = child.parent_action_pairs[0]

                        if (
                            parent_action_pair
                            not in closed_list_node.parent_action_pairs
                        ):
                            closed_list_node.append_parent_action_pair(
                                parent_action_pair
                            )
                elif child_state in open_list:
                    if f(child) < open_list[child_state].f:
                        open_list.decreaseKey(child)
                    elif child.g == open_list[child_state].g:
                        open_list_node = open_list[child_state]
                        parent_action_pair = child.parent_action_pairs[0]
                        if parent_action_pair not in open_list_node.parent_action_pairs:
                            open_list_node.append_parent_action_pair(parent_action_pair)

    return solution_nodes, closed_list
