from genericpath import exists

from pybound.search.a_star_search import a_star_search
from pybound.search.node import Node
from pybound.search.post_process_b_and_d_values import post_process

from .utils import serialize

# from ..clause_generation_helper.utils import get_node_values


def search(
    domain, heuristic, problem: str  # , search_results_path: str, search_data_path: str
):
    # if not exists(search_results_path):
    initial_state, goal_state = problem.split("_")

    problem_f = domain(initial_state, goal_state)
    problem_b = domain(goal_state, initial_state)

    solution_nodes_f, closed_list_f = a_star_search(problem_f, heuristic)
    solution_nodes_b, closed_list_b = a_star_search(problem_b, heuristic)

    max_node_id = Node.count
    Node.count = 1  # The counter must be reset for every (intail_state, goal_state) pair for correct and efficient encoding

    epsilon_global = problem_f.epsilon_global
    iota_global = problem_f.iota_global

    post_process(closed_list_f, closed_list_b, heuristic, problem_f)

    # for node in closed_list_f.values():
    #     print(node.state,node.id, get_node_values(node,0, 0, False))

    # print()

    # for node in closed_list_b.values():
    #     print(node.state,node.id, get_node_values(node, 0, 0, False))

    solution_cost = solution_nodes_f[0].g
    solution_length = solution_nodes_f[0].solution_length()

    # serialize(
    #     [
    #         max_node_id,
    #         epsilon_global,
    #         iota_global,
    #         solution_nodes_f,
    #         solution_nodes_b,
    #         closed_list_f,
    #         closed_list_b,
    #     ],
    #     search_results_path,
    # )

    # else:
    #     with open(search_data_path, "r") as f:
    #         line = f.readline()
    #         solution_length, solution_cost, max_node_id = [
    #             int(item) for item in line.split(",")
    #         ]

    return solution_length, solution_cost, max_node_id
