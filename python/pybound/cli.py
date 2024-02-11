import hashlib
import itertools
import json
import math
import random
import time
from pathlib import Path
from typing import Annotated, TypeAlias

# import pyo3_example
import typer
from alive_progress import alive_bar
from pybound.search.a_star_search import a_star_search
from pybound.search.domains import DomainMode, DomainType
from pybound.search.memoize import memoize
from pybound.search.node import Node
from pybound.search.post_process_b_and_d_values import post_process
from pybound.utils import serialize
from pybound.write_prolog import pydantic_to_prolog
from typer import Argument, Option

from . import __version__
from .console import get_console, init_logging
from .schema import SearchProblemData, SearchProblems, State, StateValues

DOMAIN_OPTIONS = "Domain Options"
PROBLEM_OPTIONS = "Problem Options"
CLI_OPTIONS = "CLI"


domain_arg: TypeAlias = Annotated[
    DomainType, Argument(help="Problem domain", rich_help_panel=DOMAIN_OPTIONS)
]
domain_mode_option: TypeAlias = Annotated[
    DomainMode,
    Option(
        "--mode",
        help="Run problem as unit or arbitrary cost.",
        rich_help_panel=DOMAIN_OPTIONS,
    ),
]
problem_size_option: TypeAlias = Annotated[
    int,
    Option(
        min=1,
        help="Domain problem size. Pancake: Number of pancakes. Tile: Number of non-blank tiles.",
        rich_help_panel=DOMAIN_OPTIONS,
    ),
]


def cli_init():
    init_logging(debug=True)


app = typer.Typer(
    rich_markup_mode="rich",
    add_completion=True,
    no_args_is_help=True,
    help="CLI for python package. Run searches, compute lower bound pairs, etc.",
    callback=cli_init,
)


@app.command()
def generate_problems(
    domain_type: domain_arg,
    size: problem_size_option,
    outdir: Annotated[
        Path, Option(file_okay=False, exists=True, help="dir to write problems to")
    ],
    sample: Annotated[int, Option(min=1)] = None,
):
    """Writes a set of problems, one per line, to a file."""
    domain = domain_type.get_domain()
    sample_problems = domain.enumerate(size=size, n=sample)
    # problem_lines = [
    #     "-".join([str(i), *(map(str, p))]) + "\n" for i, p in enumerate(sample_problems)
    # ]
    # problem_lines[-1] = problem_lines[-1].strip()
    for i, (I, G) in enumerate(sample_problems):
        with (outdir / f"{i}_{I}_{G}").open("w"):
            ...
    # with outfile.open("w") as f:
    #     f.writelines(problem_lines)

    # print(sample_problems)


@app.command()
def generate_problem_json(
    domain_type: domain_arg,
    initial: Annotated[str, Option(help="initial state")],
    goal: Annotated[str, Option(help="goal state")],
    outfile: Annotated[
        Path, Option(dir_okay=False, exists=False, help="file to write json to")
    ],
): ...


@app.command()
def json_to_prolog(
    input_json_path: Annotated[
        Path, Argument(exists=True, help="Json file to decode into searchproblems obj")
    ],
    output_path: Annotated[
        Path, Argument(file_okay=False, help="directory to write prolog files to")
    ],
):
    output_path.mkdir(parents=True, exist_ok=True)
    with input_json_path.open() as f:
        json_dict = json.loads(f.read())
    for i, problem in enumerate(SearchProblems(**json_dict).problems_data):
        axioms = pydantic_to_prolog(problem)
        with (output_path / f"{problem.size}_{problem.domain}_{i}_f2e.pl").open(
            "w"
        ) as f:
            f.writelines(map(lambda x: f"{x}\n", axioms))


@app.command(no_args_is_help=True)
def search(
    domain_type: domain_arg = DomainType.PANCAKE,
    mode: domain_mode_option = DomainMode.UNIT,
    size: problem_size_option = 8,
):
    cons = get_console()
    if domain_type == DomainType.SLIDING_TILE:
        cons.exit_with_error("Sliding tile not yet supported.")
    if domain_type == DomainType.SLIDING_TILE and size not in [8, 15]:
        cons.exit_with_error(f"Invalid problem size for sliding tile domain: {size}")

    domain = domain_type.get_domain(mode=mode)
    heuristic = domain_type.get_heuristic(mode=mode)

    initial = "41325"
    goal = "12345"
    problem = domain(initial, goal)
    solution_nodes, closed_list = a_star_search(problem, heuristic)

    initial_repr = domain.state_repr(initial)
    goal_repr = domain.state_repr(goal)
    problem_repr = domain.state_pair_str_repr(initial_repr, goal_repr)
    cons.print(problem_repr)
    print(solution_nodes)
    print(closed_list)


@app.command(no_args_is_help=True)
def search_results(
    domain_type: domain_arg = DomainType.PANCAKE,
    mode: domain_mode_option = DomainMode.UNIT,
    size: problem_size_option = 8,
    results_path: Path = None,
):
    # cons = get_console()
    # if domain_type == DomainType.SLIDING_TILE:
    #     cons.exit_with_error("Sliding tile not yet supported.")
    # if domain_type == DomainType.SLIDING_TILE and size not in [8, 15]:
    #     cons.exit_with_error(f"Invalid problem size for sliding tile domain: {size}")

    all_data = get_results(domain_type, mode, size)
    with results_path.open("w") as f:
        json.dump(all_data.dict(), f)


def get_results(domain_type: DomainType, mode, size):
    domain = domain_type.get_domain(mode=mode)
    heuristic = domain_type.get_heuristic(mode=mode)

    problems = domain.enumerate(size=size)
    if domain_type == "slidingtile":
        problems = random.sample(problems, k=min(len(problems), 3))

    # goal = "".join([str(i) for i in range(1, size + 1)])

    problems_data = []
    with alive_bar(len(problems)) as bar:
        for initial, goal in problems:
            # for initial_ in itertools.permutations(goal, r=size):
            # initial = "".join(initial_)
            problem_f = domain(initial, goal)
            problem_b = domain(goal, initial)

            solution_nodes_f, closed_list_f = a_star_search(problem_f, heuristic)
            _, closed_list_b = a_star_search(problem_b, heuristic)

            post_process(closed_list_f, closed_list_b, heuristic, problem_f)
            states = make_states(closed_list_f, closed_list_b)
            states_by_idx = {s.idx: s for s in states}

            initial_index = closed_list_f[problem_f.initial].id
            goal_index = closed_list_b[problem_b.initial].id

            solution_cost = solution_nodes_f[0].g
            solution_length = solution_nodes_f[0].solution_length()

            f2f_heuristics = compute_f2f_hs(
                list(closed_list_f.values()),
                list(closed_list_b.values()),
                heuristic,
                solution_cost,
            )

            problems_data.append(
                SearchProblemData(
                    domain=domain_type,
                    mode=mode,
                    size=size,
                    initial_state_idx=initial_index,
                    goal_state_idx=goal_index,
                    solution_cost=solution_cost,
                    solution_length=solution_length,
                    state_by_idx=states_by_idx,
                    front_to_front_h=f2f_heuristics,
                )
            )
            bar()

    all_data = SearchProblems(problems_data=problems_data)
    return all_data


def compute_f2f_hs(fnodes, bnodes, heuristic, cstar):
    f2f_hs = {}
    for f, b in itertools.product(fnodes, bnodes):
        h = heuristic(f.state, b.state)
        if f.g + h + b.g > cstar:
            continue
        f2f_hs[h] = f2f_hs.get(h, [])
        f2f_hs[h].append((f.id, b.id))
    return f2f_hs


def make_states(closed_list_f, closed_list_b):
    state2id = {}
    i = 0
    for state in itertools.chain(closed_list_f, closed_list_b):
        if state not in state2id:
            state2id[state] = i
            i += 1

    fw_states = make_directional_states(closed_list_f, 0, state2id)
    bw_states = make_directional_states(closed_list_b, 1, state2id)
    return list(itertools.chain(fw_states, bw_states))


def make_directional_states(closed_list, direction, state2id):
    states = []
    for node in closed_list.values():
        idx = node.id
        adid = state2id[node.state]
        state = str(node.state)
        values = StateValues(g=node.g, h=node.h, d=node.d, b=node.b)
        parent_indexes = [p.id for p in node.parents]
        states.append(
            State(
                idx=idx,
                adid=adid,
                direction=direction,
                state=state,
                values=values,
                parent_indexes=parent_indexes,
            )
        )
    return states


@app.command()
def view_state():
    cons = get_console()
    tile = DomainType.SLIDING_TILE.get_domain()
    pancake = DomainType.PANCAKE.get_domain()
    cons.print(tile.state_repr([int(x) for x in "801234567"]))
    cons.print(pancake.state_repr("123456"))


@app.command()
def version():
    cons = get_console()
    # print(pyo3_example.sum_as_string(5, 20))
    state1 = (7, 4, 1, 3, 2, 6, 5, 8)
    state2 = (1, 2, 3, 4, 5, 6, 7, 8)

    heuristic = DomainType.PANCAKE.get_heuristic(mode=DomainMode.UNIT)

    n = 1000000

    with alive_bar(n) as bar:
        for _ in range(n):
            # x = pyo3_example.unit_gap(state1, state2)
            bar()

    with alive_bar(n) as bar:
        for _ in range(n):
            x = heuristic(state1, state2)
            bar()

    cons.print(f"BDHS={__version__}")


if __name__ == "__main__":
    get_results(DomainType.PANCAKE, DomainMode.UNIT, size=6)
