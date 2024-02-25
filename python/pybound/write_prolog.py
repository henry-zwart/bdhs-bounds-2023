from enum import StrEnum

from .schema import SearchProblemData, State


class LbKind(StrEnum):
    F2E = "f2e"
    F2F = "f2f"
    VIDAL = "vidal"


EPSILON = 1


def state_prolog_name(state: State):
    if state.direction == 0:
        direction = "f"
    else:
        direction = "b"
    return f'"{direction}{state.idx}"'


def get_problem_level_axioms(problem_data: SearchProblemData):
    initial_state_name = state_prolog_name(
        problem_data.state_by_idx[problem_data.initial_state_idx]
    )
    goal_state_name = state_prolog_name(
        problem_data.state_by_idx[problem_data.goal_state_idx]
    )
    return [
        f"domain({problem_data.domain}).",
        f"mode({problem_data.mode}).",
        f"heuristic({problem_data.heuristic}).",
        f"size({problem_data.size}).",
        f"c_star({problem_data.solution_cost}).",
        f"special_nodes([{initial_state_name},{goal_state_name}]).",
    ]


def get_prolog_state_value_axioms(idx_to_state):
    axioms = []
    for state in idx_to_state.values():
        prolog_name = state_prolog_name(state)
        axioms.extend(
            [
                f"g({prolog_name},{state.values.g}).",
                f"h({prolog_name},{state.values.h}).",
                f"d({prolog_name},{state.values.d}).",
            ]
        )
    return axioms


def get_prolog_parent_axioms(idx_to_state):
    axioms = []
    for state in idx_to_state.values():
        state_name = state_prolog_name(state)
        for parent_idx in state.parent_indexes:
            parent = idx_to_state[parent_idx]
            parent_name = state_prolog_name(parent)
            axioms.append(
                f"parent({parent_name},{state_name},{state.values.g - parent.values.g})."
            )

    return axioms


def get_opposite_state_axioms(idx_to_state):
    adid_lookup = {}
    for state in idx_to_state.values():
        if state.adid not in adid_lookup:
            adid_lookup[state.adid] = {}
        adid_lookup[state.adid][state.direction] = state
    opposite_pairs = [pair for pair in adid_lookup.values() if len(pair) == 2]
    axioms = [
        f"opposite({state_prolog_name(pair[0])},{state_prolog_name(pair[1])})."
        for pair in opposite_pairs
    ]
    return axioms


def regular_lb(state, other, *_args):
    return max(
        state.values.g + state.values.h,
        other.values.g + other.values.h,
        state.values.g + other.values.g + EPSILON,
    )


def f2f_lb(state, other, f2f_hs):
    if state in f2f_hs and other in f2f_hs[state]:
        return state.values.g + other.values.g + f2f_hs[state][other]
    return None


def vidal_lb(state, other, *_args):
    return max(
        state.values.g + other.values.g + EPSILON,
        state.values.g + state.values.h + other.values.d,
        other.values.g + other.values.h + state.values.d,
    )


def get_lbs(idx_to_state, f2f_hs, kind: LbKind):
    fw_states = list(filter(lambda s: s.direction == 0, idx_to_state.values()))
    bw_states = list(filter(lambda s: s.direction == 1, idx_to_state.values()))
    match kind:
        case LbKind.F2E:
            lb_fn = regular_lb
        case LbKind.F2F:
            lb_fn = f2f_lb
            f2f_hs = get_f2f_by_state(f2f_hs, idx_to_state)
        case LbKind.VIDAL:
            lb_fn = vidal_lb
        case _:
            raise ValueError(f"Unknown lower bound type: {kind}")
    return {
        (fs, bs): lb
        for fs in fw_states
        for bs in bw_states
        if (lb := lb_fn(fs, bs, f2f_hs)) is not None
    }


def get_lb_axioms(idx_to_state, f2f_hs, cstar, kind: LbKind):
    pair_lbs = {
        pair: lb
        for (pair, lb) in get_lbs(idx_to_state, f2f_hs, kind).items()
        if lb <= cstar
    }
    return [
        f"{kind}_lower_bound_pair({state_prolog_name(fs)},{state_prolog_name(bs)},{lb})."
        for ((fs, bs), lb) in pair_lbs.items()
    ]


def get_f2f_by_state(f2f_values, idx_to_state):
    f2f_by_state = {}
    for h, pairs in f2f_values.items():
        for idx_1, idx_2 in pairs:
            s1 = idx_to_state[idx_1]
            s2 = idx_to_state[idx_2]
            if s1 not in f2f_by_state:
                f2f_by_state[s1] = dict()
            f2f_by_state[s1][s2] = h
    return f2f_by_state


def get_f2f_h_axioms(idx_to_state, f2f_values):
    f2f_by_state = get_f2f_by_state(f2f_values, idx_to_state)
    axioms = []
    fw_states = list(filter(lambda s: s.direction == 0, idx_to_state.values()))
    for fs in fw_states:
        if fs not in f2f_by_state:
            continue
        for bs, h in f2f_by_state[fs].items():
            axioms.append(
                f"h_f2f({state_prolog_name(fs)},{state_prolog_name(bs)},{h})."
            )
    return axioms


def get_openlist_axioms(idx_to_state, cstar):
    pair_lbs = {
        pair: lb
        for (pair, lb) in get_lbs(idx_to_state, None, LbKind.F2E).items()
        if lb <= cstar
    }
    must_expand_states_fw = {fs for (fs, _) in pair_lbs.keys()}
    must_expand_states_bw = {bs for (_, bs) in pair_lbs.keys()}
    openlist_fw = [state_prolog_name(s) for s in must_expand_states_fw]
    openlist_bw = [state_prolog_name(s) for s in must_expand_states_bw]
    return [
        f"starting_openlistF([{','.join(openlist_fw)}]).",
        f"starting_openlistB([{','.join(openlist_bw)}]).",
    ]


def pydantic_to_prolog(problem_data: SearchProblemData):
    idx_to_state = problem_data.state_by_idx
    axioms = get_problem_level_axioms(problem_data)
    axioms.extend(get_prolog_state_value_axioms(idx_to_state))
    axioms.extend(get_prolog_parent_axioms(idx_to_state))
    axioms.extend(get_opposite_state_axioms(idx_to_state))
    axioms.extend(get_f2f_h_axioms(idx_to_state, problem_data.front_to_front_h))
    axioms.extend(get_openlist_axioms(idx_to_state, problem_data.solution_cost))

    for lb_kind in LbKind:
        axioms.extend(
            get_lb_axioms(
                idx_to_state,
                problem_data.front_to_front_h,
                problem_data.solution_cost,
                lb_kind,
            )
        )

    return axioms
