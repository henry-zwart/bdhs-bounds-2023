from pydantic import BaseModel, computed_field


class StateValues(BaseModel):
    g: int
    h: int
    d: int
    b: int


class State(BaseModel):
    idx: int
    adid: int  # Adirectionl identifier
    direction: int
    state: str
    values: StateValues
    parent_indexes: list[int]

    @computed_field
    def prolog_name(self) -> str:
        match self.direction:
            case 0:
                return f"f{self.idx}"
            case 1:
                return f"b{self.idx}"
            case _:
                raise RuntimeError()

    def __hash__(self):
        return hash(self.idx)


class SearchProblemData(BaseModel):
    domain: str
    mode: str
    size: int
    heuristic: str
    degradation: int

    initial_state_idx: int
    goal_state_idx: int
    solution_cost: int
    solution_length: int

    state_by_idx: dict[int, State]
    front_to_front_h: dict[int, list[tuple[int, int]]]


class SearchProblems(BaseModel):
    problems_data: list[SearchProblemData]
