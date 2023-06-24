from .best_first_search import best_first_search
from .memoize import memoize


def a_star_search(problem, h=None, display=False):
    """A* search is best-first graph search with f(n) = g(n)+h(n)."""
    h = memoize(h or problem.h, "h")
    return best_first_search(problem, lambda n: n.g + h(n, problem.goal))
