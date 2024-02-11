from functools import reduce
from math import gcd


class Node:
    """A node in a search tree. Contains a pointer to the parent (the node
    that this is a successor of) and to the actual state for this node. Note
    that if a state is arrived at by two paths, then there are two nodes with
    the same state. Also includes the action that got us to this state, and
    the total g (also known as g) to reach the node. Other functions
    may add an f and h value; see best_first_graph_search and computationalBoundForBDHS_search for
    an explanation of how the f and h values are handled. You will not need to
    subclass this class."""

    count = 1  # must start from 1 for wcnf encoding

    def __init__(self, state, parent=None, action=None, g=0):
        """Create a search tree Node, derived from a parent by an action."""
        self.state = state
        self.parent_action_pairs = [(parent, action)] if parent else []
        self.parents = [parent] if parent else []
        self.g = g
        self.id = Node.count
        self.epsilon = None
        self.iota = None
        self.cost_of_out_going_actions = []
        Node.count += 1

    def __repr__(self):
        return "<Node {}>".format(self.state)

    def _find_gcd_of_sequence(self, sequence):
        x = reduce(gcd, sequence)
        return x

    def expand(self, problem):
        """List the nodes reachable in one step from this node."""
        children = [
            self.child_node(problem, action) for action in problem.actions(self.state)
        ]

        # the children must be generated to calculate epsilon
        self.epsilon = min(self.cost_of_out_going_actions)
        self.iota = self._find_gcd_of_sequence(self.cost_of_out_going_actions)
        return children

    def child_node(self, problem, action):
        """Created a child node given an action"""
        next_state = problem.result(self.state, action)
        cost_of_child = problem.path_cost(self.g, self.state, action, next_state)
        self.cost_of_out_going_actions.append(cost_of_child - self.g)
        next_node = Node(next_state, self, action, cost_of_child)
        return next_node

    def append_parent_action_pair(self, pair):
        if self.parent_action_pairs != None:
            self.parent_action_pairs.append(pair)
            self.parents.append(pair[0])

    def state_sequence(self):
        """Return the sequence of actions to go from the root to this node."""
        return [[node[0].state for node in path] for path in self.paths()]

    def action_sequence(self):
        """Return the sequence of actions to go from the root to this node."""
        return [[node[1] for node in path[:-1]] for path in self.paths()]

    def path_sequence(self):
        """Return the sequence of nodes in all paths"""
        return [[node[0] for node in path] for path in self.paths()]

    def solution_length(self):
        """Return the length of the solution"""
        length = 0
        node = self
        while node.parents != []:
            node = node.parents[0]
            length += 1
        return length

    def paths(self):
        """Return a list of the paths from the root to this node"""
        paths = []
        stack = []
        stack.append((self, None))  # (node object, action to achieve node object)
        parent_action_pairs_pointers = {}

        while stack:
            node = stack[-1][0]
            if node.state not in parent_action_pairs_pointers:
                parent_action_pairs_pointers[node.state] = 0
            if len(node.parent_action_pairs) > parent_action_pairs_pointers[node.state]:
                stack.append(
                    node.parent_action_pairs[parent_action_pairs_pointers[node.state]]
                )
                parent_action_pairs_pointers[node.state] += 1
            else:
                if node.g == 0:
                    path = stack.copy()
                    path.reverse()
                    paths.append(path)
                popped_node = stack.pop()[0]
                parent_action_pairs_pointers[popped_node.state] = 0
        return paths

    def __eq__(self, other):
        return isinstance(other, Node) and self.state == other.state

    def __hash__(self):
        # We use the hash value of the state
        # stored in the node instead of the node
        # object itself to quickly search a node
        # with the same state in a Hash Table
        return hash(self.state)
