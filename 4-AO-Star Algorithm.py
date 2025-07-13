import math
from enum import Enum
from collections import defaultdict
import copy

class NodeType(Enum):
    AND = "AND"
    OR = "OR"
    TERMINAL = "TERMINAL"

class AONode:
    """Node in the AND/OR graph"""
    
    def __init__(self, state, node_type=NodeType.OR, is_goal=False, is_unsolvable=False):
        self.state = state
        self.type = node_type
        self.is_goal = is_goal
        self.is_unsolvable = is_unsolvable
        self.heuristic = 0
        self.cost = math.inf if not is_goal else 0
        self.is_solved = is_goal
        self.best_action = None
        self.children = []  # List of actions, each action contains list of successor nodes
        self.parents = []   # Parent nodes
        self.visited = False
    
    def __str__(self):
        return f"Node({self.state}, {self.type.value}, cost={self.cost:.2f}, solved={self.is_solved})"
    
    def __repr__(self):
        return self.__str__()

class AOStar:
    """AO* Algorithm implementation"""
    
    def __init__(self, problem):
        self.problem = problem
        self.graph = {}  # state -> AONode mapping
        self.solution_graph = {}  # stores the solution sub-graph
    
    def solve(self, initial_state):
        """
        Main AO* algorithm
        Returns: (is_solvable, solution_cost, solution_graph)
        """
        # Initialize root node
        root = AONode(initial_state, NodeType.OR, self.problem.is_goal(initial_state))
        root.heuristic = self.problem.heuristic(initial_state)
        root.cost = 0 if root.is_goal else root.heuristic
        
        self.graph[initial_state] = root
        
        # Main AO* loop
        while True:
            # Phase 1: Expand a non-terminal node
            if root.is_solved:
                # Solution found
                self._build_solution_graph(root)
                return True, root.cost, self.solution_graph
            
            if root.is_unsolvable:
                # No solution exists
                return False, math.inf, {}
            
            # Find best node to expand
            node_to_expand = self._select_node_to_expand(root)
            
            if node_to_expand is None:
                # No more nodes to expand
                break
            
            # Expand the selected node
            self._expand_node(node_to_expand)
            
            # Phase 2: Update costs and solved status
            self._update_costs_and_solved_status(node_to_expand)
        
        return False, math.inf, {}
    
    def _select_node_to_expand(self, root):
        """Select the best node to expand using best-first strategy"""
        # Find all non-terminal nodes in the current best solution graph
        candidates = []
        self._collect_expansion_candidates(root, candidates, set())
        
        if not candidates:
            return None
        
        # Select node with minimum cost
        return min(candidates, key=lambda n: n.cost)
    
    def _collect_expansion_candidates(self, node, candidates, visited):
        """Collect nodes that can be expanded"""
        if node.state in visited:
            return
        visited.add(node.state)
        
        if node.is_goal or node.is_unsolvable:
            return
        
        if not node.children:
            # This is a leaf node that can be expanded
            candidates.append(node)
            return
        
        # Recurse through best action's children
        if node.best_action is not None:
            for child_node, action_cost in node.best_action:
                self._collect_expansion_candidates(child_node, candidates, visited)
    
    def _expand_node(self, node):
        """Expand a node by generating its successors"""
        if node.is_goal or node.is_unsolvable or node.children:
            return
        
        # Get all possible actions from current state
        actions = self.problem.get_actions(node.state)
        
        if not actions:
            # No actions available - mark as unsolvable
            node.is_unsolvable = True
            node.cost = math.inf
            return
        
        # For each action, create successor nodes
        for action in actions:
            successors = self.problem.get_successors(node.state, action)
            action_nodes = []
            
            for successor_state, action_cost in successors:
                # Check if node already exists in graph
                if successor_state in self.graph:
                    successor_node = self.graph[successor_state]
                else:
                    # Create new node
                    is_goal = self.problem.is_goal(successor_state)
                    successor_node = AONode(successor_state, NodeType.OR, is_goal)
                    successor_node.heuristic = self.problem.heuristic(successor_state)
                    successor_node.cost = 0 if is_goal else successor_node.heuristic
                    self.graph[successor_state] = successor_node
                
                successor_node.parents.append(node)
                action_nodes.append((successor_node, action_cost))
            
            node.children.append(action_nodes)
    
    def _update_costs_and_solved_status(self, start_node):
        """Update costs and solved status propagating upwards"""
        # Use iterative deepening to avoid stack overflow
        to_update = [start_node]
        updated = set()
        
        while to_update:
            current = to_update.pop()
            
            if current.state in updated:
                continue
            
            # Update current node
            old_cost = current.cost
            old_solved = current.is_solved
            
            self._update_single_node(current)
            
            # If node changed, propagate to parents
            if current.cost != old_cost or current.is_solved != old_solved:
                for parent in current.parents:
                    if parent.state not in updated:
                        to_update.append(parent)
            
            updated.add(current.state)
    
    def _update_single_node(self, node):
        """Update cost and solved status for a single node"""
        if node.is_goal:
            node.cost = 0
            node.is_solved = True
            return
        
        if node.is_unsolvable or not node.children:
            node.cost = math.inf
            node.is_solved = False
            node.is_unsolvable = True
            return
        
        # Calculate cost for each action
        min_cost = math.inf
        best_action = None
        
        for action in node.children:
            # For AND nodes: sum of all successor costs
            # For OR nodes: cost of best successor
            if node.type == NodeType.AND:
                action_cost = sum(child_cost for child, child_cost in action)
                action_cost += sum(child.cost for child, _ in action)
            else:  # OR node
                action_cost = min(child_cost + child.cost for child, child_cost in action)
            
            if action_cost < min_cost:
                min_cost = action_cost
                best_action = action
        
        node.cost = min_cost
        node.best_action = best_action
        
        # Update solved status
        if node.best_action:
            if node.type == NodeType.AND:
                node.is_solved = all(child.is_solved for child, _ in node.best_action)
            else:  # OR node
                node.is_solved = any(child.is_solved for child, _ in node.best_action)
        else:
            node.is_solved = False
    
    def _build_solution_graph(self, root):
        """Build the solution sub-graph"""
        self.solution_graph = {}
        self._build_solution_recursive(root, set())
    
    def _build_solution_recursive(self, node, visited):
        """Recursively build solution graph"""
        if node.state in visited:
            return
        visited.add(node.state)
        
        self.solution_graph[node.state] = {
            'type': node.type.value,
            'cost': node.cost,
            'is_goal': node.is_goal,
            'best_action': [],
            'children': []
        }
        
        if node.best_action:
            for child, action_cost in node.best_action:
                self.solution_graph[node.state]['best_action'].append((child.state, action_cost))
                self.solution_graph[node.state]['children'].append(child.state)
                self._build_solution_recursive(child, visited)

# Example Problem 1: Tower of Hanoi
class TowerOfHanoiProblem:
    """Tower of Hanoi problem for AO*"""
    
    def __init__(self, num_disks):
        self.num_disks = num_disks
        self.goal_state = tuple([tuple([]) for _ in range(2)] + [tuple(range(num_disks, 0, -1))])
    
    def is_goal(self, state):
        """Check if all disks are on the rightmost peg"""
        return len(state[2]) == self.num_disks and all(disk == self.num_disks - i for i, disk in enumerate(state[2]))
    
    def get_actions(self, state):
        """Get all valid actions from current state"""
        actions = []
        pegs = [list(peg) for peg in state]
        
        for from_peg in range(3):
            if pegs[from_peg]:  # If peg is not empty
                for to_peg in range(3):
                    if from_peg != to_peg:
                        if not pegs[to_peg] or pegs[from_peg][-1] < pegs[to_peg][-1]:
                            actions.append((from_peg, to_peg))
        
        return actions
    
    def get_successors(self, state, action):
        """Get successor states for a given action"""
        from_peg, to_peg = action
        new_state = [list(peg) for peg in state]
        
        disk = new_state[from_peg].pop()
        new_state[to_peg].append(disk)
        
        return [(tuple(tuple(peg) for peg in new_state), 1)]
    
    def heuristic(self, state):
        """Heuristic: number of disks not on the goal peg + penalty for wrong order"""
        disks_on_goal = state[2]
        correct_disks = 0
        for i, disk in enumerate(disks_on_goal):
            if disk == self.num_disks - i:
                correct_disks += 1
            else:
                break
        return self.num_disks - correct_disks

# Example Problem 2: Block World
class BlockWorldProblem:
    """Simple block world problem for AO*"""
    
    def __init__(self, initial_state, goal_state):
        self.initial_state = self._state_to_tuple(initial_state)
        self.goal_state = self._state_to_tuple(goal_state)
    
    def _state_to_tuple(self, state_dict):
        """Convert state dictionary to tuple for hashing"""
        return tuple(sorted(state_dict.items()))
    
    def _tuple_to_dict(self, state_tuple):
        """Convert state tuple back to dictionary"""
        return dict(state_tuple)
    
    def is_goal(self, state):
        """Check if current state matches goal state"""
        return state == self.goal_state
    
    def get_actions(self, state):
        """Get all valid block moving actions"""
        actions = []
        state_dict = self._tuple_to_dict(state)
        
        # state format: {block: position} where position can be 'table' or another block
        for block in state_dict:
            # Can move block if nothing is on top of it
            if not self._has_block_on_top(block, state_dict):
                # Can move to table
                if state_dict[block] != 'table':
                    actions.append(('move', block, 'table'))
                
                # Can move on top of other blocks
                for other_block in state_dict:
                    if (other_block != block and 
                        not self._has_block_on_top(other_block, state_dict) and
                        state_dict[block] != other_block):
                        actions.append(('move', block, other_block))
        
        return actions
    
    def get_successors(self, state, action):
        """Get successor states for a given action"""
        action_type, block, destination = action
        state_dict = self._tuple_to_dict(state)
        new_state = state_dict.copy()
        new_state[block] = destination
        
        return [(self._state_to_tuple(new_state), 1)]
    
    def heuristic(self, state):
        """Heuristic: number of blocks not in correct position"""
        state_dict = self._tuple_to_dict(state)
        goal_dict = self._tuple_to_dict(self.goal_state)
        return sum(1 for block in state_dict if state_dict[block] != goal_dict[block])
    
    def _has_block_on_top(self, block, state_dict):
        """Check if any block is on top of the given block"""
        return any(state_dict[b] == block for b in state_dict if b != block)

# Example Problem 3: Simple Game Tree
class GameTreeProblem:
    """Simple game tree problem for AO*"""
    
    def __init__(self, tree_structure, leaf_values):
        self.tree = tree_structure  # dict: state -> [(child_state, cost), ...]
        self.leaf_values = leaf_values  # dict: leaf_state -> value
        self.goal_states = set(leaf_values.keys())
    
    def is_goal(self, state):
        return state in self.goal_states
    
    def get_actions(self, state):
        """Each child represents an action"""
        if state in self.tree:
            return list(range(len(self.tree[state])))
        return []
    
    def get_successors(self, state, action):
        """Get successor for a specific action"""
        if state in self.tree and action < len(self.tree[state]):
            return [self.tree[state][action]]
        return []
    
    def heuristic(self, state):
        """Heuristic for game tree"""
        if state in self.leaf_values:
            return self.leaf_values[state]
        return 0

# Utility functions
def print_solution(is_solvable, cost, solution_graph):
    """Print the solution"""
    if not is_solvable:
        print("No solution exists!")
        return
    
    print(f"Solution found with cost: {cost}")
    print("Solution graph:")
    for state, info in solution_graph.items():
        print(f"  {state}: {info}")

def print_tower_solution(solution_graph, initial_state):
    """Print Tower of Hanoi solution"""
    print("Tower of Hanoi Solution Steps:")
    print("=" * 40)
    
    def print_state(state):
        print("Pegs: ", end="")
        for i, peg in enumerate(state):
            print(f"Peg{i+1}:{list(peg)} ", end="")
        print()
    
    def trace_solution(state, visited=None):
        if visited is None:
            visited = set()
        
        if state in visited:
            return
        visited.add(state)
        
        print_state(state)
        
        if state in solution_graph:
            info = solution_graph[state]
            if info['best_action']:
                print(f"  -> Action cost: {info['cost']}")
                for child_state, action_cost in info['best_action']:
                    print(f"  -> Next: {child_state}")
                    trace_solution(child_state, visited)
    
    trace_solution(initial_state)

# Example usage and test cases
if __name__ == "__main__":
    # Test 1: Tower of Hanoi
    print("=" * 60)
    print("Test 1: Tower of Hanoi (3 disks)")
    print("=" * 60)
    
    hanoi_problem = TowerOfHanoiProblem(3)
    hanoi_solver = AOStar(hanoi_problem)
    
    # Initial state: all disks on first peg
    initial_hanoi = (tuple([3, 2, 1]), tuple([]), tuple([]))
    
    print(f"Initial state: {initial_hanoi}")
    print(f"Goal state: {hanoi_problem.goal_state}")
    
    is_solvable, cost, solution = hanoi_solver.solve(initial_hanoi)
    print(f"Solvable: {is_solvable}, Cost: {cost}")
    
    if is_solvable:
        print_tower_solution(solution, initial_hanoi)
    
    # Test 2: Block World
    print("\n" + "=" * 60)
    print("Test 2: Block World")
    print("=" * 60)
    
    # Initial: A on table, B on A, C on table
    # Goal: C on B, B on A, A on table
    initial_blocks = {'A': 'table', 'B': 'A', 'C': 'table'}
    goal_blocks = {'A': 'table', 'B': 'A', 'C': 'B'}
    
    block_problem = BlockWorldProblem(initial_blocks, goal_blocks)
    block_solver = AOStar(block_problem)
    
    print(f"Initial state: {initial_blocks}")
    print(f"Goal state: {goal_blocks}")
    
    is_solvable, cost, solution = block_solver.solve(block_problem.initial_state)
    print(f"Solvable: {is_solvable}, Cost: {cost}")
    print_solution(is_solvable, cost, solution)
    
    # Test 3: Simple Game Tree
    print("\n" + "=" * 60)
    print("Test 3: Simple Game Tree")
    print("=" * 60)
    
    # Simple tree structure
    tree_structure = {
        'A': [('B', 1), ('C', 2)],
        'B': [('D', 3), ('E', 1)],
        'C': [('F', 2), ('G', 4)],
    }
    
    leaf_values = {'D': 5, 'E': 2, 'F': 8, 'G': 3}
    
    game_problem = GameTreeProblem(tree_structure, leaf_values)
    game_solver = AOStar(game_problem)
    
    print("Tree structure:", tree_structure)
    print("Leaf values:", leaf_values)
    
    is_solvable, cost, solution = game_solver.solve('A')
    print(f"Solvable: {is_solvable}, Cost: {cost}")
    print_solution(is_solvable, cost, solution)
    
    # Algorithm properties
    print("\n" + "=" * 60)
    print("AO* Algorithm Properties:")
    print("=" * 60)
    print("✓ Optimal: Finds minimum cost solution")
    print("✓ Handles AND/OR graphs: Can solve complex decomposable problems")
    print("✓ Heuristic-guided: Uses admissible heuristics")
    print("✓ Solution graphs: Maintains compact solution representation")
    print("✓ Iterative improvement: Refines solution through search")
    print("\nApplications:")
    print("- Game playing (minimax with pruning)")
    print("- Planning problems")
    print("- Theorem proving")
    print("- Puzzle solving with sub-goals")