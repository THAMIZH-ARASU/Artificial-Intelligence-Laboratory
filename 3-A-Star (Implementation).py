import heapq
import math
from abc import ABC, abstractmethod

class AStarNode:
    """Base class for A* nodes"""
    def __init__(self, state, parent=None, g_cost=0, h_cost=0):
        self.state = state
        self.parent = parent
        self.g_cost = g_cost  # Cost from start to current node
        self.h_cost = h_cost  # Heuristic cost from current node to goal
        self.f_cost = g_cost + h_cost  # Total cost
    
    def __lt__(self, other):
        """For priority queue comparison"""
        if self.f_cost == other.f_cost:
            return self.h_cost < other.h_cost
        return self.f_cost < other.f_cost
    
    def __eq__(self, other):
        return self.state == other.state
    
    def __hash__(self):
        return hash(str(self.state))

class AStar:
    """Generic A* algorithm implementation"""
    
    def __init__(self, problem):
        self.problem = problem
    
    def search(self, start_state, goal_state):
        """
        Perform A* search
        Returns: (path, nodes_explored) or (None, nodes_explored)
        """
        # Initialize
        start_node = AStarNode(
            start_state, 
            None, 
            0, 
            self.problem.heuristic(start_state, goal_state)
        )
        
        open_list = [start_node]
        closed_set = set()
        open_dict = {str(start_state): start_node}
        nodes_explored = 0
        
        while open_list:
            current_node = heapq.heappop(open_list)
            current_state_str = str(current_node.state)
            
            # Remove from open_dict
            if current_state_str in open_dict:
                del open_dict[current_state_str]
            
            # Check if goal reached
            if self.problem.is_goal(current_node.state, goal_state):
                path = self._reconstruct_path(current_node)
                return path, nodes_explored
            
            closed_set.add(current_state_str)
            nodes_explored += 1
            
            # Explore neighbors
            for neighbor_state, action_cost in self.problem.get_neighbors(current_node.state):
                neighbor_state_str = str(neighbor_state)
                
                # Skip if already in closed set
                if neighbor_state_str in closed_set:
                    continue
                
                # Calculate costs
                tentative_g = current_node.g_cost + action_cost
                h_cost = self.problem.heuristic(neighbor_state, goal_state)
                
                # Check if this path to neighbor is better
                if neighbor_state_str in open_dict:
                    existing_node = open_dict[neighbor_state_str]
                    if tentative_g < existing_node.g_cost:
                        # Update existing node
                        existing_node.g_cost = tentative_g
                        existing_node.f_cost = tentative_g + h_cost
                        existing_node.parent = current_node
                        heapq.heapify(open_list)  # Re-heapify due to updated costs
                else:
                    # Add new node
                    neighbor_node = AStarNode(neighbor_state, current_node, tentative_g, h_cost)
                    heapq.heappush(open_list, neighbor_node)
                    open_dict[neighbor_state_str] = neighbor_node
        
        return None, nodes_explored
    
    def _reconstruct_path(self, node):
        """Reconstruct path from goal to start"""
        path = []
        current = node
        while current:
            path.append(current.state)
            current = current.parent
        return path[::-1]

# Problem 1: 8-Puzzle Problem
class EightPuzzleProblem:
    """8-Puzzle problem implementation for A*"""
    
    def __init__(self):
        self.goal_state = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
    
    def is_goal(self, state, goal_state):
        return state == self.goal_state
    
    def get_neighbors(self, state):
        """Get all possible moves from current state"""
        neighbors = []
        blank_pos = self._find_blank(state)
        
        # Possible moves: up, down, left, right
        moves = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        
        for dr, dc in moves:
            new_row, new_col = blank_pos[0] + dr, blank_pos[1] + dc
            
            if 0 <= new_row < 3 and 0 <= new_col < 3:
                new_state = [row[:] for row in state]  # Deep copy
                new_state[blank_pos[0]][blank_pos[1]], new_state[new_row][new_col] = \
                    new_state[new_row][new_col], new_state[blank_pos[0]][blank_pos[1]]
                neighbors.append((new_state, 1))  # Each move costs 1
        
        return neighbors
    
    def heuristic(self, state, goal_state):
        """Manhattan distance heuristic"""
        distance = 0
        for i in range(3):
            for j in range(3):
                if state[i][j] != 0:
                    # Find goal position of this number
                    for gi in range(3):
                        for gj in range(3):
                            if self.goal_state[gi][gj] == state[i][j]:
                                distance += abs(i - gi) + abs(j - gj)
                                break
        return distance
    
    def _find_blank(self, state):
        """Find position of blank tile (0)"""
        for i in range(3):
            for j in range(3):
                if state[i][j] == 0:
                    return (i, j)
        return None

# Problem 2: Grid Pathfinding
class GridPathfindingProblem:
    """Grid pathfinding problem implementation for A*"""
    
    def __init__(self, grid):
        self.grid = grid
        self.rows = len(grid)
        self.cols = len(grid[0])
    
    def is_goal(self, state, goal_state):
        return state == goal_state
    
    def get_neighbors(self, state):
        """Get all valid neighboring cells"""
        neighbors = []
        row, col = state
        
        # 8-directional movement (including diagonals)
        directions = [
            (-1, -1), (-1, 0), (-1, 1),
            (0, -1),           (0, 1),
            (1, -1),  (1, 0),  (1, 1)
        ]
        
        for dr, dc in directions:
            new_row, new_col = row + dr, col + dc
            
            if (0 <= new_row < self.rows and 0 <= new_col < self.cols and 
                self.grid[new_row][new_col] != 1):  # 1 represents obstacle
                
                # Diagonal moves cost sqrt(2), orthogonal moves cost 1
                cost = math.sqrt(2) if abs(dr) + abs(dc) == 2 else 1
                neighbors.append(((new_row, new_col), cost))
        
        return neighbors
    
    def heuristic(self, state, goal_state):
        """Euclidean distance heuristic"""
        return math.sqrt((state[0] - goal_state[0])**2 + (state[1] - goal_state[1])**2)

# Problem 3: Graph Search
class GraphSearchProblem:
    """Graph search problem implementation for A*"""
    
    def __init__(self, graph, heuristic_values):
        self.graph = graph  # Adjacency list with weights
        self.heuristic_values = heuristic_values
    
    def is_goal(self, state, goal_state):
        return state == goal_state
    
    def get_neighbors(self, state):
        """Get all neighboring nodes in the graph"""
        return [(neighbor, weight) for neighbor, weight in self.graph.get(state, [])]
    
    def heuristic(self, state, goal_state):
        """Use precomputed heuristic values"""
        return self.heuristic_values.get(state, 0)

# Utility functions
def print_8_puzzle_solution(path, nodes_explored):
    """Print solution for 8-puzzle"""
    if not path:
        print("No solution found!")
        return
    
    print(f"Solution found in {len(path) - 1} moves")
    print(f"Nodes explored: {nodes_explored}")
    print("=" * 30)
    
    for i, state in enumerate(path):
        print(f"Step {i}:")
        for row in state:
            print(' '.join(str(cell) if cell != 0 else ' ' for cell in row))
        print("-" * 10)

def print_grid_solution(grid, path, start, goal, nodes_explored):
    """Print solution for grid pathfinding"""
    if not path:
        print("No path found!")
        return
    
    print(f"Path found with length: {len(path)}")
    print(f"Nodes explored: {nodes_explored}")
    print("Path visualization:")
    
    # Create visualization grid
    vis_grid = [row[:] for row in grid]
    for i, pos in enumerate(path):
        row, col = pos
        if pos == start:
            vis_grid[row][col] = 'S'
        elif pos == goal:
            vis_grid[row][col] = 'G'
        else:
            vis_grid[row][col] = '*'
    
    for row in vis_grid:
        print(' '.join(str(cell) for cell in row))
    print()

def print_graph_solution(path, nodes_explored):
    """Print solution for graph search"""
    if not path:
        print("No path found!")
        return
    
    print(f"Path found: {' -> '.join(path)}")
    print(f"Path length: {len(path)}")
    print(f"Nodes explored: {nodes_explored}")

# Example usage and test cases
if __name__ == "__main__":
    # Test 1: 8-Puzzle Problem
    print("=" * 60)
    print("Test 1: 8-Puzzle Problem")
    print("=" * 60)
    
    puzzle_problem = EightPuzzleProblem()
    puzzle_solver = AStar(puzzle_problem)
    
    # Test case: solvable puzzle
    initial_puzzle = [[1, 2, 3], [4, 0, 6], [7, 5, 8]]
    goal_puzzle = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
    
    print("Initial state:")
    for row in initial_puzzle:
        print(' '.join(str(cell) if cell != 0 else ' ' for cell in row))
    
    path, nodes_explored = puzzle_solver.search(initial_puzzle, goal_puzzle)
    print_8_puzzle_solution(path, nodes_explored)
    
    # Test 2: Grid Pathfinding
    print("\n" + "=" * 60)
    print("Test 2: Grid Pathfinding")
    print("=" * 60)
    
    # Create a grid (0 = free, 1 = obstacle)
    grid = [
        [0, 0, 0, 1, 0, 0, 0],
        [0, 1, 0, 1, 0, 1, 0],
        [0, 1, 0, 0, 0, 1, 0],
        [0, 0, 0, 1, 1, 1, 0],
        [1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0]
    ]
    
    grid_problem = GridPathfindingProblem(grid)
    grid_solver = AStar(grid_problem)
    
    start_pos = (0, 0)
    goal_pos = (6, 6)
    
    print("Grid (0=free, 1=obstacle):")
    for row in grid:
        print(' '.join(str(cell) for cell in row))
    print(f"Start: {start_pos}, Goal: {goal_pos}")
    
    path, nodes_explored = grid_solver.search(start_pos, goal_pos)
    print_grid_solution(grid, path, start_pos, goal_pos, nodes_explored)
    
    # Test 3: Graph Search
    print("\n" + "=" * 60)
    print("Test 3: Graph Search")
    print("=" * 60)
    
    # Create a weighted graph
    graph = {
        'A': [('B', 2), ('C', 3)],
        'B': [('A', 2), ('D', 1), ('E', 4)],
        'C': [('A', 3), ('F', 5)],
        'D': [('B', 1), ('G', 2)],
        'E': [('B', 4), ('G', 1)],
        'F': [('C', 5), ('G', 3)],
        'G': []
    }
    
    # Heuristic values (estimated distance to goal 'G')
    heuristic_values = {
        'A': 6, 'B': 4, 'C': 5, 'D': 2, 'E': 1, 'F': 3, 'G': 0
    }
    
    graph_problem = GraphSearchProblem(graph, heuristic_values)
    graph_solver = AStar(graph_problem)
    
    start_node = 'A'
    goal_node = 'G'
    
    print("Graph structure:")
    for node, neighbors in graph.items():
        print(f"{node}: {neighbors}")
    print(f"Heuristic values: {heuristic_values}")
    print(f"Start: {start_node}, Goal: {goal_node}")
    
    path, nodes_explored = graph_solver.search(start_node, goal_node)
    print_graph_solution(path, nodes_explored)
    
    # Performance comparison note
    print("\n" + "=" * 60)
    print("A* Algorithm Properties:")
    print("=" * 60)
    print("✓ Optimal: Finds shortest path (if heuristic is admissible)")
    print("✓ Complete: Always finds solution if one exists")
    print("✓ Efficient: Uses heuristic to guide search")
    print("✓ Memory: O(b^d) space complexity")
    print("✓ Time: O(b^d) time complexity in worst case")
    print("\nKey: b = branching factor, d = depth of solution")