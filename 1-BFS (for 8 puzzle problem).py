from collections import deque
import os
import math

class PuzzleState:
    def __init__(self, board, parent=None, move=None, depth=0):
        self.board = board
        self.parent = parent
        self.move = move
        self.depth = depth
        self.size = len(board)
        self.blank_pos = self.find_blank()
    
    def find_blank(self):
        """Find the position of the blank tile (0)"""
        for i in range(self.size):
            for j in range(self.size):
                if self.board[i][j] == 0:
                    return (i, j)
        return None
    
    def is_goal(self):
        """Check if current state is the goal state"""
        goal = []
        for i in range(self.size):
            row = []
            for j in range(self.size):
                if i == self.size - 1 and j == self.size - 1:
                    row.append(0)  # Blank at bottom-right
                else:
                    row.append(i * self.size + j + 1)
            goal.append(row)
        return self.board == goal
    
    def get_neighbors(self):
        """Generate all possible next states"""
        neighbors = []
        row, col = self.blank_pos
        
        # Possible moves: up, down, left, right
        moves = [(-1, 0, 'UP'), (1, 0, 'DOWN'), (0, -1, 'LEFT'), (0, 1, 'RIGHT')]
        
        for dr, dc, move_name in moves:
            new_row, new_col = row + dr, col + dc
            
            # Check if the move is valid (within bounds)
            if 0 <= new_row < self.size and 0 <= new_col < self.size:
                # Create new board by swapping blank with adjacent tile
                new_board = [row[:] for row in self.board]  # Deep copy
                new_board[row][col], new_board[new_row][new_col] = \
                    new_board[new_row][new_col], new_board[row][col]
                
                neighbors.append(PuzzleState(new_board, self, move_name, self.depth + 1))
        
        return neighbors
    
    def get_state_string(self):
        """Convert board to string for hashing/comparison"""
        return str(self.board)
    
    def print_board(self):
        """Print the current board state"""
        for row in self.board:
            print(' '.join(str(cell) if cell != 0 else ' ' for cell in row))
        print()
    
    def board_to_string(self):
        """Convert board to formatted string"""
        result = ""
        for row in self.board:
            result += " ".join(str(cell) if cell != 0 else " " for cell in row) + "\n"
        return result.rstrip()

def bfs_solve(initial_state):
    """
    Solve n-puzzle using Breadth-First Search
    Returns: (solution_path, nodes_explored, all_states_tree) or (None, nodes_explored, all_states_tree)
    """
    start_state = PuzzleState(initial_state)
    
    # Check if initial state is already the goal
    if start_state.is_goal():
        return [start_state], 1, {start_state.get_state_string(): start_state}
    
    # BFS setup
    queue = deque([start_state])
    visited = {start_state.get_state_string()}
    nodes_explored = 0
    all_states_tree = {start_state.get_state_string(): start_state}
    
    while queue:
        current_state = queue.popleft()
        nodes_explored += 1
        
        # Generate all possible next states
        for neighbor in current_state.get_neighbors():
            state_string = neighbor.get_state_string()
            
            # Skip if we've already visited this state
            if state_string in visited:
                continue
            
            # Add to tree
            all_states_tree[state_string] = neighbor
            
            # Check if we found the goal
            if neighbor.is_goal():
                # Reconstruct path
                path = []
                current = neighbor
                while current:
                    path.append(current)
                    current = current.parent
                path.reverse()
                return path, nodes_explored + 1, all_states_tree
            
            # Add to queue and mark as visited
            queue.append(neighbor)
            visited.add(state_string)
    
    # No solution found
    return None, nodes_explored, all_states_tree

def print_solution(path):
    """Print the solution path"""
    if not path:
        print("No solution found!")
        return
    
    print(f"Solution found in {len(path) - 1} moves:")
    print("=" * 30)
    
    for i, state in enumerate(path):
        if i == 0:
            print("Initial State:")
        else:
            print(f"Move {i}: {state.move}")
        
        state.print_board()
        print(f"Depth: {state.depth}")
        print("-" * 15)

def format_tree_output(all_states_tree, solution_path):
    """Format the tree output for file and console display"""
    output = []
    puzzle_size = next(iter(all_states_tree.values())).size
    output.append(f"{puzzle_size}-PUZZLE BFS SEARCH TREE")
    output.append("=" * 50)
    output.append("")
    
    # Build parent-child relationships
    children_map = {}
    for state in all_states_tree.values():
        if state.parent:
            parent_key = state.parent.get_state_string()
            if parent_key not in children_map:
                children_map[parent_key] = []
            children_map[parent_key].append(state)
    
    # Find root state (state with no parent)
    root_state = None
    for state in all_states_tree.values():
        if state.parent is None:
            root_state = state
            break
    
    def print_tree_node(state, prefix="", is_last=True, visited=None):
        """Recursively print tree node with proper formatting"""
        if visited is None:
            visited = set()
        
        state_key = state.get_state_string()
        if state_key in visited:
            return
        
        visited.add(state_key)
        
        # Mark if this state is part of the solution path
        is_solution = state in solution_path if solution_path else False
        solution_marker = " [SOLUTION]" if is_solution else ""
        
        # Print current node
        connector = "\\-- " if is_last else "+-- "
        output.append(f"{prefix}{connector}State (Depth {state.depth}){solution_marker}")
        
        if state.move:
            move_prefix = "    " if is_last else "|   "
            output.append(f"{prefix}{move_prefix}Move: {state.move}")
        
        # Print board
        board_prefix = "    " if is_last else "|   "
        output.append(f"{prefix}{board_prefix}Board:")
        board_lines = state.board_to_string().split('\n')
        for line in board_lines:
            output.append(f"{prefix}{board_prefix}{line}")
        output.append("")
        
        # Print children
        children = children_map.get(state_key, [])
        for i, child in enumerate(children):
            is_last_child = (i == len(children) - 1)
            new_prefix = prefix + ("    " if is_last else "|   ")
            print_tree_node(child, new_prefix, is_last_child, visited)
    
    # Print the tree starting from root
    if root_state:
        output.append("TREE STRUCTURE:")
        output.append("-" * 20)
        print_tree_node(root_state)
    
    # Add solution summary
    if solution_path:
        output.append("\nSOLUTION PATH:")
        output.append("-" * 20)
        for i, state in enumerate(solution_path):
            if i == 0:
                output.append("Initial State:")
            else:
                output.append(f"Move {i}: {state.move}")
            output.append(state.board_to_string())
            output.append("")
    
    return "\n".join(output)

def get_puzzle_size():
    """Get the puzzle size from user"""
    while True:
        try:
            size = int(input("Enter puzzle size (e.g., 3 for 3x3, 4 for 4x4): "))
            if size < 2:
                print("Puzzle size must be at least 2")
                continue
            return size
        except ValueError:
            print("Please enter a valid number")

def get_user_input():
    """Get the initial board state from user"""
    size = get_puzzle_size()
    max_num = size * size - 1
    
    print(f"\nEnter the initial {size}x{size} puzzle board state:")
    print(f"Use 0 to represent the blank space")
    print(f"Enter each row as space-separated numbers (0 to {max_num}):")
    
    board = []
    for i in range(size):
        while True:
            try:
                row_input = input(f"Row {i+1}: ").strip()
                row = [int(x) for x in row_input.split()]
                if len(row) != size:
                    print(f"Please enter exactly {size} numbers per row")
                    continue
                if not all(0 <= x <= max_num for x in row):
                    print(f"Please enter numbers between 0 and {max_num}")
                    continue
                board.append(row)
                break
            except ValueError:
                print("Please enter valid numbers")
    
    return board

def is_solvable(board):
    """
    Check if the n-puzzle is solvable
    For odd-sized puzzles: solvable if inversions is even
    For even-sized puzzles: solvable if inversions + blank_row_from_bottom is odd
    """
    size = len(board)
    max_num = size * size - 1
    
    # Flatten the board and remove the blank (0)
    flattened = [num for row in board for num in row if num != 0]
    
    # Count inversions
    inversions = 0
    for i in range(len(flattened)):
        for j in range(i + 1, len(flattened)):
            if flattened[i] > flattened[j]:
                inversions += 1
    
    # Find blank position from bottom
    blank_row_from_bottom = 0
    for i in range(size):
        for j in range(size):
            if board[i][j] == 0:
                blank_row_from_bottom = size - 1 - i
                break
    
    if size % 2 == 1:  # Odd-sized puzzle
        return inversions % 2 == 0
    else:  # Even-sized puzzle
        return (inversions + blank_row_from_bottom) % 2 == 1

def validate_board(board):
    """Validate that the board contains all required numbers exactly once"""
    size = len(board)
    max_num = size * size - 1
    numbers = [num for row in board for num in row]
    return sorted(numbers) == list(range(max_num + 1))

def print_goal_state(size):
    """Print the goal state for reference"""
    print(f"\nGoal state for {size}x{size} puzzle:")
    goal = []
    for i in range(size):
        row = []
        for j in range(size):
            if i == size - 1 and j == size - 1:
                row.append(0)  # Blank at bottom-right
            else:
                row.append(i * size + j + 1)
        goal.append(row)
    
    for row in goal:
        print(' '.join(str(cell) if cell != 0 else ' ' for cell in row))
    print()

# Example usage
if __name__ == "__main__":
    print("N-PUZZLE SOLVER USING BFS")
    print("=" * 30)
    print()
    
    # Get user input
    initial_board = get_user_input()
    size = len(initial_board)
    
    print(f"\nInitial {size}x{size} Board:")
    for row in initial_board:
        print(' '.join(str(cell) if cell != 0 else ' ' for cell in row))
    print()
    
    # Show goal state
    print_goal_state(size)
    
    # Validate the board
    if not validate_board(initial_board):
        max_num = size * size - 1
        print(f"Error: Board must contain numbers 0-{max_num} exactly once!")
        exit(1)
    
    # Check if puzzle is solvable
    if not is_solvable(initial_board):
        print("This puzzle is not solvable!")
        exit(1)
    
    # Solve the puzzle
    solution, nodes_explored, all_states_tree = bfs_solve(initial_board)
    
    # Generate tree output
    tree_output = format_tree_output(all_states_tree, solution)
    
    # Save to file
    with open("outputs/1-output.txt", "w") as f:
        f.write(tree_output)
    
    # Display results
    print(f"Nodes explored: {nodes_explored}")
    print(f"Total states in tree: {len(all_states_tree)}")
    
    if solution:
        print(f"Solution found in {len(solution) - 1} moves!")
        print_solution(solution)
    else:
        print("No solution found!")
    
    print("\n" + "="*50)
    print("TREE OUTPUT:")
    print("="*50)
    print(tree_output)
    print(f"\nTree output has been saved to 'outputs/1-output.txt'")