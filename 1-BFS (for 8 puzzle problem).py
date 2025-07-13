from collections import deque

class PuzzleState:
    def __init__(self, board, parent=None, move=None, depth=0):
        self.board = board
        self.parent = parent
        self.move = move
        self.depth = depth
        self.blank_pos = self.find_blank()
    
    def find_blank(self):
        """Find the position of the blank tile (0)"""
        for i in range(3):
            for j in range(3):
                if self.board[i][j] == 0:
                    return (i, j)
        return None
    
    def is_goal(self):
        """Check if current state is the goal state"""
        goal = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
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
            if 0 <= new_row < 3 and 0 <= new_col < 3:
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

def bfs_solve(initial_state):
    """
    Solve 8-puzzle using Breadth-First Search
    Returns: (solution_path, nodes_explored) or (None, nodes_explored)
    """
    start_state = PuzzleState(initial_state)
    
    # Check if initial state is already the goal
    if start_state.is_goal():
        return [start_state], 1
    
    # BFS setup
    queue = deque([start_state])
    visited = {start_state.get_state_string()}
    nodes_explored = 0
    
    while queue:
        current_state = queue.popleft()
        nodes_explored += 1
        
        # Generate all possible next states
        for neighbor in current_state.get_neighbors():
            state_string = neighbor.get_state_string()
            
            # Skip if we've already visited this state
            if state_string in visited:
                continue
            
            # Check if we found the goal
            if neighbor.is_goal():
                # Reconstruct path
                path = []
                current = neighbor
                while current:
                    path.append(current)
                    current = current.parent
                path.reverse()
                return path, nodes_explored + 1
            
            # Add to queue and mark as visited
            queue.append(neighbor)
            visited.add(state_string)
    
    # No solution found
    return None, nodes_explored

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

def is_solvable(board):
    """
    Check if the 8-puzzle is solvable
    A puzzle is solvable if the number of inversions is even
    """
    # Flatten the board and remove the blank (0)
    flattened = [num for row in board for num in row if num != 0]
    
    # Count inversions
    inversions = 0
    for i in range(len(flattened)):
        for j in range(i + 1, len(flattened)):
            if flattened[i] > flattened[j]:
                inversions += 1
    
    return inversions % 2 == 0

# Example usage
if __name__ == "__main__":
    # Example 1: Easy puzzle (2 moves to solve)
    initial_board1 = [
        [1, 2, 3],
        [4, 5, 6],
        [0, 7, 8]
    ]
    
    # Example 2: Medium puzzle
    initial_board2 = [
        [1, 2, 3],
        [4, 0, 6],
        [7, 5, 8]
    ]
    
    # Example 3: Harder puzzle
    initial_board3 = [
        [1, 2, 3],
        [5, 4, 6],
        [0, 7, 8]
    ]
    
    # Test with different puzzles
    test_cases = [
        ("Easy puzzle", initial_board1),
        ("Medium puzzle", initial_board2),
        ("Harder puzzle", initial_board3)
    ]
    
    for name, board in test_cases:
        print(f"\n{'='*50}")
        print(f"Solving: {name}")
        print(f"{'='*50}")
        
        # Check if puzzle is solvable
        if not is_solvable(board):
            print("This puzzle is not solvable!")
            continue
        
        # Solve the puzzle
        solution, nodes_explored = bfs_solve(board)
        
        if solution:
            print(f"Nodes explored: {nodes_explored}")
            print_solution(solution)
        else:
            print("No solution found!")
            print(f"Nodes explored: {nodes_explored}")