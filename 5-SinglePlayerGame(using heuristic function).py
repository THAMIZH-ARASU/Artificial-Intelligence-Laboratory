import tkinter as tk
from tkinter import messagebox, ttk
import random
import heapq
from copy import deepcopy
import time

class PuzzleState:
    def __init__(self, board, moves=0, parent=None):
        self.board = board
        self.moves = moves
        self.parent = parent
        self.blank_pos = self.find_blank()
        
    def find_blank(self):
        for i in range(3):
            for j in range(3):
                if self.board[i][j] == 0:
                    return (i, j)
        return None
    
    def is_goal(self):
        goal = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
        return self.board == goal
    
    def get_neighbors(self):
        neighbors = []
        row, col = self.blank_pos
        directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        
        for dr, dc in directions:
            new_row, new_col = row + dr, col + dc
            if 0 <= new_row < 3 and 0 <= new_col < 3:
                new_board = deepcopy(self.board)
                new_board[row][col], new_board[new_row][new_col] = new_board[new_row][new_col], new_board[row][col]
                neighbors.append(PuzzleState(new_board, self.moves + 1, self))
        
        return neighbors
    
    def manhattan_distance(self):
        """Heuristic function: Manhattan distance"""
        distance = 0
        goal_positions = {
            1: (0, 0), 2: (0, 1), 3: (0, 2),
            4: (1, 0), 5: (1, 1), 6: (1, 2),
            7: (2, 0), 8: (2, 1), 0: (2, 2)
        }
        
        for i in range(3):
            for j in range(3):
                if self.board[i][j] != 0:
                    goal_row, goal_col = goal_positions[self.board[i][j]]
                    distance += abs(i - goal_row) + abs(j - goal_col)
        
        return distance
    
    def misplaced_tiles(self):
        """Alternative heuristic: Number of misplaced tiles"""
        goal = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
        count = 0
        for i in range(3):
            for j in range(3):
                if self.board[i][j] != 0 and self.board[i][j] != goal[i][j]:
                    count += 1
        return count
    
    def __lt__(self, other):
        return (self.moves + self.manhattan_distance()) < (other.moves + other.manhattan_distance())
    
    def __eq__(self, other):
        return self.board == other.board
    
    def __hash__(self):
        return hash(str(self.board))

class PuzzleGame:
    def __init__(self, root):
        self.root = root
        self.root.title("8-Puzzle Game with AI Heuristics")
        self.root.geometry("600x700")
        self.root.configure(bg='#f0f0f0')
        
        self.current_state = None
        self.solution_path = []
        self.hint_step = 0
        self.moves_count = 0
        
        self.setup_ui()
        self.new_game()
    
    def setup_ui(self):
        # Title
        title_label = tk.Label(self.root, text="8-Puzzle Game", 
                              font=("Arial", 20, "bold"), 
                              bg='#f0f0f0', fg='#333')
        title_label.pack(pady=10)
        
        # Game board frame
        self.board_frame = tk.Frame(self.root, bg='#f0f0f0')
        self.board_frame.pack(pady=20)
        
        # Create 3x3 grid of buttons
        self.buttons = []
        for i in range(3):
            row = []
            for j in range(3):
                btn = tk.Button(self.board_frame, text="", 
                               font=("Arial", 20, "bold"),
                               width=4, height=2,
                               command=lambda r=i, c=j: self.tile_clicked(r, c),
                               bg='#e0e0e0', fg='#333',
                               relief='raised', bd=2)
                btn.grid(row=i, column=j, padx=2, pady=2)
                row.append(btn)
            self.buttons.append(row)
        
        # Control buttons frame
        control_frame = tk.Frame(self.root, bg='#f0f0f0')
        control_frame.pack(pady=20)
        
        # Buttons
        tk.Button(control_frame, text="New Game", 
                 command=self.new_game, 
                 font=("Arial", 12, "bold"),
                 bg='#4CAF50', fg='white', 
                 padx=20, pady=5).pack(side=tk.LEFT, padx=5)
        
        tk.Button(control_frame, text="Get Hint", 
                 command=self.get_hint, 
                 font=("Arial", 12, "bold"),
                 bg='#2196F3', fg='white', 
                 padx=20, pady=5).pack(side=tk.LEFT, padx=5)
        
        tk.Button(control_frame, text="Auto Solve", 
                 command=self.auto_solve, 
                 font=("Arial", 12, "bold"),
                 bg='#FF9800', fg='white', 
                 padx=20, pady=5).pack(side=tk.LEFT, padx=5)
        
        # Info frame
        info_frame = tk.Frame(self.root, bg='#f0f0f0')
        info_frame.pack(pady=10)
        
        self.moves_label = tk.Label(info_frame, text="Moves: 0", 
                                   font=("Arial", 12), 
                                   bg='#f0f0f0', fg='#333')
        self.moves_label.pack(side=tk.LEFT, padx=20)
        
        self.heuristic_label = tk.Label(info_frame, text="Manhattan Distance: 0", 
                                       font=("Arial", 12), 
                                       bg='#f0f0f0', fg='#333')
        self.heuristic_label.pack(side=tk.LEFT, padx=20)
        
        # Status text
        self.status_text = tk.Text(self.root, height=8, width=70, 
                                  font=("Arial", 10),
                                  bg='#ffffff', fg='#333',
                                  relief='sunken', bd=2)
        self.status_text.pack(pady=10, padx=20)
        
        # Scrollbar for status text
        scrollbar = tk.Scrollbar(self.status_text)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.status_text.config(yscrollcommand=scrollbar.set)
        scrollbar.config(command=self.status_text.yview)
    
    def update_display(self):
        """Update the game board display"""
        for i in range(3):
            for j in range(3):
                value = self.current_state.board[i][j]
                if value == 0:
                    self.buttons[i][j].config(text="", bg='#f0f0f0', state='disabled')
                else:
                    self.buttons[i][j].config(text=str(value), bg='#e0e0e0', state='normal')
        
        # Update info labels
        self.moves_label.config(text=f"Moves: {self.moves_count}")
        self.heuristic_label.config(text=f"Manhattan Distance: {self.current_state.manhattan_distance()}")
    
    def tile_clicked(self, row, col):
        """Handle tile click"""
        if self.current_state.board[row][col] == 0:
            return
        
        blank_row, blank_col = self.current_state.blank_pos
        
        # Check if the clicked tile is adjacent to the blank space
        if (abs(row - blank_row) == 1 and col == blank_col) or \
           (abs(col - blank_col) == 1 and row == blank_row):
            
            # Swap tiles
            new_board = deepcopy(self.current_state.board)
            new_board[row][col], new_board[blank_row][blank_col] = \
                new_board[blank_row][blank_col], new_board[row][col]
            
            self.current_state = PuzzleState(new_board)
            self.moves_count += 1
            self.update_display()
            
            # Check if puzzle is solved
            if self.current_state.is_goal():
                self.log_message("🎉 Congratulations! Puzzle solved!")
                messagebox.showinfo("Victory!", f"Puzzle solved in {self.moves_count} moves!")
    
    def new_game(self):
        """Start a new game with shuffled board"""
        # Start with goal state and shuffle
        goal_board = [[1, 2, 3], [4, 5, 6], [7, 8, 0]]
        self.current_state = PuzzleState(goal_board)
        
        # Shuffle by making random valid moves
        for _ in range(100):
            neighbors = self.current_state.get_neighbors()
            if neighbors:
                self.current_state = random.choice(neighbors)
        
        self.moves_count = 0
        self.hint_step = 0
        self.solution_path = []
        
        self.update_display()
        self.log_message("New game started! Use heuristics to solve the puzzle.")
        self.log_message(f"Initial heuristic values:")
        self.log_message(f"  Manhattan Distance: {self.current_state.manhattan_distance()}")
        self.log_message(f"  Misplaced Tiles: {self.current_state.misplaced_tiles()}")
    
    def a_star_search(self):
        """A* search algorithm using Manhattan distance heuristic"""
        start_time = time.time()
        
        open_set = []
        closed_set = set()
        
        heapq.heappush(open_set, self.current_state)
        nodes_explored = 0
        
        while open_set:
            current = heapq.heappop(open_set)
            nodes_explored += 1
            
            if current.is_goal():
                # Reconstruct path
                path = []
                while current:
                    path.append(current)
                    current = current.parent
                
                end_time = time.time()
                self.log_message(f"Solution found! Nodes explored: {nodes_explored}")
                self.log_message(f"Search time: {end_time - start_time:.3f} seconds")
                self.log_message(f"Optimal moves: {len(path) - 1}")
                
                return path[::-1]
            
            closed_set.add(current)
            
            for neighbor in current.get_neighbors():
                if neighbor in closed_set:
                    continue
                
                if neighbor not in open_set:
                    heapq.heappush(open_set, neighbor)
        
        return None
    
    def get_hint(self):
        """Provide a hint using A* search"""
        if self.current_state.is_goal():
            self.log_message("Puzzle already solved!")
            return
        
        self.log_message("🔍 Calculating hint using A* search...")
        
        if not self.solution_path:
            self.solution_path = self.a_star_search()
            self.hint_step = 0
        
        if self.solution_path and self.hint_step < len(self.solution_path) - 1:
            current = self.solution_path[self.hint_step]
            next_state = self.solution_path[self.hint_step + 1]
            
            # Find which tile moved
            for i in range(3):
                for j in range(3):
                    if current.board[i][j] != next_state.board[i][j] and next_state.board[i][j] != 0:
                        tile_value = next_state.board[i][j]
                        self.log_message(f"💡 Hint: Move tile {tile_value}")
                        
                        # Highlight the tile to move
                        for row in range(3):
                            for col in range(3):
                                if current.board[row][col] == tile_value:
                                    self.buttons[row][col].config(bg='#FFD700')
                                    self.root.after(2000, lambda r=row, c=col: 
                                                   self.buttons[r][c].config(bg='#e0e0e0'))
                        return
        else:
            self.log_message("No more hints available!")
    
    def auto_solve(self):
        """Automatically solve the puzzle step by step"""
        if self.current_state.is_goal():
            self.log_message("Puzzle already solved!")
            return
        
        self.log_message("🤖 Auto-solving puzzle...")
        
        solution_path = self.a_star_search()
        
        if solution_path:
            self.animate_solution(solution_path, 1)
        else:
            self.log_message("No solution found!")
    
    def animate_solution(self, path, step):
        """Animate the solution step by step"""
        if step >= len(path):
            self.log_message("✅ Auto-solve completed!")
            return
        
        self.current_state = path[step]
        self.moves_count = step
        self.update_display()
        
        # Continue animation
        self.root.after(500, lambda: self.animate_solution(path, step + 1))
    
    def log_message(self, message):
        """Add message to status text"""
        self.status_text.insert(tk.END, message + "\n")
        self.status_text.see(tk.END)
        self.root.update()

def main():
    root = tk.Tk()
    game = PuzzleGame(root)
    root.mainloop()

if __name__ == "__main__":
    main()