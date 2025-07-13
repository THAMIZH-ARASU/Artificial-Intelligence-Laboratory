import tkinter as tk
from tkinter import messagebox
import copy

class ConnectFour:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Connect Four - Human vs AI")
        self.root.geometry("700x650")
        self.root.configure(bg='#2c3e50')
        
        # Game constants
        self.ROWS = 6
        self.COLS = 7
        self.EMPTY = 0
        self.HUMAN = 1
        self.AI = 2
        
        # Initialize game board
        self.board = [[self.EMPTY for _ in range(self.COLS)] for _ in range(self.ROWS)]
        self.current_player = self.HUMAN
        self.game_over = False
        
        # Colors
        self.colors = {
            self.EMPTY: '#34495e',
            self.HUMAN: '#e74c3c',
            self.AI: '#f1c40f'
        }
        
        self.setup_ui()
        
    def setup_ui(self):
        # Title
        title_label = tk.Label(self.root, text="Connect Four", 
                              font=('Arial', 24, 'bold'), 
                              bg='#2c3e50', fg='white')
        title_label.pack(pady=10)
        
        # Status label
        self.status_label = tk.Label(self.root, text="Your turn (Red)", 
                                    font=('Arial', 16), 
                                    bg='#2c3e50', fg='white')
        self.status_label.pack(pady=5)
        
        # Game board frame
        self.board_frame = tk.Frame(self.root, bg='#34495e', bd=3, relief='raised')
        self.board_frame.pack(pady=10)
        
        # Create board buttons
        self.buttons = []
        for row in range(self.ROWS):
            button_row = []
            for col in range(self.COLS):
                btn = tk.Button(self.board_frame, 
                               width=8, height=4,
                               font=('Arial', 12, 'bold'),
                               bg=self.colors[self.EMPTY],
                               command=lambda c=col: self.make_move(c))
                btn.grid(row=row, column=col, padx=2, pady=2)
                button_row.append(btn)
            self.buttons.append(button_row)
        
        # Control buttons
        control_frame = tk.Frame(self.root, bg='#2c3e50')
        control_frame.pack(pady=10)
        
        reset_btn = tk.Button(control_frame, text="New Game", 
                             font=('Arial', 12, 'bold'),
                             bg='#3498db', fg='white',
                             command=self.reset_game)
        reset_btn.pack(side=tk.LEFT, padx=10)
        
        quit_btn = tk.Button(control_frame, text="Quit", 
                            font=('Arial', 12, 'bold'),
                            bg='#e74c3c', fg='white',
                            command=self.root.quit)
        quit_btn.pack(side=tk.LEFT, padx=10)
        
    def make_move(self, col):
        if self.game_over or self.current_player != self.HUMAN:
            return
        
        if self.drop_piece(col, self.HUMAN):
            self.update_display()
            
            if self.check_winner(self.HUMAN):
                self.game_over = True
                self.status_label.config(text="You Win! 🎉")
                messagebox.showinfo("Game Over", "Congratulations! You won!")
                return
            
            if self.is_board_full():
                self.game_over = True
                self.status_label.config(text="It's a Draw!")
                messagebox.showinfo("Game Over", "It's a draw!")
                return
            
            self.current_player = self.AI
            self.status_label.config(text="AI is thinking...")
            self.root.after(1000, self.ai_move)  # Delay for better UX
    
    def ai_move(self):
        if self.game_over:
            return
        
        col = self.get_best_move()
        if col is not None:
            self.drop_piece(col, self.AI)
            self.update_display()
            
            if self.check_winner(self.AI):
                self.game_over = True
                self.status_label.config(text="AI Wins! 🤖")
                messagebox.showinfo("Game Over", "AI won this time!")
                return
            
            if self.is_board_full():
                self.game_over = True
                self.status_label.config(text="It's a Draw!")
                messagebox.showinfo("Game Over", "It's a draw!")
                return
            
            self.current_player = self.HUMAN
            self.status_label.config(text="Your turn (Red)")
    
    def drop_piece(self, col, player):
        if col < 0 or col >= self.COLS:
            return False
        
        for row in range(self.ROWS - 1, -1, -1):
            if self.board[row][col] == self.EMPTY:
                self.board[row][col] = player
                return True
        return False
    
    def update_display(self):
        for row in range(self.ROWS):
            for col in range(self.COLS):
                piece = self.board[row][col]
                self.buttons[row][col].config(bg=self.colors[piece])
    
    def check_winner(self, player):
        # Check horizontal
        for row in range(self.ROWS):
            for col in range(self.COLS - 3):
                if all(self.board[row][col + i] == player for i in range(4)):
                    return True
        
        # Check vertical
        for row in range(self.ROWS - 3):
            for col in range(self.COLS):
                if all(self.board[row + i][col] == player for i in range(4)):
                    return True
        
        # Check diagonal (top-left to bottom-right)
        for row in range(self.ROWS - 3):
            for col in range(self.COLS - 3):
                if all(self.board[row + i][col + i] == player for i in range(4)):
                    return True
        
        # Check diagonal (top-right to bottom-left)
        for row in range(self.ROWS - 3):
            for col in range(3, self.COLS):
                if all(self.board[row + i][col - i] == player for i in range(4)):
                    return True
        
        return False
    
    def is_board_full(self):
        return all(self.board[0][col] != self.EMPTY for col in range(self.COLS))
    
    def is_valid_move(self, col):
        return 0 <= col < self.COLS and self.board[0][col] == self.EMPTY
    
    def get_valid_moves(self):
        return [col for col in range(self.COLS) if self.is_valid_move(col)]
    
    def evaluate_window(self, window, player):
        """Evaluate a window of 4 positions"""
        score = 0
        opponent = self.HUMAN if player == self.AI else self.AI
        
        if window.count(player) == 4:
            score += 100
        elif window.count(player) == 3 and window.count(self.EMPTY) == 1:
            score += 10
        elif window.count(player) == 2 and window.count(self.EMPTY) == 2:
            score += 2
        
        if window.count(opponent) == 3 and window.count(self.EMPTY) == 1:
            score -= 80
        
        return score
    
    def heuristic_evaluation(self, board, player):
        """Heuristic function to evaluate board position"""
        score = 0
        
        # Center column preference
        center_array = [board[row][self.COLS//2] for row in range(self.ROWS)]
        center_count = center_array.count(player)
        score += center_count * 3
        
        # Horizontal evaluation
        for row in range(self.ROWS):
            for col in range(self.COLS - 3):
                window = [board[row][col + i] for i in range(4)]
                score += self.evaluate_window(window, player)
        
        # Vertical evaluation
        for col in range(self.COLS):
            for row in range(self.ROWS - 3):
                window = [board[row + i][col] for i in range(4)]
                score += self.evaluate_window(window, player)
        
        # Positive diagonal evaluation
        for row in range(self.ROWS - 3):
            for col in range(self.COLS - 3):
                window = [board[row + i][col + i] for i in range(4)]
                score += self.evaluate_window(window, player)
        
        # Negative diagonal evaluation
        for row in range(self.ROWS - 3):
            for col in range(3, self.COLS):
                window = [board[row + i][col - i] for i in range(4)]
                score += self.evaluate_window(window, player)
        
        return score
    
    def minimax(self, board, depth, alpha, beta, maximizing_player):
        """Minimax algorithm with alpha-beta pruning"""
        valid_moves = self.get_valid_moves()
        
        # Terminal node checks
        if self.check_winner_on_board(board, self.AI):
            return (None, 100000)
        elif self.check_winner_on_board(board, self.HUMAN):
            return (None, -100000)
        elif len(valid_moves) == 0 or depth == 0:
            return (None, self.heuristic_evaluation(board, self.AI))
        
        if maximizing_player:
            max_eval = float('-inf')
            best_col = valid_moves[0]
            
            for col in valid_moves:
                temp_board = copy.deepcopy(board)
                self.drop_piece_on_board(temp_board, col, self.AI)
                
                eval_score = self.minimax(temp_board, depth - 1, alpha, beta, False)[1]
                
                if eval_score > max_eval:
                    max_eval = eval_score
                    best_col = col
                
                alpha = max(alpha, eval_score)
                if beta <= alpha:
                    break
            
            return (best_col, max_eval)
        else:
            min_eval = float('inf')
            best_col = valid_moves[0]
            
            for col in valid_moves:
                temp_board = copy.deepcopy(board)
                self.drop_piece_on_board(temp_board, col, self.HUMAN)
                
                eval_score = self.minimax(temp_board, depth - 1, alpha, beta, True)[1]
                
                if eval_score < min_eval:
                    min_eval = eval_score
                    best_col = col
                
                beta = min(beta, eval_score)
                if beta <= alpha:
                    break
            
            return (best_col, min_eval)
    
    def drop_piece_on_board(self, board, col, player):
        """Drop piece on a copy of the board"""
        for row in range(self.ROWS - 1, -1, -1):
            if board[row][col] == self.EMPTY:
                board[row][col] = player
                return True
        return False
    
    def check_winner_on_board(self, board, player):
        """Check winner on a given board state"""
        # Check horizontal
        for row in range(self.ROWS):
            for col in range(self.COLS - 3):
                if all(board[row][col + i] == player for i in range(4)):
                    return True
        
        # Check vertical
        for row in range(self.ROWS - 3):
            for col in range(self.COLS):
                if all(board[row + i][col] == player for i in range(4)):
                    return True
        
        # Check diagonal (top-left to bottom-right)
        for row in range(self.ROWS - 3):
            for col in range(self.COLS - 3):
                if all(board[row + i][col + i] == player for i in range(4)):
                    return True
        
        # Check diagonal (top-right to bottom-left)
        for row in range(self.ROWS - 3):
            for col in range(3, self.COLS):
                if all(board[row + i][col - i] == player for i in range(4)):
                    return True
        
        return False
    
    def get_best_move(self):
        """Get the best move using minimax with heuristic evaluation"""
        valid_moves = self.get_valid_moves()
        if not valid_moves:
            return None
        
        best_col, _ = self.minimax(self.board, 5, float('-inf'), float('inf'), True)
        return best_col
    
    def reset_game(self):
        """Reset the game to initial state"""
        self.board = [[self.EMPTY for _ in range(self.COLS)] for _ in range(self.ROWS)]
        self.current_player = self.HUMAN
        self.game_over = False
        self.status_label.config(text="Your turn (Red)")
        self.update_display()
    
    def run(self):
        self.root.mainloop()

# Run the game
if __name__ == "__main__":
    game = ConnectFour()
    game.run()