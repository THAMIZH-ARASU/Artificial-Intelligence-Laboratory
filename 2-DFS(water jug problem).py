import math

class WaterJugState:
    def __init__(self, jug1_amount, jug2_amount, parent=None, action=None, depth=0):
        self.jug1_amount = jug1_amount
        self.jug2_amount = jug2_amount
        self.parent = parent
        self.action = action
        self.depth = depth
    
    def is_goal(self, target):
        """Check if current state contains the target amount in either jug"""
        return self.jug1_amount == target or self.jug2_amount == target
    
    def get_state_tuple(self):
        """Return state as tuple for hashing/comparison"""
        return (self.jug1_amount, self.jug2_amount)
    
    def get_neighbors(self, jug1_capacity, jug2_capacity):
        """Generate all possible next states from current state"""
        neighbors = []
        
        # Action 1: Fill jug 1 completely
        if self.jug1_amount < jug1_capacity:
            neighbors.append(WaterJugState(
                jug1_capacity, 
                self.jug2_amount, 
                self, 
                f"Fill Jug 1 (capacity {jug1_capacity})",
                self.depth + 1
            ))
        
        # Action 2: Fill jug 2 completely
        if self.jug2_amount < jug2_capacity:
            neighbors.append(WaterJugState(
                self.jug1_amount, 
                jug2_capacity, 
                self, 
                f"Fill Jug 2 (capacity {jug2_capacity})",
                self.depth + 1
            ))
        
        # Action 3: Empty jug 1
        if self.jug1_amount > 0:
            neighbors.append(WaterJugState(
                0, 
                self.jug2_amount, 
                self, 
                "Empty Jug 1",
                self.depth + 1
            ))
        
        # Action 4: Empty jug 2
        if self.jug2_amount > 0:
            neighbors.append(WaterJugState(
                self.jug1_amount, 
                0, 
                self, 
                "Empty Jug 2",
                self.depth + 1
            ))
        
        # Action 5: Pour from jug 1 to jug 2
        if self.jug1_amount > 0 and self.jug2_amount < jug2_capacity:
            pour_amount = min(self.jug1_amount, jug2_capacity - self.jug2_amount)
            neighbors.append(WaterJugState(
                self.jug1_amount - pour_amount,
                self.jug2_amount + pour_amount,
                self,
                f"Pour from Jug 1 to Jug 2 ({pour_amount} units)",
                self.depth + 1
            ))
        
        # Action 6: Pour from jug 2 to jug 1
        if self.jug2_amount > 0 and self.jug1_amount < jug1_capacity:
            pour_amount = min(self.jug2_amount, jug1_capacity - self.jug1_amount)
            neighbors.append(WaterJugState(
                self.jug1_amount + pour_amount,
                self.jug2_amount - pour_amount,
                self,
                f"Pour from Jug 2 to Jug 1 ({pour_amount} units)",
                self.depth + 1
            ))
        
        return neighbors
    
    def __str__(self):
        return f"Jug1: {self.jug1_amount}, Jug2: {self.jug2_amount}"

def dfs_solve(jug1_capacity, jug2_capacity, target, max_depth=50):
    """
    Solve Water Jug problem using Depth-First Search
    Returns: (solution_path, nodes_explored) or (None, nodes_explored)
    """
    # Check if target is achievable
    if not is_solvable(jug1_capacity, jug2_capacity, target):
        return None, 0
    
    # Start with both jugs empty
    initial_state = WaterJugState(0, 0)
    
    # Check if initial state is already the goal
    if initial_state.is_goal(target):
        return [initial_state], 1
    
    # DFS setup
    stack = [initial_state]
    visited = {initial_state.get_state_tuple()}
    nodes_explored = 0
    
    while stack:
        current_state = stack.pop()
        nodes_explored += 1
        
        # Prevent infinite loops with depth limit
        if current_state.depth >= max_depth:
            continue
        
        # Generate all possible next states
        for neighbor in current_state.get_neighbors(jug1_capacity, jug2_capacity):
            state_tuple = neighbor.get_state_tuple()
            
            # Skip if we've already visited this state
            if state_tuple in visited:
                continue
            
            # Check if we found the goal
            if neighbor.is_goal(target):
                # Reconstruct path
                path = []
                current = neighbor
                while current:
                    path.append(current)
                    current = current.parent
                path.reverse()
                return path, nodes_explored + 1
            
            # Add to stack and mark as visited
            stack.append(neighbor)
            visited.add(state_tuple)
    
    # No solution found within depth limit
    return None, nodes_explored

def is_solvable(jug1_capacity, jug2_capacity, target):
    """
    Check if the water jug problem is solvable
    A target amount is achievable if and only if:
    1. target <= max(jug1_capacity, jug2_capacity)
    2. target is divisible by gcd(jug1_capacity, jug2_capacity)
    """
    if target > max(jug1_capacity, jug2_capacity):
        return False
    
    gcd_value = math.gcd(jug1_capacity, jug2_capacity)
    return target % gcd_value == 0

def print_solution(path, jug1_capacity, jug2_capacity, target):
    """Print the solution path"""
    if not path:
        print("No solution found!")
        return
    
    print(f"Solution for measuring {target} units:")
    print(f"Jug 1 capacity: {jug1_capacity}, Jug 2 capacity: {jug2_capacity}")
    print(f"Solution found in {len(path) - 1} steps:")
    print("=" * 50)
    
    for i, state in enumerate(path):
        if i == 0:
            print("Initial State:")
        else:
            print(f"Step {i}: {state.action}")
        
        print(f"  {state}")
        
        # Highlight if target is achieved
        if state.is_goal(target):
            if state.jug1_amount == target:
                print(f"  *** TARGET {target} ACHIEVED in Jug 1 ***")
            if state.jug2_amount == target:
                print(f"  *** TARGET {target} ACHIEVED in Jug 2 ***")
        
        print("-" * 30)

def solve_water_jug_problem(jug1_capacity, jug2_capacity, target, max_depth=50):
    """
    Main function to solve the water jug problem
    """
    print(f"\nSolving Water Jug Problem:")
    print(f"Jug 1 capacity: {jug1_capacity}")
    print(f"Jug 2 capacity: {jug2_capacity}")
    print(f"Target amount: {target}")
    print("-" * 40)
    
    # Check if problem is solvable
    if not is_solvable(jug1_capacity, jug2_capacity, target):
        print("This problem is not solvable!")
        print(f"Target {target} is not achievable with jugs of capacity {jug1_capacity} and {jug2_capacity}")
        return
    
    # Solve using DFS
    solution, nodes_explored = dfs_solve(jug1_capacity, jug2_capacity, target, max_depth)
    
    print(f"Nodes explored: {nodes_explored}")
    
    if solution:
        print_solution(solution, jug1_capacity, jug2_capacity, target)
    else:
        print(f"No solution found within depth limit of {max_depth}")

# Example usage and test cases
if __name__ == "__main__":
    # Classic water jug problems
    test_cases = [
        # (jug1_capacity, jug2_capacity, target, description)
        (4, 3, 2, "Classic 4L-3L jug problem to get 2L"),
        (5, 3, 4, "5L-3L jug problem to get 4L"),
        (8, 5, 3, "8L-5L jug problem to get 3L"),
        (7, 4, 5, "7L-4L jug problem to get 5L"),
        (6, 4, 2, "6L-4L jug problem to get 2L"),
        (10, 6, 8, "10L-6L jug problem to get 8L"),
        (9, 4, 6, "9L-4L jug problem to get 6L"),
        (12, 8, 4, "12L-8L jug problem to get 4L")
    ]
    
    for jug1_cap, jug2_cap, target, description in test_cases:
        print(f"\n{'='*60}")
        print(f"Test Case: {description}")
        print(f"{'='*60}")
        
        solve_water_jug_problem(jug1_cap, jug2_cap, target)
    
    # Example of unsolvable problem
    print(f"\n{'='*60}")
    print("Example of Unsolvable Problem:")
    print(f"{'='*60}")
    solve_water_jug_problem(4, 6, 5)  # This should be unsolvable