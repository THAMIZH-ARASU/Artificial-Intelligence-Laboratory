import math

class WaterJugState:
    def __init__(self, jug1_amount, jug2_amount, parent=None, action=None, depth=0):
        self.jug1_amount = jug1_amount
        self.jug2_amount = jug2_amount
        self.parent = parent
        self.action = action
        self.depth = depth
    
    def is_goal(self, target, target_jug=None):
        """Check if current state contains the target amount in the specified jug"""
        if target_jug is None:
            # Default behavior: target in either jug
            return self.jug1_amount == target or self.jug2_amount == target
        elif target_jug == 1:
            return self.jug1_amount == target
        elif target_jug == 2:
            return self.jug2_amount == target
        return False
    
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

def dfs_solve(jug1_capacity, jug2_capacity, target, target_jug=None, max_depth=50):
    """
    Solve Water Jug problem using Depth-First Search
    Returns: (solution_path, nodes_explored, all_states_tree) or (None, nodes_explored, all_states_tree)
    """
    # Check if target is achievable
    if not is_solvable(jug1_capacity, jug2_capacity, target):
        return None, 0, {}
    
    # Start with both jugs empty
    initial_state = WaterJugState(0, 0)
    
    # Check if initial state is already the goal
    if initial_state.is_goal(target, target_jug):
        all_states_tree = {initial_state.get_state_tuple(): initial_state}
        return [initial_state], 1, all_states_tree
    
    # DFS setup
    stack = [initial_state]
    visited = {initial_state.get_state_tuple()}
    nodes_explored = 0
    all_states_tree = {initial_state.get_state_tuple(): initial_state}
    
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
            
            # Add to tree
            all_states_tree[state_tuple] = neighbor
            
            # Check if we found the goal
            if neighbor.is_goal(target, target_jug):
                # Reconstruct path
                path = []
                current = neighbor
                while current:
                    path.append(current)
                    current = current.parent
                path.reverse()
                return path, nodes_explored + 1, all_states_tree
            
            # Add to stack and mark as visited
            stack.append(neighbor)
            visited.add(state_tuple)
    
    # No solution found within depth limit
    return None, nodes_explored, all_states_tree

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

def format_tree_output(all_states_tree, solution_path, jug1_capacity, jug2_capacity, target, target_jug=None):
    """Format the tree output for file and console display"""
    output = []
    output.append("WATER JUG PROBLEM DFS SEARCH TREE")
    output.append("=" * 50)
    output.append("")
    
    # Build parent-child relationships
    children_map = {}
    for state in all_states_tree.values():
        if state.parent:
            parent_key = state.parent.get_state_tuple()
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
        
        state_key = state.get_state_tuple()
        if state_key in visited:
            return
        
        visited.add(state_key)
        
        # Mark if this state is part of the solution path
        is_solution = state in solution_path if solution_path else False
        solution_marker = " [SOLUTION]" if is_solution else ""
        
        # Print current node
        connector = "\\-- " if is_last else "+-- "
        output.append(f"{prefix}{connector}State (Depth {state.depth}){solution_marker}")
        
        if state.action:
            action_prefix = "    " if is_last else "|   "
            output.append(f"{prefix}{action_prefix}Action: {state.action}")
        
        # Print state
        state_prefix = "    " if is_last else "|   "
        output.append(f"{prefix}{state_prefix}State: {state}")
        
        # Highlight if target is achieved
        if state.is_goal(target, target_jug):
            if state.jug1_amount == target:
                output.append(f"{prefix}{state_prefix}*** TARGET {target} ACHIEVED in Jug 1 ***")
            if state.jug2_amount == target:
                output.append(f"{prefix}{state_prefix}*** TARGET {target} ACHIEVED in Jug 2 ***")
        
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
                output.append(f"Step {i}: {state.action}")
            output.append(f"  {state}")
            
            # Highlight if target is achieved
            if state.is_goal(target, target_jug):
                if state.jug1_amount == target:
                    output.append(f"  *** TARGET {target} ACHIEVED in Jug 1 ***")
                if state.jug2_amount == target:
                    output.append(f"  *** TARGET {target} ACHIEVED in Jug 2 ***")
            output.append("")
    
    return "\n".join(output)

def get_user_input():
    """Get jug capacities and target from user"""
    print("WATER JUG PROBLEM SOLVER USING DFS")
    print("=" * 40)
    print()
    
    # Get jug capacities
    while True:
        try:
            jug1_capacity = int(input("Enter capacity of Jug 1: "))
            if jug1_capacity <= 0:
                print("Jug capacity must be positive")
                continue
            break
        except ValueError:
            print("Please enter a valid number")
    
    while True:
        try:
            jug2_capacity = int(input("Enter capacity of Jug 2: "))
            if jug2_capacity <= 0:
                print("Jug capacity must be positive")
                continue
            break
        except ValueError:
            print("Please enter a valid number")
    
    # Get target amount
    while True:
        try:
            target = int(input("Enter target amount to measure: "))
            if target <= 0:
                print("Target amount must be positive")
                continue
            break
        except ValueError:
            print("Please enter a valid number")
    
    # Get target jug preference
    while True:
        try:
            print("\nWhich jug should contain the target amount in the final state?")
            print("1. Jug 1")
            print("2. Jug 2") 
            print("3. Either jug (any solution)")
            target_jug_choice = int(input("Enter your choice (1, 2, or 3): "))
            if target_jug_choice in [1, 2, 3]:
                target_jug = target_jug_choice if target_jug_choice != 3 else None
                break
            else:
                print("Please enter 1, 2, or 3")
        except ValueError:
            print("Please enter a valid number")
    
    return jug1_capacity, jug2_capacity, target, target_jug

def print_solution(path, jug1_capacity, jug2_capacity, target, target_jug=None):
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
        if state.is_goal(target, target_jug):
            if state.jug1_amount == target:
                print(f"  *** TARGET {target} ACHIEVED in Jug 1 ***")
            if state.jug2_amount == target:
                print(f"  *** TARGET {target} ACHIEVED in Jug 2 ***")
        
        print("-" * 30)

def solve_water_jug_problem(jug1_capacity, jug2_capacity, target, target_jug=None, max_depth=50):
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
        return None, 0, {}
    
    # Solve using DFS
    solution, nodes_explored, all_states_tree = dfs_solve(jug1_capacity, jug2_capacity, target, target_jug, max_depth)
    
    print(f"Nodes explored: {nodes_explored}")
    
    if solution:
        print_solution(solution, jug1_capacity, jug2_capacity, target, target_jug)
    else:
        print(f"No solution found within depth limit of {max_depth}")
    
    return solution, nodes_explored, all_states_tree

# Main execution
if __name__ == "__main__":
    # Get user input
    jug1_cap, jug2_cap, target, target_jug = get_user_input()
    
    # Display target jug preference
    if target_jug == 1:
        target_jug_desc = "Jug 1"
    elif target_jug == 2:
        target_jug_desc = "Jug 2"
    else:
        target_jug_desc = "Either jug"
    
    print(f"\nInitial State: Jug1: 0, Jug2: 0")
    print(f"Target: {target} units in {target_jug_desc}")
    print()
    
    # Solve the problem
    solution, nodes_explored, all_states_tree = solve_water_jug_problem(jug1_cap, jug2_cap, target, target_jug)
    
    # Generate tree output
    tree_output = format_tree_output(all_states_tree, solution, jug1_cap, jug2_cap, target, target_jug)
    
    # Save to file
    with open("outputs/2-output.txt", "w") as f:
        f.write(tree_output)
    
    # Display results
    print(f"\nNodes explored: {nodes_explored}")
    print(f"Total states in tree: {len(all_states_tree)}")
    
    if solution:
        print(f"Solution found in {len(solution) - 1} steps!")
    else:
        print("No solution found!")
    
    print("\n" + "="*50)
    print("TREE OUTPUT:")
    print("="*50)
    print(tree_output)
    print(f"\nTree output has been saved to 'outputs/2-output.txt'")