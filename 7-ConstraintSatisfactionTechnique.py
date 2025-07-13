from typing import Dict, List, Set, Callable, Optional, Any
from collections import defaultdict
import copy

class CSP:
    """
    Constraint Satisfaction Problem implementation with backtracking search
    and various optimization techniques.
    """
    
    def __init__(self):
        self.variables = set()
        self.domains = {}
        self.constraints = []
        self.neighbors = defaultdict(set)
        
    def add_variable(self, var: str, domain: List[Any]):
        """Add a variable with its domain to the CSP."""
        self.variables.add(var)
        self.domains[var] = set(domain)
        
    def add_constraint(self, constraint_func: Callable, variables: List[str]):
        """Add a constraint function that applies to given variables."""
        self.constraints.append((constraint_func, variables))
        
        # Update neighbor relationships
        for i in range(len(variables)):
            for j in range(i + 1, len(variables)):
                self.neighbors[variables[i]].add(variables[j])
                self.neighbors[variables[j]].add(variables[i])
    
    def is_consistent(self, assignment: Dict[str, Any]) -> bool:
        """Check if current assignment satisfies all constraints."""
        for constraint_func, variables in self.constraints:
            # Only check constraints where all variables are assigned
            if all(var in assignment for var in variables):
                values = [assignment[var] for var in variables]
                if not constraint_func(*values):
                    return False
        return True
    
    def select_unassigned_variable(self, assignment: Dict[str, Any]) -> Optional[str]:
        """Select next variable using Minimum Remaining Values (MRV) heuristic."""
        unassigned = [var for var in self.variables if var not in assignment]
        if not unassigned:
            return None
        
        # MRV: choose variable with smallest domain
        return min(unassigned, key=lambda var: len(self.domains[var]))
    
    def order_domain_values(self, var: str, assignment: Dict[str, Any]) -> List[Any]:
        """Order domain values using Least Constraining Value heuristic."""
        values = list(self.domains[var])
        
        def count_conflicts(value):
            """Count how many values this choice eliminates from neighboring variables."""
            conflicts = 0
            test_assignment = assignment.copy()
            test_assignment[var] = value
            
            for neighbor in self.neighbors[var]:
                if neighbor not in assignment:
                    for neighbor_value in self.domains[neighbor]:
                        test_assignment[neighbor] = neighbor_value
                        if not self.is_consistent(test_assignment):
                            conflicts += 1
                        del test_assignment[neighbor]
            
            return conflicts
        
        # Least constraining value: prefer values that eliminate fewer options
        return sorted(values, key=count_conflicts)
    
    def forward_check(self, var: str, value: Any, assignment: Dict[str, Any]) -> Optional[Dict[str, Set]]:
        """
        Forward checking: remove inconsistent values from neighboring variables.
        Returns updated domains or None if a domain becomes empty.
        """
        new_domains = copy.deepcopy(self.domains)
        test_assignment = assignment.copy()
        test_assignment[var] = value
        
        for neighbor in self.neighbors[var]:
            if neighbor not in assignment:
                to_remove = []
                for neighbor_value in new_domains[neighbor]:
                    test_assignment[neighbor] = neighbor_value
                    if not self.is_consistent(test_assignment):
                        to_remove.append(neighbor_value)
                    del test_assignment[neighbor]
                
                for val in to_remove:
                    new_domains[neighbor].discard(val)
                
                if not new_domains[neighbor]:
                    return None  # Domain wipeout
        
        return new_domains
    
    def ac3(self) -> bool:
        """
        Arc Consistency Algorithm (AC-3) for constraint propagation.
        Returns True if CSP is arc consistent, False if inconsistent.
        """
        queue = []
        
        # Initialize queue with all arcs
        for constraint_func, variables in self.constraints:
            if len(variables) == 2:  # Binary constraints only
                queue.append((variables[0], variables[1], constraint_func))
                queue.append((variables[1], variables[0], constraint_func))
        
        while queue:
            xi, xj, constraint = queue.pop(0)
            
            if self.revise(xi, xj, constraint):
                if not self.domains[xi]:
                    return False  # Domain became empty
                
                # Add arcs (xk, xi) for all neighbors xk of xi except xj
                for xk in self.neighbors[xi]:
                    if xk != xj:
                        for const_func, const_vars in self.constraints:
                            if len(const_vars) == 2 and xk in const_vars and xi in const_vars:
                                queue.append((xk, xi, const_func))
        
        return True
    
    def revise(self, xi: str, xj: str, constraint: Callable) -> bool:
        """Remove inconsistent values from domain of xi."""
        revised = False
        to_remove = []
        
        for x in self.domains[xi]:
            # Check if there exists a value y in domain of xj such that constraint(x, y) is satisfied
            satisfiable = False
            for y in self.domains[xj]:
                if constraint(x, y):
                    satisfiable = True
                    break
            
            if not satisfiable:
                to_remove.append(x)
                revised = True
        
        for val in to_remove:
            self.domains[xi].discard(val)
        
        return revised
    
    def backtrack_search(self, assignment: Optional[Dict[str, Any]] = None, 
                        use_forward_checking: bool = True) -> Optional[Dict[str, Any]]:
        """
        Backtracking search with optional forward checking.
        Returns solution assignment or None if no solution exists.
        """
        if assignment is None:
            assignment = {}
        
        # Check if assignment is complete
        if len(assignment) == len(self.variables):
            return assignment
        
        # Select unassigned variable
        var = self.select_unassigned_variable(assignment)
        if var is None:
            return assignment
        
        # Try values in domain
        for value in self.order_domain_values(var, assignment):
            assignment[var] = value
            
            if self.is_consistent(assignment):
                if use_forward_checking:
                    # Save current domains
                    old_domains = copy.deepcopy(self.domains)
                    
                    # Forward check
                    new_domains = self.forward_check(var, value, assignment)
                    if new_domains is not None:
                        self.domains = new_domains
                        result = self.backtrack_search(assignment, use_forward_checking)
                        if result is not None:
                            return result
                    
                    # Restore domains
                    self.domains = old_domains
                else:
                    result = self.backtrack_search(assignment, use_forward_checking)
                    if result is not None:
                        return result
            
            del assignment[var]
        
        return None
    
    def solve(self, use_ac3: bool = True, use_forward_checking: bool = True) -> Optional[Dict[str, Any]]:
        """
        Solve the CSP using backtracking search with optional optimizations.
        
        Args:
            use_ac3: Whether to use Arc Consistency preprocessing
            use_forward_checking: Whether to use forward checking during search
            
        Returns:
            Solution assignment or None if no solution exists
        """
        # Apply AC-3 preprocessing if requested
        if use_ac3:
            if not self.ac3():
                return None  # CSP is inconsistent
        
        return self.backtrack_search(use_forward_checking=use_forward_checking)


# Example usage and demonstration
def demo_n_queens():
    """Demonstrate CSP solver with N-Queens problem."""
    print("=== N-Queens Problem (4x4) ===")
    
    n = 4
    csp = CSP()
    
    # Add variables (queens) with domain (row positions)
    for col in range(n):
        csp.add_variable(f"Q{col}", list(range(n)))
    
    # Add constraints
    def not_same_row(row1, row2):
        return row1 != row2
    
    def not_same_diagonal(col1, row1, col2, row2):
        return abs(col1 - col2) != abs(row1 - row2)
    
    # Add row constraints
    for i in range(n):
        for j in range(i + 1, n):
            csp.add_constraint(not_same_row, [f"Q{i}", f"Q{j}"])
    
    # Add diagonal constraints
    for i in range(n):
        for j in range(i + 1, n):
            csp.add_constraint(lambda r1, r2, c1=i, c2=j: not_same_diagonal(c1, r1, c2, r2), 
                             [f"Q{i}", f"Q{j}"])
    
    solution = csp.solve()
    
    if solution:
        print("Solution found:")
        board = [['.' for _ in range(n)] for _ in range(n)]
        for col, row in solution.items():
            board[row][int(col[1])] = 'Q'
        
        for row in board:
            print(' '.join(row))
    else:
        print("No solution exists")
    
    return solution

def demo_map_coloring():
    """Demonstrate CSP solver with map coloring problem."""
    print("\n=== Map Coloring Problem (Australia) ===")
    
    csp = CSP()
    
    # Regions
    regions = ['WA', 'NT', 'SA', 'Q', 'NSW', 'V', 'T']
    colors = ['red', 'green', 'blue']
    
    # Add variables
    for region in regions:
        csp.add_variable(region, colors)
    
    # Add adjacency constraints
    def different_colors(color1, color2):
        return color1 != color2
    
    adjacencies = [
        ('WA', 'NT'), ('WA', 'SA'), ('NT', 'SA'), ('NT', 'Q'),
        ('SA', 'Q'), ('SA', 'NSW'), ('SA', 'V'), ('Q', 'NSW'),
        ('NSW', 'V')
    ]
    
    for region1, region2 in adjacencies:
        csp.add_constraint(different_colors, [region1, region2])
    
    solution = csp.solve()
    
    if solution:
        print("Solution found:")
        for region, color in sorted(solution.items()):
            print(f"{region}: {color}")
    else:
        print("No solution exists")
    
    return solution

def demo_sudoku():
    """Demonstrate CSP solver with a simple 4x4 Sudoku."""
    print("\n=== 4x4 Sudoku Problem ===")
    
    csp = CSP()
    
    # Create variables for each cell
    for row in range(4):
        for col in range(4):
            csp.add_variable(f"cell_{row}_{col}", [1, 2, 3, 4])
    
    # Row constraints
    for row in range(4):
        for col1 in range(4):
            for col2 in range(col1 + 1, 4):
                csp.add_constraint(lambda x, y: x != y, 
                                 [f"cell_{row}_{col1}", f"cell_{row}_{col2}"])
    
    # Column constraints
    for col in range(4):
        for row1 in range(4):
            for row2 in range(row1 + 1, 4):
                csp.add_constraint(lambda x, y: x != y, 
                                 [f"cell_{row1}_{col}", f"cell_{row2}_{col}"])
    
    # 2x2 box constraints
    for box_row in range(2):
        for box_col in range(2):
            cells = []
            for r in range(2):
                for c in range(2):
                    cells.append(f"cell_{box_row*2 + r}_{box_col*2 + c}")
            
            # All pairs in box must be different
            for i in range(len(cells)):
                for j in range(i + 1, len(cells)):
                    csp.add_constraint(lambda x, y: x != y, [cells[i], cells[j]])
    
    # Add some initial values (partial puzzle)
    csp.domains["cell_0_0"] = {1}
    csp.domains["cell_0_2"] = {3}
    csp.domains["cell_1_1"] = {2}
    csp.domains["cell_2_2"] = {4}
    
    solution = csp.solve()
    
    if solution:
        print("Solution found:")
        for row in range(4):
            row_str = ""
            for col in range(4):
                row_str += str(solution[f"cell_{row}_{col}"]) + " "
            print(row_str)
    else:
        print("No solution exists")
    
    return solution

if __name__ == "__main__":
    # Run demonstrations
    demo_n_queens()
    demo_map_coloring()
    demo_sudoku()
    
    print("\n=== CSP Solver Features ===")
    print("✓ Backtracking search with chronological backtracking")
    print("✓ Minimum Remaining Values (MRV) variable selection")
    print("✓ Least Constraining Value (LCV) value ordering")
    print("✓ Forward checking for constraint propagation")
    print("✓ Arc Consistency (AC-3) preprocessing")
    print("✓ Flexible constraint definition")
    print("✓ Support for various CSP types (N-Queens, Map Coloring, Sudoku, etc.)")