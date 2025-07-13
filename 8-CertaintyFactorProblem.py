from typing import Dict, List, Tuple, Optional, Any
from collections import defaultdict
import math

class CertaintyFactor:
    """
    Certainty Factor system for handling uncertain knowledge in expert systems.
    CF values range from -1 (definitely false) to +1 (definitely true).
    """
    
    def __init__(self):
        self.facts = {}  # fact -> CF value
        self.rules = []  # list of Rule objects
        self.rule_counter = 0
        
    def add_fact(self, fact: str, cf_value: float):
        """Add a fact with its certainty factor."""
        if not (-1 <= cf_value <= 1):
            raise ValueError("CF value must be between -1 and 1")
        self.facts[fact] = cf_value
        
    def get_cf(self, fact: str) -> float:
        """Get the certainty factor for a fact."""
        return self.facts.get(fact, 0.0)
    
    def add_rule(self, premises: List[str], conclusion: str, cf_rule: float):
        """Add a rule with premises, conclusion, and rule CF."""
        rule_id = f"R{self.rule_counter}"
        self.rule_counter += 1
        rule = Rule(rule_id, premises, conclusion, cf_rule)
        self.rules.append(rule)
        return rule
    
    def combine_cf_and(self, cf1: float, cf2: float) -> float:
        """Combine CFs using AND operation (conjunction)."""
        return min(cf1, cf2)
    
    def combine_cf_or(self, cf1: float, cf2: float) -> float:
        """Combine CFs using OR operation (disjunction)."""
        return max(cf1, cf2)
    
    def combine_cf_parallel(self, cf1: float, cf2: float) -> float:
        """
        Combine CFs from parallel evidence using the standard CF combination formula.
        This is used when multiple rules conclude the same fact.
        """
        if cf1 == 0:
            return cf2
        if cf2 == 0:
            return cf1
        
        if cf1 > 0 and cf2 > 0:
            return cf1 + cf2 - (cf1 * cf2)
        elif cf1 < 0 and cf2 < 0:
            return cf1 + cf2 + (cf1 * cf2)
        else:
            return (cf1 + cf2) / (1 - min(abs(cf1), abs(cf2)))
    
    def evaluate_rule(self, rule: 'Rule') -> float:
        """Evaluate a rule and return the CF for its conclusion."""
        premise_cfs = []
        
        for premise in rule.premises:
            cf = self.get_cf(premise)
            premise_cfs.append(cf)
        
        # Combine premise CFs using AND operation
        combined_premise_cf = premise_cfs[0]
        for cf in premise_cfs[1:]:
            combined_premise_cf = self.combine_cf_and(combined_premise_cf, cf)
        
        # Calculate conclusion CF: CF(conclusion) = CF(premise) * CF(rule)
        conclusion_cf = combined_premise_cf * rule.cf_rule
        
        return conclusion_cf
    
    def forward_chain(self, max_iterations: int = 10) -> Dict[str, float]:
        """
        Perform forward chaining to derive new facts.
        Returns the final state of all facts.
        """
        iteration = 0
        changed = True
        
        while changed and iteration < max_iterations:
            changed = False
            iteration += 1
            
            print(f"\n--- Forward Chaining Iteration {iteration} ---")
            
            for rule in self.rules:
                # Check if all premises are satisfied (CF > 0)
                can_fire = all(self.get_cf(premise) > 0 for premise in rule.premises)
                
                if can_fire:
                    new_cf = self.evaluate_rule(rule)
                    old_cf = self.get_cf(rule.conclusion)
                    
                    if rule.conclusion not in self.facts:
                        # First time deriving this fact
                        self.facts[rule.conclusion] = new_cf
                        changed = True
                        print(f"Rule {rule.rule_id} fires: {rule.conclusion} = {new_cf:.3f}")
                    else:
                        # Combine with existing CF
                        combined_cf = self.combine_cf_parallel(old_cf, new_cf)
                        if abs(combined_cf - old_cf) > 0.001:  # Significant change
                            self.facts[rule.conclusion] = combined_cf
                            changed = True
                            print(f"Rule {rule.rule_id} fires: {rule.conclusion} updated from {old_cf:.3f} to {combined_cf:.3f}")
        
        return self.facts.copy()
    
    def backward_chain(self, goal: str, depth: int = 0) -> float:
        """
        Perform backward chaining to determine CF of a goal.
        Returns the certainty factor for the goal.
        """
        indent = "  " * depth
        print(f"{indent}Trying to prove: {goal}")
        
        # Check if goal is already known
        if goal in self.facts:
            cf = self.facts[goal]
            print(f"{indent}Known fact: {goal} = {cf:.3f}")
            return cf
        
        # Find rules that can conclude the goal
        applicable_rules = [rule for rule in self.rules if rule.conclusion == goal]
        
        if not applicable_rules:
            print(f"{indent}No rules found for {goal}")
            return 0.0
        
        goal_cfs = []
        
        for rule in applicable_rules:
            print(f"{indent}Trying rule {rule.rule_id}: {rule}")
            
            # Recursively evaluate premises
            premise_cfs = []
            all_premises_satisfied = True
            
            for premise in rule.premises:
                premise_cf = self.backward_chain(premise, depth + 1)
                premise_cfs.append(premise_cf)
                if premise_cf <= 0:
                    all_premises_satisfied = False
            
            if all_premises_satisfied:
                # Combine premise CFs
                combined_premise_cf = premise_cfs[0]
                for cf in premise_cfs[1:]:
                    combined_premise_cf = self.combine_cf_and(combined_premise_cf, cf)
                
                # Calculate conclusion CF
                conclusion_cf = combined_premise_cf * rule.cf_rule
                goal_cfs.append(conclusion_cf)
                print(f"{indent}Rule {rule.rule_id} contributes: {conclusion_cf:.3f}")
            else:
                print(f"{indent}Rule {rule.rule_id} cannot fire (premises not satisfied)")
        
        # Combine CFs from all applicable rules
        if goal_cfs:
            final_cf = goal_cfs[0]
            for cf in goal_cfs[1:]:
                final_cf = self.combine_cf_parallel(final_cf, cf)
            
            # Store the derived fact
            self.facts[goal] = final_cf
            print(f"{indent}Final CF for {goal}: {final_cf:.3f}")
            return final_cf
        
        print(f"{indent}Cannot prove {goal}")
        return 0.0
    
    def display_facts(self):
        """Display all known facts and their certainty factors."""
        print("\n=== Known Facts ===")
        for fact, cf in sorted(self.facts.items()):
            confidence = self.cf_to_confidence(cf)
            print(f"{fact}: CF = {cf:.3f} ({confidence})")
    
    def cf_to_confidence(self, cf: float) -> str:
        """Convert CF value to human-readable confidence level."""
        if cf >= 0.8:
            return "Very High Confidence"
        elif cf >= 0.6:
            return "High Confidence"
        elif cf >= 0.4:
            return "Moderate Confidence"
        elif cf >= 0.2:
            return "Low Confidence"
        elif cf > 0:
            return "Very Low Confidence"
        elif cf == 0:
            return "Unknown"
        elif cf > -0.2:
            return "Very Low Disbelief"
        elif cf > -0.4:
            return "Low Disbelief"
        elif cf > -0.6:
            return "Moderate Disbelief"
        elif cf > -0.8:
            return "High Disbelief"
        else:
            return "Very High Disbelief"


class Rule:
    """Represents a rule in the expert system."""
    
    def __init__(self, rule_id: str, premises: List[str], conclusion: str, cf_rule: float):
        self.rule_id = rule_id
        self.premises = premises
        self.conclusion = conclusion
        self.cf_rule = cf_rule
    
    def __str__(self):
        premises_str = " AND ".join(self.premises)
        return f"IF {premises_str} THEN {self.conclusion} (CF = {self.cf_rule})"


def demo_medical_diagnosis():
    """Demonstrate CF system with a medical diagnosis scenario."""
    print("=== Medical Diagnosis Expert System ===")
    
    cf_system = CertaintyFactor()
    
    # Initial symptoms (evidence)
    cf_system.add_fact("fever", 0.8)
    cf_system.add_fact("cough", 0.7)
    cf_system.add_fact("fatigue", 0.6)
    cf_system.add_fact("body_aches", 0.5)
    
    # Diagnostic rules
    cf_system.add_rule(
        ["fever", "cough"], 
        "respiratory_infection", 
        0.7
    )
    
    cf_system.add_rule(
        ["fever", "body_aches"], 
        "flu", 
        0.8
    )
    
    cf_system.add_rule(
        ["respiratory_infection", "fatigue"], 
        "pneumonia", 
        0.6
    )
    
    cf_system.add_rule(
        ["flu", "cough"], 
        "influenza_a", 
        0.9
    )
    
    cf_system.add_rule(
        ["fever", "cough", "fatigue"], 
        "covid_19", 
        0.75
    )
    
    print("Initial symptoms:")
    cf_system.display_facts()
    
    # Forward chaining
    print("\n=== Forward Chaining ===")
    final_facts = cf_system.forward_chain()
    
    print("\nFinal diagnosis results:")
    cf_system.display_facts()
    
    return cf_system


def demo_car_diagnosis():
    """Demonstrate CF system with car troubleshooting."""
    print("\n\n=== Car Diagnosis Expert System ===")
    
    cf_system = CertaintyFactor()
    
    # Initial observations
    cf_system.add_fact("engine_wont_start", 0.9)
    cf_system.add_fact("lights_dim", 0.6)
    cf_system.add_fact("clicking_sound", 0.8)
    
    # Diagnostic rules
    cf_system.add_rule(
        ["engine_wont_start", "lights_dim"], 
        "battery_weak", 
        0.8
    )
    
    cf_system.add_rule(
        ["battery_weak", "clicking_sound"], 
        "battery_dead", 
        0.9
    )
    
    cf_system.add_rule(
        ["engine_wont_start", "clicking_sound"], 
        "starter_problem", 
        0.7
    )
    
    cf_system.add_rule(
        ["battery_dead"], 
        "needs_jump_start", 
        0.95
    )
    
    cf_system.add_rule(
        ["starter_problem"], 
        "replace_starter", 
        0.85
    )
    
    print("Initial observations:")
    cf_system.display_facts()
    
    # Forward chaining
    print("\n=== Forward Chaining ===")
    cf_system.forward_chain()
    
    print("\nFinal diagnosis:")
    cf_system.display_facts()
    
    return cf_system


def demo_backward_chaining():
    """Demonstrate backward chaining with a simple knowledge base."""
    print("\n\n=== Backward Chaining Demo ===")
    
    cf_system = CertaintyFactor()
    
    # Facts
    cf_system.add_fact("rainy", 0.8)
    cf_system.add_fact("cold", 0.7)
    
    # Rules
    cf_system.add_rule(["rainy"], "wet_ground", 0.9)
    cf_system.add_rule(["cold", "wet_ground"], "slippery", 0.8)
    cf_system.add_rule(["slippery"], "dangerous_driving", 0.85)
    
    print("Known facts:")
    cf_system.display_facts()
    
    print("\n=== Backward Chaining for 'dangerous_driving' ===")
    goal_cf = cf_system.backward_chain("dangerous_driving")
    
    print(f"\nConclusion: dangerous_driving has CF = {goal_cf:.3f}")
    print(f"Confidence level: {cf_system.cf_to_confidence(goal_cf)}")
    
    return cf_system


def demo_cf_combinations():
    """Demonstrate different CF combination methods."""
    print("\n\n=== CF Combination Methods Demo ===")
    
    cf_system = CertaintyFactor()
    
    # Test values
    cf1, cf2 = 0.7, 0.6
    cf3, cf4 = -0.5, 0.8
    cf5, cf6 = -0.3, -0.4
    
    print(f"CF1 = {cf1}, CF2 = {cf2}")
    print(f"AND combination: {cf_system.combine_cf_and(cf1, cf2):.3f}")
    print(f"OR combination: {cf_system.combine_cf_or(cf1, cf2):.3f}")
    print(f"Parallel combination: {cf_system.combine_cf_parallel(cf1, cf2):.3f}")
    
    print(f"\nCF3 = {cf3}, CF4 = {cf4}")
    print(f"Parallel combination (mixed): {cf_system.combine_cf_parallel(cf3, cf4):.3f}")
    
    print(f"\nCF5 = {cf5}, CF6 = {cf6}")
    print(f"Parallel combination (both negative): {cf_system.combine_cf_parallel(cf5, cf6):.3f}")


def demo_uncertainty_propagation():
    """Demonstrate how uncertainty propagates through rule chains."""
    print("\n\n=== Uncertainty Propagation Demo ===")
    
    cf_system = CertaintyFactor()
    
    # Chain of uncertain evidence
    cf_system.add_fact("symptom_a", 0.6)
    cf_system.add_fact("symptom_b", 0.5)
    
    # Chain of rules with different certainties
    cf_system.add_rule(["symptom_a"], "condition_x", 0.8)
    cf_system.add_rule(["symptom_b"], "condition_y", 0.7)
    cf_system.add_rule(["condition_x", "condition_y"], "diagnosis_z", 0.9)
    
    print("Demonstrating uncertainty propagation:")
    print("symptom_a (CF=0.6) -> condition_x")
    print("symptom_b (CF=0.5) -> condition_y")
    print("condition_x AND condition_y -> diagnosis_z")
    
    cf_system.forward_chain()
    
    print("\nFinal results show how uncertainty compounds:")
    cf_system.display_facts()


if __name__ == "__main__":
    # Run all demonstrations
    demo_medical_diagnosis()
    demo_car_diagnosis()
    demo_backward_chaining()
    demo_cf_combinations()
    demo_uncertainty_propagation()
    
    print("\n=== Certainty Factor System Features ===")
    print("✓ CF values from -1 (definitely false) to +1 (definitely true)")
    print("✓ Rule-based inference with uncertain knowledge")
    print("✓ Forward chaining for data-driven reasoning")
    print("✓ Backward chaining for goal-driven reasoning")
    print("✓ CF combination for AND, OR, and parallel evidence")
    print("✓ Uncertainty propagation through rule chains")
    print("✓ Human-readable confidence levels")
    print("✓ Support for multiple evidence sources")