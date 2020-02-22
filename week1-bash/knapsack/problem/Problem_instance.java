package problem;

import java.util.Random;

import problem.Knapsack;
import problem.Items;
import solvers.Solver;

/**
 * Generates a random problem instant. As the random number generator
 * is seeded with a constant ('0') it will generate the same random
 * problem instance for the same 'nr_items' and 'dimensions' arguments
 * every time. This is useful for testing and comparing different
 * algorithms.
 */
public class Problem_instance {
    Items items;
    Knapsack knapsack;

    /**
     * Constructor
     * @param nr_items number of items in the Problem_instance
     * @param dimensions the resource dimenions for all items
     */
    public Problem_instance(int nr_items,
                            int dimensions) {
        int average_value=7000;
        int offset_value=3000;
        double resources_ratio=0.6;
        items=Items.random_items(new Random(0),nr_items,dimensions,
                                 average_value,offset_value);
        int knapsack_resources=(int)(average_value*nr_items*resources_ratio);
        knapsack=new Knapsack(new Resources(dimensions,knapsack_resources),
                              items.nr_items());
    }
    
    /**
     * Solve the Problem_instant
     * @param solver solver to use to solve the Problem_instant
     */
    public void solve(Solver solver) {
        solver.solve_base(items,knapsack);
    }

    /**
     * Convert Problem_instance to string
     * @return string representation
     */
    public String toString() {
        return ""+items+knapsack;
    }
}
