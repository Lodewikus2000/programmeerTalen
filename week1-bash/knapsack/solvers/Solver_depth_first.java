package solvers;

import problem.Knapsack;
import problem.Items;

/**
 * Depth first solver that iterates over all possible combinations of
 * items and will therefore give the optimal combination but will take
 * a long time. Time exponential in the number of items.
 */
public class Solver_depth_first extends Solver {
    /**
     * Constructor
     * @param verbose if true prints each Knapsack found in each iteration 
     *        otherwise only when a better one if found
     */
    public Solver_depth_first(boolean verbose) { super(verbose); }
    
    /**
     * Overrides Solver.solve() to start depth first search
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    @Override
    public void solve(Items items,Knapsack knapsack) {
        solve_recursive(0,items,knapsack);
    }

    /**
     * Recursively first packs and then doesn't pack each item.
     * @param i index of item to first packs and then doesn't pack
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    void solve_recursive(int i,Items items,Knapsack knapsack) {
        if (update_best_value(knapsack) || verbose)
            print_knapsack(knapsack);
        if (i<items.nr_items()) {
            // pack
            if (knapsack.pack(i,items)) {
                solve_recursive(i+1,items,knapsack);
                knapsack.unpack(i,items);
            }
            // don't pack
            solve_recursive(i+1,items,knapsack);
        }
    }

}
