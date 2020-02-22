package solvers;

import problem.Knapsack;
import problem.Items;

/**
 * Base class of all solvers.
 */
public abstract class Solver {
    protected int iteration;
    int best_value;
    protected boolean verbose;

    /**
     * Constructor
     * @param verbose if true prints each Knapsack found in each iteration 
     *        otherwise only when a better one if found
     */
    public Solver(boolean verbose) { this.verbose=verbose; }

    /**
     * Called to start a new run to solve the knapsack problem with Items and Knapsack.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    public void solve_base(Items items,Knapsack knapsack) {
        iteration=0;
        best_value=-1;
        System.out.println("iteration:"+iteration+" solve_begin_time:"+System.currentTimeMillis());
        this.solve(items,knapsack);
        System.out.println("iteration:"+iteration+" solve_end_time:"+System.currentTimeMillis());
    }

    /**
     * Check if a higher value Knapsack is found for this run of the solver.
     * @param knapsack knapsack to check against best known value
     * @return true best know is improved
     */
    protected boolean update_best_value(Knapsack knapsack) {
        iteration+=1;
        if (knapsack.get_value()>best_value)
        {
            best_value=knapsack.get_value();
            return true;
        }
        if (iteration%100000==0)
            System.out.println("iteration:"+iteration+" time:"+System.currentTimeMillis()+
                               " best_value:"+best_value);
        return false;
    }

    /**
     * Prints the current iteration, time and best_value and optionally the knapsack knapsack
     * @param print_knapsack if true the knapsack is also printed
     */
    protected void print_knapsack(Knapsack knapsack) {
        System.out.println("iteration:"+iteration+" time:"+System.currentTimeMillis()+
                           " best_value:"+best_value+" "+knapsack);
    }

    /**
     * Abstract method to override by base classes to implement the solver.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    protected abstract void solve(Items items,Knapsack knapsack);
    
}
