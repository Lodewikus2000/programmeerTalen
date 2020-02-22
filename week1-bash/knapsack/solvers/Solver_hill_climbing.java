package solvers;

import problem.Knapsack;
import problem.Items;
import problem.Inventory;

/**
 * Hill Climber first tries to pack all items in a random
 * order. Then it iteratively unpacks and packs items randomly from
 * the Knapsack and when the value gets worse it will undo the last
 * pack and unpacks.
 */
public class Solver_hill_climbing extends Solver_iterative {

    /**
     * Constructor
     * @param verbose if true prints each Knapsack found in each iteration 
     *        otherwise only when a better one if found
     * @param nr_iterations number of iterations to search
     * @param nr_to_unpack number of random items to try to unpack in each iteration
     */
    public Solver_hill_climbing(boolean verbose,int nr_iterations,int nr_to_unpack) {
        super(verbose,nr_iterations,nr_to_unpack);
    }

    /**
     * Overrides Solver.solve() to start Hill Climber search
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    @Override
    public void solve(Items items,Knapsack knapsack) {
        for (int iteration=0;iteration<nr_iterations;iteration++)
        {
            int value_old=knapsack.get_value();
            unpack(items,knapsack);
            pack(items,knapsack);
            int improvement=knapsack.get_value()-value_old;
            if (improvement<0)
            {
                undo_last_pack(items,knapsack);
                undo_last_unpack(items,knapsack);
            }
            if (update_best_value(knapsack) || verbose)
                print_knapsack(knapsack);
        }
    }
    
}
