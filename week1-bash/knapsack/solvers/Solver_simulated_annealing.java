package solvers;

import java.util.Random;
import java.lang.Math;

import problem.Knapsack;
import problem.Items;
import problem.Inventory;

/**
 * Simulated Annealing first tries to pack all items in a random
 * order. Then it iteratively unpacks and packs items randomly from
 * the Knapsack and when the value gets worse there is a chance it
 * will undo the last pack and unpacks. The chance is dependent on the
 * amount the value gets worse and the temperature. The temperature is
 * at its maximum in the beginnning and drops linearly with the number of
 * iterations to zero.
 */
public class Solver_simulated_annealing extends Solver_iterative {
    int max_temperature;

    /**
     * Constructor
     * @param verbose if true prints each Knapsack found in each iteration 
     *        otherwise only when a better one if found
     * @param nr_iterations number of iterations to search
     * @param nr_to_unpack number of random items to try to unpack in each iteration
     * @param max_temperature temperature at the start of the search
     */
    public Solver_simulated_annealing(boolean verbose,int nr_iterations,int nr_to_unpack,int max_temperature) {
        super(verbose,nr_iterations,nr_to_unpack);
        this.max_temperature=max_temperature;
    }

    /**
     * Get the current temperature (starts at max_temperature and
     * linearly drop to zero over nr_iterations iterations).
     * @param iteration current iteration of the search
     * @return the current temperature
     */
    public double current_temperature(int iteration) {
        return max_temperature*(1-iteration/(double)nr_iterations);
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
            boolean accept=true;
            if (improvement<0)
            {
                double current_temperature=current_temperature(iteration);
                double chance=0;
                if (current_temperature>0)
                    chance=Math.exp(improvement/current_temperature);
                System.out.println("improvement:"+improvement+" current_temperature:"+current_temperature+" chance:"+chance);
                if (random.nextDouble()>chance)
                {
                    undo_last_pack(items,knapsack);
                    undo_last_unpack(items,knapsack);
                    accept=false;
                }
            }
            //System.out.println("value_old:"+value_old+" improvement: "+improvement+" accept:"+accept);
            if (update_best_value(knapsack) || accept && verbose)
                print_knapsack(knapsack);
        }
    }
}
