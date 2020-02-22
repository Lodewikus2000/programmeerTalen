package solvers;

import java.util.Random;
import java.util.ArrayList;

import problem.Knapsack;
import problem.Items;
import problem.Inventory;

/**
 * Base class of all iterative solvers.
 */
public abstract class Solver_iterative extends Solver {
    protected int nr_iterations;
    int nr_to_unpack;
    protected Random random;
    ArrayList<Integer> undo_unpack_indices;
    ArrayList<Integer> undo_pack_indices;

    /**
     * Constructor
     * @param verbose if true prints each Knapsack found in each iteration 
     *        otherwise only when a better one if found
     * @param nr_iterations number of iterations to search
     * @param nr_to_unpack number of random items to try to unpack in each iteration
     */
    public Solver_iterative(boolean verbose,int nr_iterations,int nr_to_unpack) {
        super(verbose);
        this.nr_iterations=nr_iterations;
        this.nr_to_unpack=nr_to_unpack;
        random=new Random();
        undo_unpack_indices=new ArrayList<Integer>();
        undo_pack_indices=new ArrayList<Integer>();
    }
    
    /**
     * Unpack 'nr_to_unpack' random items from Knapsack.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    protected void unpack(Items items,Knapsack knapsack)
    {
        Inventory inventory=knapsack.get_inventory();
        undo_unpack_indices.clear();
        //System.out.print("  unpack:");
        for (int i=0;i<nr_to_unpack;i++) {
            if (inventory.index_in_size()==0)
                break;
            int index=inventory.get_random_index_in(random);
            knapsack.unpack(index,items);
            //System.out.print(" "+index);
            undo_unpack_indices.add(index);
        }
        // System.out.println("");
    }
    
    /**
     * Try and pack all unpacked items in a random order.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    protected void pack(Items items,Knapsack knapsack)
    {
        Inventory inventory=knapsack.get_inventory();
        undo_pack_indices.clear();
        ArrayList<Integer>  random_shuffle_out=inventory.get_randomly_shuffled_out_indices();
        //System.out.print("  pack:");
        for (int index: random_shuffle_out)
            if (knapsack.pack(index,items))
            {
                undo_pack_indices.add(index);
                //System.out.print(" "+index);
            }
        //System.out.println("");
    }
    
    /**
     * Undo the last pack of items in Knapsack.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    protected void undo_last_pack(Items items,Knapsack knapsack)
    {
        //System.out.print("  undo_last_pack:");
        for (int index: undo_pack_indices)
        {
            knapsack.unpack(index,items);
            //System.out.print(" "+index);
        }
        undo_pack_indices.clear();
        //System.out.println("");
    }
    
    /**
     * Undo the last unpack of items from Knapsack.
     * @param items items to try and fit in the knapsack to get best value
     * @param knapsack knapsack to try and fit the items in
     */
    protected void undo_last_unpack(Items items,Knapsack knapsack)
    {
        //System.out.print("  undo_last_unpack:");
        for (int index: undo_unpack_indices)
        {
            knapsack.pack(index,items);
            //System.out.print(" "+index);
        }
        //System.out.println("");
        undo_unpack_indices.clear();
    }

}
