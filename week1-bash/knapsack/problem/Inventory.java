package problem;

import java.util.ArrayList;
import java.util.Random;
import java.util.Collections;

/**
 * Inventory keeps track which items are in or out by storing the
 * index of the items in the in or out list.
 */
public class Inventory {
    ArrayList<Integer> indices_in;
    ArrayList<Integer> indices_out;

    /**
     * Constructor
     * @param size initializes with 'size' items that are out.
     */
    public Inventory(int size) {
        indices_in =new ArrayList<Integer>();
        indices_out=new ArrayList<Integer>();
        for (int i=0;i<size;i++)
            indices_out.add(i);
    }

    /**
     * Check if 'index' is in the Inventory.
     * @param index index of the item
     * @return true if in, false otherwise
     */
    public boolean has_index(int index) {
        return indices_in.contains(index);
    }

    /**
     * Moves index from in to out.
     * @param index index of the item
     */
    public void add_index(int index) {
        indices_in.add(index);
        indices_out.remove((Integer)index);
    }

    /**
     * Moves index from out to in.
     * @param index index of the item
     */
    public void remove_index(int index) {
        indices_in.remove((Integer)index);
        indices_out.add(index);
    }

    /**
     * Get the number of indices in the Inventory
     * @return number of items in
     */
    public int index_in_size() {
        return indices_in.size();
    }

    /**
     * Get a random index from those that are in the Inventory
     * @param random the random generator
     * @return index index of the item
     */
    public Integer get_random_index_in(Random random) {
        int i=random.nextInt(indices_in.size());
        return indices_in.get(i);
    }

    /**
     * Get the number of items out of the Inventory
     * @return number of items out
     */
    public int index_out_size() {
        return indices_out.size();
    }
    
    /**
     * Returns a copy of the out randomly shuffled
     */
    public ArrayList<Integer> get_randomly_shuffled_out_indices() {
        ArrayList<Integer> random_shuffle=new ArrayList<Integer>(indices_out);
        Collections.shuffle(random_shuffle);
        return random_shuffle;
    }

    /**
     * Convert Inventory to string
     * @return string representation
     */
    public String toString() {
        return "Inventory: in:"+indices_in+" out:"+indices_out;
    }
}
