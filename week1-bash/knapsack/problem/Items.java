package problem;

import java.util.ArrayList;
import java.util.Random;

/**
 * List of all the Items. Items are refered to by the index in this list.
 */
public class Items {
    ArrayList<Item> items;

    /**
     * Constructor
     */
    public Items() {
        items=new ArrayList<Item>();
    }
    
    /**
     * Static function that Constructs a random list of Items.
     * @param random random generator to use
     * @param nr_items number of items to generate
     * @param dimensions resource dimension
     * @param average_value average value of each resource dimension and value of the item
     * @param offset_value maximum random offset from average_value (positive and negative offset)
     * @return random list of Items
     */
    public static Items random_items(Random random,int nr_items,int dimensions,
                                     int average_value,int offset_value) {
        Items items=new Items();
        for (int i=0;i<nr_items;i++) {
            ArrayList<Integer> res=new ArrayList<Integer>();
            for (int d=0;d<dimensions;d++){
                res.add(average_value+random.nextInt()%offset_value);
            }
            Item item=new Item(average_value+random.nextInt()%offset_value,
                               new Resources(res));
            items.add(item);
        }
        return items;
    }

    /**
     * Add Item to the list
     * @param item Item to add
     */
    public void add(Item item) {
        items.add(item);
    }
    
    /**
     * Number of Items in the list
     * @return number of Items
     */
    public int nr_items() {
        return items.size();
    }

    /**
     * Get an Item from the list
     * @param index index of the Item
     * @return the Item at index
     */
    public Item get(int index) {
        return items.get(index);
    }

    /**
     * Convert Items to string
     * @return string representation
     */
    public String toString() {
        String s="Items:\n";
        for (int i=0;i<items.size();i++)
        {
            s+="  "+items.get(i)+"\n";
        }
        return s;
    }
}
