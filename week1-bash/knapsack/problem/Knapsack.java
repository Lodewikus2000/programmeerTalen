package problem;

/**
 * A Knapsack. Its Inventory can be filled with Items to get a better
 * value until no more Items can fit because of its limited Resources.
 */
public class Knapsack {
    Inventory inventory;
    Resources resources;
    int value;

    /**
     * Constructor
     * @param resources the Resources
     * @param size number of available Items 
     */
    public Knapsack(Resources resources,int size) {
        inventory=new Inventory(size);
        this.resources=resources;
        value=0;
    }

    /**
     * Pack an item in the knapsack, the item packed is the one at 'index' of the items list.
     * @param index the index of the Item that is packed in the Knapsack
     * @param items the list of items the index refers to
     * @return true if succesfully packed, false otherwise
     */
    public boolean pack(int index,Items items) {
        if (inventory.has_index(index))
            return false;
        Item item=items.get(index);
        if (!resources.can_fit(item.get_resources()))
            return false;
        resources.subtract(item.get_resources());
        value+=item.get_value();
        inventory.add_index(index);
        return true;
    }

    /**
     * Unpack an item from the knapsack, the item unpacked is the one at 'index' of the items list.
     * @param index the index of the Item that is unpacked from the Knapsack
     * @param items the list of items the index refers to
     * @return true if succesfully unpacked, false otherwise
     */
    public boolean unpack(int index,Items items) {
        if (!inventory.has_index(index))
            return false;
        Item item=items.get(index);
        resources.add(item.get_resources());
        value-=item.get_value();
        inventory.remove_index(index);
        return true;
    }

    /**
     * Get the Knapsack's Inventory
     * @return the Inventory
     */
    public Inventory get_inventory() {
        return inventory;
    }

    /**
     * Get the Knapsack's current value
     * @return the current value
     */
    public int get_value() {
        return value;
    }
    
    /**
     * Convert Knapsack to string
     * @return string representation
     */
    public String toString() {
        return "Knapsack: value:"+value+" "+resources+" "+inventory;
    }
}
