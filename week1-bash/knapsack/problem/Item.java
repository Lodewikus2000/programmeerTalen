package problem;

import java.util.Random;

/**
 * Item with a value and the amount of resources it requires to fit.
 */
public class Item {
    int value;
    Resources resources;

    /**
     * Constructor
     * @param value value of the item
     * @param resources the resources the item requires to fit
     */
    public Item(int value,Resources resources) {
        this.value=value;
        this.resources=resources;
    }

    /**
     * Get the value
     * @return the value
     */
    public int get_value() {
        return this.value;
    }
    
    /**
     * Get the required resources
     * @return the Resources
     */
    public Resources get_resources() {
        return this.resources;
    }
    
    /**
     * Convert Item to string
     * @return string representation
     */
    public String toString() {
        return "Item: value:"+value+" "+resources;
    }
}
