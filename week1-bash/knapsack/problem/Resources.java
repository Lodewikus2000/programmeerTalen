package problem;

import java.util.ArrayList;
import java.util.Random;

/**
 * Resources of certain dimension.
 */
public class Resources {
    ArrayList<Integer> resources;

    /**
     * Constructor
     * @param resources the resources in each dimension
     */
    public Resources(ArrayList<Integer> resources) {
        this.resources=resources;
    }
    
    /**
     * Constructor
     * @param dimensions the number of dimensions
     * @param value the value of each resource dimension (all get the same value)
     */
    public Resources(int dimensions,int value)
    {
        resources=new ArrayList<Integer>();
        for (int i=0;i<dimensions;i++)
            resources.add(value);
    }

    /**
     * Add Resources to this Resources
     * @param r2 the resources to add 
     */
    public void add(Resources r2) {
        for (int i=0;i<resources.size();i++)
            resources.set(i, resources.get(i) + r2.resources.get(i) );
    }
    
    /**
     * Subtract Resources from this Resources
     * @param r2 the resources to subtract 
     */
    public void subtract(Resources r2) {
        for (int i=0;i<resources.size();i++)
            resources.set(i, resources.get(i) - r2.resources.get(i) );
    }
    
    /**
     * Check if Resources fit in this Resources
     * @param r2 the resources to fit
     * @return true of r2 fits, false otherwise 
     */
    public boolean can_fit(Resources r2) {
        for (int i=0;i<resources.size();i++)
            if (resources.get(i)<r2.resources.get(i))
                return false;
        return true;
    }
    
    /**
     * Convert Resources to string
     * @return string representation
     */
    public String toString() {
        return "Resources:"+resources;
    }
}
