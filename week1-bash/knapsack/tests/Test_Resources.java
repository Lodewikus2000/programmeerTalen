
import java.util.ArrayList;

import problem.Resources;

public class Test_Resources {
    
    static ArrayList<Integer> one(int dimension,int d)
    {
        ArrayList<Integer> one=new ArrayList<Integer>();
        for (int i=0;i<dimension;i++)
            one.add((i == d) ? 1 : 0);
        return one;
    }

    public static void main(String args[]) {
        int dimension=3;
        int value=10;
        Resources r1=new Resources(dimension,value);
        Resources r2=new Resources(dimension,value*2);
        r1.add(r1);
        System.out.println("r1: "+r1);
        for (int i=0;i<dimension;i++)
        {
            Resources delta=new Resources(one(dimension,i));
            System.out.println("delta: "+delta);
            r2.add(delta);
            System.out.println("r2: "+r2);
            if (r1.can_fit(r2))
            {
                System.err.println("error: r1 should NOT be able to fit r2");
                System.exit(-1);
            }
            r2.subtract(delta);
            System.out.println("r2: "+r2);
            if (!r1.can_fit(r2))
            {
                System.err.println("error: r1 should be able to fit r2");
                System.exit(-1);
            }
        }
    }

}
