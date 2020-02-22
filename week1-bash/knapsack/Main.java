
import java.util.Hashtable;

import problem.Problem_instance;
import solvers.Solver;
import solvers.Solver_depth_first;
import solvers.Solver_hill_climbing;
import solvers.Solver_simulated_annealing;
    
/**
 * Main class to run Solver on Problem_instance.
 */
public class Main {

    /**
     * Parses command line arguments and runs Solver on Problem_instance.
     * @param  args command line arguments
     */
    public static void main(String args[]) {
        int verbose=0;
        int nr_items=20;
        int dimensions=2;
        String solver_name="depth_first";
        int nr_iterations=50000;
        int nr_to_unpack=1;
        int max_temperature=1000;
        
        // put arguments in a hashtable for easy lookup
        Hashtable<String, String> arg_values=new Hashtable<String, String>();
        for (int i=0;i<args.length;i+=1)
        {
            String key=args[i];
            if (key.charAt(0)=='-')
            {
                String value=(i+1<args.length) ? args[i+1] : "";
                arg_values.put(key,value);
            }
        }
        
        if (arg_values.containsKey("-help"))
        {
            System.out.println("Knapsack solver, arguments:");
            System.out.println("  -help                            : show this message");
            System.out.println("  -verbose <int>                   : verbosity 0=off 1=on, default: 0");
            System.out.println("  -nr_items <int>                  : number of knapsack items, default: "+nr_items);
            System.out.println("  -dimensions <int>                : number of resources dimensions, default: "+dimensions);
            System.out.println("  -solver depth_first              : use the depth_first solver (default)");
            System.out.println("          hill_climbing            : use the hill_climbing solver (iterative)");
            System.out.println("          simulated_annealing      : use the simulated_annealing solver (iterative)");
            
            System.out.println("arguments for iterative algorithms:");
            System.out.println("  -nr_iterations <int>             : number of iteration, default:"+nr_iterations);
            System.out.println("  -nr_to_unpack <int>              : number of items to unpack per iteration, default: "+nr_to_unpack);
            System.out.println("arguments for simulated annealing:");
            System.out.println("  -max_temperature <int>           : max or start temperature, default: "+max_temperature);
            System.exit(0);
        }
        
        String arg_value;
        if ((arg_value=arg_values.get("-verbose"))!=null)
            verbose=Integer.valueOf(arg_value);
        System.out.println("verbose:"+verbose);
        if ((arg_value=arg_values.get("-nr_items"))!=null)
            nr_items=Integer.valueOf(arg_value);
        System.out.println("nr_items:"+nr_items);
        if ((arg_value=arg_values.get("-dimensions"))!=null)
            dimensions=Integer.valueOf(arg_value);
        System.out.println("dimensions:"+dimensions);
        if ((arg_value=arg_values.get("-solver"))!=null)
            solver_name=arg_value;
        System.out.println("solver_name:"+solver_name);
        if ((arg_value=arg_values.get("-nr_iterations"))!=null)
            nr_iterations=Integer.valueOf(arg_value);
        System.out.println("nr_iterations:"+nr_iterations);
        if ((arg_value=arg_values.get("-nr_to_unpack"))!=null)
            nr_to_unpack=Integer.valueOf(arg_value);
        System.out.println("nr_to_unpack:"+nr_to_unpack);
        if ((arg_value=arg_values.get("-max_temperature"))!=null)
            max_temperature=Integer.valueOf(arg_value);
        System.out.println("max_temperature:"+max_temperature);
        
        // create problem instance
        Problem_instance problem_instance=new Problem_instance(nr_items,dimensions);
        System.out.println(problem_instance);

        Solver solver=null;
        switch(solver_name) 
        { 
            case "depth_first":
                solver=new Solver_depth_first(verbose>0);
                break; 
            case "hill_climbing": 
                solver=new Solver_hill_climbing(verbose>0,nr_iterations,nr_to_unpack);
                break; 
            case "simulated_annealing": 
                solver=new Solver_simulated_annealing(verbose>0,nr_iterations,nr_to_unpack,max_temperature);
                break; 
            default: 
                System.out.println("unkown solver '"+solver_name+"'"); 
        } 
        
        // solve problem instance with solver
        if (solver!=null)
            problem_instance.solve(solver);        
    } 
}
