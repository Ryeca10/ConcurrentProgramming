import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.Random;
import java.util.concurrent.TimeUnit;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int ALLOWED_CUSTOMERS = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread; 
    private ExecutorService executorService;
    private float sales = 0;

    // TO DO

    private Random rnd;
    private Customer customer;
    private Semaphore storeLimit = new Semaphore(ALLOWED_CUSTOMERS);
    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {

        
        int breadLeft = availableBread.get(bread);
        

        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);
        // TODO
        rnd = new Random();
        int currentCustomers = rnd.nextInt(TOTAL_CUSTOMERS + 1 - 0) + 0;
        Customer [] customer = new Customer[currentCustomers];
        for (int i = 0 ; i < currentCustomers ; i++)
        {

            customer[i] = new Customer(this);
            String s = customer[i].toString();
            System.out.println(s);
        }

        executorService = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);

        for (int i = 0 ; i < currentCustomers ; i++)
        {
             executorService.execute(customer[i]);            
        }
        
        executorService.shutdown();
    }

}