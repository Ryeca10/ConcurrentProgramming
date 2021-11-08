
import java.util.*;
import java.util.concurrent.Semaphore;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;
    private int count = 0;
    Semaphore [] shelfAccess = new Semaphore []{new Semaphore(1), new Semaphore(1), new Semaphore(1)};
    Semaphore cashiersAccess  = new Semaphore(4, true);
    private int customerCount = 0;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        rnd = new Random();
        shoppingCart = new LinkedList<BreadType>();
        fillShoppingCart();
        shopTime = rnd.nextInt(15 + 1 - 5) + 5;
        checkoutTime = rnd.nextInt(15 + 1 - 5) + 5;
        this.bakery = bakery;
    }

    private void breadBattle()
    {
        for (BreadType bread : shoppingCart)
            {       
                    if (bread == BreadType.RYE)
                    {
                        try
                        {
                            shelfAccess[0].acquire();
                        }
                        catch(InterruptedException e)
                        {
                            System.out.println("shelf access 0 *******");
                        }
                        bakery.takeBread(bread);
                        shelfAccess[0].release();
                    } 
                    else if (bread == BreadType.SOURDOUGH)
                    {
                           try
                        {
                            shelfAccess[1].acquire();
                        }
                        catch(InterruptedException e)
                        {
                            System.out.println("shelf access 1 *******");
                        }
                        bakery.takeBread(bread);
                        shelfAccess[1].release();
                    }
                    else if (bread == BreadType.WONDER)
                    {
                        try
                        {
                            shelfAccess[2].acquire();
                        }
                        catch(InterruptedException e)
                        {
                            System.out.println("shelf access 2 *******");
                        }
                        bakery.takeBread(bread);
                        shelfAccess[2].release();
                    }   
            }
    }

    private void paymentProcess()
    {
        try
        {
            cashiersAccess.acquire();
        }
        catch(InterruptedException e)
        {
            System.out.println("cashiersAccess access *******");
        }
        bakery.addSales(getItemsValue());
        customerCount++;
        cashiersAccess.release();
    }
    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
            breadBattle();
            paymentProcess();
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
        }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}