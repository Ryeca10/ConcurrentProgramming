import java.io.*;
import java.util.*;


public class TextSwap {

	 

    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) 
    {
        // TODO: Implement me!
    	List <Interval> tmpList  = new ArrayList<Interval>();



    	for (int i = 0 ; i < numChunks * chunkSize; i = i + chunkSize)
    	{
    	    int a = i;
    	    int b = i + chunkSize - 1;
    	    System.out.println("(" + a + "," + b + ")");
    	    Interval tmp = new Interval (a,b);
    	    tmpList.add(tmp);
    	}

    	Interval [] returnArr = new Interval [numChunks];

    	int i = 0;

    	for (Interval intrvl : tmpList)
    	{
    	    returnArr[i] = intrvl;
    	    i++;
    	}
    	    
    	 
        return returnArr;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        System.out.println(labels);
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) 
    {
        // creates the intervals, 
        // runs the Swapper threads, 
        // returns the reordered buffer that will be written to the new file

        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);

		char [] buff = new char [content.length()];
        int [] indexes = new int[labels.size()];

		int j = 0;

		for (Character c: labels)
		{
		    for (int i = 0 ; i < content.length() ; i = i + chunkSize)
            {
                if ( Character.toLowerCase(c) == Character.toLowerCase(content.charAt(i)) )
                {
                    indexes[j] = i; 
                }
            }

            j++;
		}
       
       Swapper s;
       Thread t[] = new Thread[indexes.length];

       try
       {
            for (int i = 0 ; i < indexes.length ; i++)
            {
                    s = new Swapper(intervals[i], content, buff, indexes[i]) ; 
                    t[i]  = new Thread (s);  
                    t[i].start();    
            }
        }
        catch(RuntimeException e)
        {
            System.out.println("** RuntimeException");
        }


        try 
        {
            for (int i = 0 ; i < indexes.length ; i++)
            {
                t[i].join();
            }
        } 
        
        catch(InterruptedException e)
        {
             System.out.println("** InterruptedException");
        }

        return buff;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception 
    {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) 
    {
        if (args.length != 2) 
        {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }


        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);

        if (contents.length() / chunkSize > 26) 
        {
            System.out.println("Chunk size too small");
            return;
        }
        else if (contents.length() != chunkSize * contents.length() / chunkSize ||
                 chunkSize < 1)
        {
               System.out.println( "Chunk size must be positive and file size must be a multiple of the chunk size" );
               return;
        }

        try 
        {
            contents = readFile(args[1]);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } 
        catch (Exception e) 
        {
            System.out.println("Error with IO.");
            return;
        }
    }
}	// TextSwap




