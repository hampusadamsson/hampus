import java.util.Scanner;
import java.util.InputMismatchException;

public class Die {
    private int numberOfSides;
    private int value;

    public Die() { 
	numberOfSides = 6; 
    }

    public Die(int _numberOfSides) { 
	numberOfSides = _numberOfSides; 
    }

    public int roll() {
	return value =  (int) (Math.random()*numberOfSides) + 1;
    }
    
    public int get() { 
	return value; 
    }

    public String toString() {
	return "Die(" + value + ")";
    }

    public static void main(String [] args){	
	int sides = 0;
	boolean sidesCheck = false;

	while(!sidesCheck){
	    Scanner sc = new Scanner(System.in);
	    System.out.print("Number of sides: ");
	    try{sides = sc.nextInt();
		if(sides > 0) {sidesCheck = true;}
		else{
		    System.out.println("Dumbass, you must have a positive number of sides!");}
	    }
	    catch (InputMismatchException e) {
		System.out.println("You must enter a number!");}	    
	}
        Die d = new Die(sides);
	d.roll();
	System.out.println("Tärningen är kastad: " + d.get());
	System.out.println(d.toString());
    }
    
}


