import java.util.Scanner;

public class PairOfDice {
    private Die dice1;
    private Die dice2;

    public PairOfDice() {
	dice1 = new Die();
	dice2 = new Die();
    }

    public PairOfDice(int numberOfSides) {
	dice1 = new Die(numberOfSides);
	dice2 = new Die(numberOfSides);
    }

    public Die getDice1() {
	return dice1;
    }

    public Die getDice2() {
	return dice2;
    }

    public String pairToString() {
	return ("Dice 1: " + dice1.toString() + "\nDice 2: " + dice2.toString());
    }
    
    public boolean equals() {
	return dice1.get() == dice2.get();
    }

    public boolean equals(PairOfDice pairOfDice2) {
	if(this.dice1.get() == pairOfDice2.dice1.get() && this.dice2.get() == pairOfDice2.dice2.get())
	    return true;
    	else return false;
    }

    public static void main(String [] args){
	Scanner sc = new Scanner(System.in);
	int numberOfSides = 0;
	boolean sidesCheck = false;

	while(!sidesCheck){
	    System.out.print("Number of sides: ");
	    numberOfSides = sc.nextInt();
	    if(numberOfSides > 0) {sidesCheck = true;}
	    else{
		System.out.println("Try again!");
	    }
	}
	PairOfDice dPair = new PairOfDice(numberOfSides);
	dPair.dice1.roll();
	dPair.dice2.roll();
	System.out.println("Tärning 1: " + dPair.dice1.get());
	System.out.println("Tärning 2: " + dPair.dice2.get());
	System.out.println(dPair.pairToString());
	System.out.print("Tärningarna är ");
	if(dPair.equals()) 
	    System.out.println("lika.");
	else System.out.println("inte lika.");
    }
}
