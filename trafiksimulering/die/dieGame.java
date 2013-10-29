public class dieGame {
    public static void main(String[] args) {

	PairOfDice pairOfDice1 = new PairOfDice(2);
	PairOfDice pairOfDice2 = new PairOfDice(2);
	
	pairOfDice1.getDice1().roll();
	pairOfDice1.getDice2().roll();
	pairOfDice2.getDice1().roll();
	pairOfDice2.getDice2().roll();
	
	if(pairOfDice1.equals(pairOfDice2)) {
	    System.out.println("Lika");}
	else
	    System.out.println("Ej lika");
    }
}
