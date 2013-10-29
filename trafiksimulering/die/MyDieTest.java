import java.util.Scanner;

public class MyDieTest {
    private int numberOfSides;
    private int value;

    public MyDieTest() {
	numberOfSides = 6;
    }

    public MyDieTest(int _numberOfSides) {
	numberOfSides = _numberOfSides;
    }

    public int roll(){
	for(int i = 0; i < 10; i++){
	    value = value + (int) (Math.random()*numberOfSides) + 1;
	}
	return value;
    }

    public static void main(String [] args) {
	Scanner sc = new Scanner(System.in);
	System.out.print("Number of sides: ");
	MyDieTest d = new MyDieTest(sc.nextInt());
	System.out.println("Summan av 10 kast är: " + d.roll());
    }
}
