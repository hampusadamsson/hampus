import java.util.Scanner;

public class TrafficSystem {
    // Definierar de vägar och signaler som ingår i det 
    // system som skall studeras.
    // Samlar statistik
    
    // Attribut som beskriver beståndsdelarna i systemet
    private Lane  r0;
    private Lane  r1;
    private Lane  r2;
    private Light s1;
    private Light s2;

    // Diverse attribut för simuleringsparametrar (ankomstintensiteter,
    // destinationer...)  
    private int arrivalRate;
    private int periodLights;
    private int periodGreen1;
    private int periodGreen2;
    private int r0Length;
    private int r1Length;
    private int r2Length;
    private int totalTime;

    private int time = 0;
    private int arrivalRateTime = 0;

    // Diverse attribut för statistiksamling
    //....  
    private Lane missedCars;

    public TrafficSystem() {
	readParameters();
	r0 = new Lane(r0Length);
	r1 = new Lane(r1Length);
	r2 = new Lane(r2Length);
	s1 = new Light(periodLights, periodGreen1);
	s2 = new Light(periodLights, periodGreen2);
	missedCars = new Lane(1);
}

    private void readParameters() {

        Scanner sc = new Scanner(System.in);
	System.out.print("Indicate arrival rate (ex. 1 - one new car every timestep, 4 - one new car every fourth timestep): ");
	arrivalRate = sc.nextInt();
	System.out.print("Specify the period time for the lights: ");
	periodLights = sc.nextInt();
	System.out.print("Specify the green light period time for light 1: ");
	periodGreen1 = sc.nextInt();
	System.out.print("Specify the green light period time for light 2: ");
	periodGreen2 = sc.nextInt();
	System.out.print("Set the length of road: ");
	r0Length = sc.nextInt();
	System.out.print("Set the length of file: ");
	r2Length = sc.nextInt();
	r1Length = r2Length;
	r0Length -= r1Length;
	System.out.print("How many time steps do you want the program to run?: ");
	totalTime = sc.nextInt();
	// Läser in parametrar för simuleringen
	// Metoden kan läsa från terminalfönster, dialogrutor
	// eller från en parameterfil. Det sista alternativet
	// är att föredra vid uttestning av programmet eftersom
	// man inte då behöver mata in värdena vid varje körning.
        // Standardklassen Properties är användbar för detta. 
        //test
    }

    public void step() {
	time += 1;
	s1.step();
	s2.step();
	if(s1.isGreen()) 
	    r1.getFirst();
	if(s2.isGreen()) 
	    r2.getFirst();
	r1.step();
	r2.step();
	   
	if(r0.firstCar() != null) { 
	    if(r0.firstCar().getDest() == 1 && r1.lastFree() == true) { 
		r1.putLast(r0.getFirst());}
	    else if(r0.firstCar().getDest() == 2 && r2.lastFree() == true) {
		r2.putLast(r0.getFirst());}
	}
	r0.step();
	
	arrivalRateTime += 1; 	
	if(arrivalRateTime == arrivalRate) {
	    Car newCar = new Car(time, (int) ((Math.random() * 2) + 1));
	    System.out.println("New car incomming: " + newCar);
	    try { r0.putLast(newCar);
	    } catch (RuntimeException e) {
		System.out.println("Oh no, there's no room for this car!");
		if (missedCars.firstCar() != null) {
		Lane _missedCars = new Lane(missedCars.getLength()+1);
		System.arraycopy(missedCars.getTheLane(), 0, _missedCars.getTheLane(), 0, missedCars.getLength());
		missedCars = _missedCars;
		missedCars.putLast(newCar);
		}
		else {
		    missedCars.putLast(newCar);
		}
	    }
	    arrivalRateTime = 0;
	}
    }
	
	// Stega systemet ett tidssteg m h a komponenternas step-metoder
	// Skapa bilar, lägg in och ta ur på de olika Lane-kompenenterna
    
    public int getTotalTime() {
	return totalTime;
    }
    
    public void printStatistics() {
	if(missedCars.getLength() > 1) {
	    System.out.println("The total amount of missed cars:");
	    for(int i=0; i<missedCars.getLength(); i++) {
		System.out.println(missedCars.getTheLane()[i].toString() + " ");
	    }
	}
	// Skriv statistiken samlad så här långt
    }

    public void print() {
	System.out.println(s1.toString() + r1.toString() + r0.toString());
	System.out.println(s2.toString() + r2.toString());
	// Skriv ut en grafisk representation av kösituationen
	// med hjälp av klassernas toString-metoder
    }

}
