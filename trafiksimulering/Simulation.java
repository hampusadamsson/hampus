public class Simulation {

    public static void main(String [] args) {

	TrafficSystem test = new TrafficSystem();
	int totalTime = test.getTotalTime();
	test.print();
	for(int i = 0; i < totalTime; i++) {
	    System.out.println("--------------------------------------------");
	    test.step();
	    test.print();
	    test.printStatistics();
	}





	// Car testCar = new Car(1,2);
	// System.out.println(testCar);

	// Light testLight = new Light(2,1);
	// System.out.println(testLight);

        // Lane testLane = new Lane(5);
        // System.out.println(testLane);

	// Skapar ett TrafficSystem
	// Utför stegningen, anropar utskriftsmetoder
    }
}
