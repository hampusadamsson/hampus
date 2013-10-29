public class Lane {

    public static class OverflowException extends RuntimeException {
	// Undantag som kastas när det inte gick att lägga 
        // in en ny bil på vägen
    }

    private Car[] theLane;

    public Lane(int n) {
	theLane = new Car[n];
	// Konstruerar ett Lane-objekt med plats för n fordon
    }

    public int getLength() {
	return theLane.length;
    }

    public Car[] getTheLane() {
	return theLane;
    }

    public void step() {
	for(int i = 0; i < theLane.length-1; i++) {
	    if(theLane[i] == null) {
		theLane[i] = theLane[i+1];
		theLane[i+1] = null;
	    }
	}
	// Stega fram alla fordon (utom det på plats 0) ett steg 
        // (om det går). (Fordonet på plats 0 tas bort utifrån 
	// m h a metoden nedan.)
    }

    public Car getFirst() {
	Car first = theLane[0];
	theLane[0] = null;
	return first;
	// Returnera och tag bort bilen som står först
    }

    public Car firstCar() {
	return theLane[0];
	// Returnera bilen som står först utan att ta bort den
    }


    public boolean lastFree() {
	if(theLane[theLane.length-1] == null) {
	    return true;
        }
        else return false;
        // Returnera true om sista platsen ledig, annars false
    }

    public void putLast(Car c) throws OverflowException {
	if(theLane[theLane.length-1] == null) {
	    theLane[theLane.length-1] = c;
	}
	else { throw new OverflowException();
	}
	// Ställ en bil på sista platsen på vägen
	// (om det går).
    }
    
    public String toString() {
        String result = "";
        for(int i=0; i<theLane.length; i++) {
            if(theLane[i] == null) {
                result += " _ ";}
            else {
                result += ".^.";}
        }
        return result;
    }
}
