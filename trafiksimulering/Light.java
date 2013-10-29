public class Light {
    private int period;
    private int time;  // Intern klocka: 0, 1, ... period-1, 0, 1 ...
    private int green; // Signalen grön när time<green 

    public Light(int period, int green) {
	this.period = period;
        this.time = 0;
	this.green = green;
    }

    public void    step() {
	if(time < (period-1)) time += 1;
	else time = 0;
       // Stegar fram klocka ett steg
    }

    public boolean isGreen()   {
	if(time < green) return true;
	else return false;
	// Returnerar true om time<green, annars false
    }

	public String toString() {
	    if(time < green) return "G";
	    else return "R";
//return "Light(period=" + this.period + ", time=" + this.time + ", green=" + this.green + ")";
	}
	
}
