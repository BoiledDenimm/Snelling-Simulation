//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Mark Hoefer
 */

import scalation.process._
import scalation.model.Modelable
import scalation.random.{Discrete, Uniform, Variate, Normal}
import scalation.linalgebra.VectorD
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHall` object is used to run the `DiningHallModel` class.
 */
object DiningHall extends App with Modelable
{
    val nArrivals = 50       // 
    val nExisting = 200
    val iArrivalRV = Uniform(0.5, 0.3)
    val existingRV = Uniform(0,0)
    
    val nCardScanners = 1
    val cardScannerRV = Uniform(0.5, 0.3)
    
    val nPizzaServers = 1
    val pizzaRV = Uniform(1, 0.5)
    val nGrillServers = 3
    val grillRV = Uniform(5, 2)
    val nMexicanServers = 2
    val mexicanRV = Uniform(5, 2)
    val nSandwichServers = 2
    val sandwichRV = Uniform(5, 2)
    val nMainServers = 1
    val mainRV = Uniform(1, 0.5)
    
    val whatToEatPDist: VectorD = VectorD(0.2, 0.2 , 0.2, 0.2, 0.2)
    val whatToEatInts: VectorD = VectorD(1,2,3,4,5) //1=pizza, 2=mexican, 3=grill, 4=sandwich, 5=main
    val whatToEatRV = Discrete(whatToEatPDist, whatToEatInts)
    
    val nTables = 50
    val tableRV = Uniform(1, 0.5)
    val moveRV = Discrete(VectorD(1,1,1),VectorD(1,1,1))
    val aniRatio = 200      // the ratio of simulation speed vs. animation speed
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `DiningHallModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val bm = new DiningHallModel ("Snelling", nArrivals, nExisting, nCardScanners, nPizzaServers, nGrillServers, nMexicanServers, nSandwichServers,
            nMainServers, nTables, iArrivalRV, existingRV, cardScannerRV, pizzaRV, grillRV, mexicanRV, sandwichRV, mainRV, whatToEatRV, tableRV, moveRV, aniRatio)
        bm.simulate ()
    } // simulate

    simulate (0.0)

} // DiningHall object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHallModel` class defines a simple process-interaction model of a Dining Hall
 *  where students enter the dining hall, get food, sit down to eat, and then leave
 *  @param name        		the name of the model
 *  @param nArrivals   		the number of arrivals
 *  @param nExisting   		the number of existing
 *  @param nCardScanners    the number of card scanners
 *  @param nPizzaServers    the number of pizza servers
 *  @param nGrillServers   	the number of grill servers
 *  @param nMexicanServers  the number of mexican food servers
 *  @param nSandwichServers the number of sandwich servers
 *  @param nMainServers     the number of main food servers
 *  @param nTables     		the number of tables
 *  @param iArrivalRV  		the inter-arrival time between students
 *  @param cardScannerRV   	the card scanning time distribution
 *  @param pizzaRV  		the pizza serving time distribution
 *  @param grillRV  		the grill serving time distribution
 *  @param mexicanRV  		the mexican food serving time distribution
 *  @param sandwichRV  		the sandwich serving time distribution
 *  @param mainRV  			the main food serving time distribution
 *  @param whatToEatRV  	the choice of food distribution
 *  @param tableRV    		the distribution of time spent eating
 *  @param moveRV      		distribution of student movement speed 
 *  @param aniRatio    		the ratio of simulation speed vs. animation speed
 */
class DiningHallModel (name: String, nArrivals: Int, nExisting: Int, nCardScanners: Int, nPizzaServers: Int, nGrillServers: Int, nMexicanServers: Int, nSandwichServers: Int, 
    nMainServers: Int, nTables: Int, iArrivalRV: Variate, existingRV: Variate, cardScannerRV: Variate, pizzaRV: Variate, grillRV: Variate, mexicanRV: Variate, sandwichRV: Variate, 
    mainRV: Variate, whatToEatRV: Variate, tableRV: Variate, moveRV: Variate, aniRatio: Double)
      extends Model (name, aniRatio)
{
	
    val entrance = Source ("Entrance", this, Student, 0, nArrivals, iArrivalRV, (230, 300))
    val existingPpl = Source("Existing People", this, ExistingStudents, 0, nExisting, existingRV, (815, 100))
    
    val scannerQ  	= WaitQueue ("scannerQ", (330, 300))
    val scanner      = Resource ("scanner", scannerQ, nCardScanners, cardScannerRV, (351, 295))
    val toScannerQ 	= new Transport ("toScannerQ", entrance, scannerQ, moveRV)
    
    val pizzaQ  	= WaitQueue ("pizzaQ", (575, 100))
    val pizza      = Resource ("pizza", pizzaQ, nPizzaServers, pizzaRV, (596, 95))
    val toPizzaQ 	= new Transport ("toPizzaQ", scanner, pizzaQ, moveRV)
    
    val grillQ  	= WaitQueue ("grillQ", (575, 200))
    val grill      = Resource ("grill", grillQ, nGrillServers, grillRV, (596, 195))
    val toGrillQ 	= new Transport ("toGrillQ", scanner, grillQ, moveRV)
    
    val mexicanQ  	= WaitQueue ("mexicanQ", (575, 300))
    val mexican      = Resource ("mexican", mexicanQ, nMexicanServers, mexicanRV, (596, 295))
    val toMexicanQ 	= new Transport ("toMexicanQ", scanner, mexicanQ, moveRV)
    
    val sandwichQ  	= WaitQueue ("sandwichQ", (575, 400))
    val sandwich      = Resource ("sandwich", sandwichQ, nSandwichServers, sandwichRV, (596, 395))
    val toSandwichQ 	= new Transport ("toSandwichQ", scanner, sandwichQ, moveRV)
    
    val mainQ  	= WaitQueue ("mainQ", (575, 500))
    val main      = Resource ("main", mainQ, nMainServers, mainRV, (596, 495))
    val toMainQ 	= new Transport ("toMainQ", scanner, mainQ, moveRV)
    
    val tableQ  	= WaitQueue ("tableQ", (815, 300))
    val table      = Resource ("table", tableQ, nTables, tableRV, (836, 295))
    val toTableQ1 	= new Transport ("toTableQ", pizza, tableQ, moveRV)
    val toTableQ2 	= new Transport ("toTableQ", mexican, tableQ, moveRV)
    val toTableQ3 	= new Transport ("toTableQ", grill, tableQ, moveRV)
    val toTableQ4 	= new Transport ("toTableQ", sandwich, tableQ, moveRV)
    val toTableQ5 	= new Transport ("toTableQ", main, tableQ, moveRV)
    
    val diningHallExit = Sink ("Exit", (846, 500))
    val toDiningHallExit 	= new Transport ("toDiningHallExit", table, diningHallExit, moveRV)

    addComponent (entrance, existingPpl, scannerQ, scanner, toScannerQ, pizzaQ, pizza, toPizzaQ, grillQ, grill, toGrillQ, mexicanQ, mexican, toMexicanQ, 
        sandwichQ, sandwich, toSandwichQ, mainQ, main, toMainQ, tableQ, table, toTableQ1, toTableQ2, toTableQ3, toTableQ4, toTableQ5, diningHallExit, toDiningHallExit)

    case class Student extends SimActor ("s", this) 
    {
        def act ()
        {
          
        	toScannerQ.move ()
            if (scanner.busy) 
              scannerQ.waitIn ()
            else scannerQ.noWait ()
            scanner.utilize ()
            scanner.release ()
            
            val whatToEat = whatToEatRV.igen
            if (whatToEat == 1) {
            
	            toPizzaQ.move ()
	            if (pizza.busy) 
	              pizzaQ.waitIn ()
	            else pizzaQ.noWait ()
	            pizza.utilize ()
	            pizza.release ()
	            toTableQ1.move ()
            
            } else if (whatToEat == 2) {
            
	            toGrillQ.move ()
	            if (grill.busy) 
	              grillQ.waitIn ()
	            else grillQ.noWait ()
	            grill.utilize ()
	            grill.release ()
	            toTableQ2.move ()
            
            } else if (whatToEat == 3) {
        	  
	            toMexicanQ.move ()
	            if (mexican.busy) 
	              mexicanQ.waitIn ()
	            else mexicanQ.noWait ()
	            mexican.utilize ()
	            mexican.release ()
	            toTableQ3.move ()
            
        	} else if (whatToEat == 4) {
            
	            toSandwichQ.move ()
	            if (sandwich.busy) 
	              sandwichQ.waitIn ()
	            else sandwichQ.noWait ()
	            sandwich.utilize ()
	            sandwich.release ()
	            toTableQ4.move ()
            
        	} else if (whatToEat == 5) {
            
	            toMainQ.move ()
	            if (main.busy) 
	              mainQ.waitIn ()
	            else mainQ.noWait ()
	            main.utilize ()
	            main.release ()
	            toTableQ5.move ()
            
        	}
            
            
            if (table.busy) 
              tableQ.waitIn ()
            else tableQ.noWait ()
            table.utilize ()
            table.release ()
            
            diningHallExit.leave ()
        } // act

    } // Students
    
    case class ExistingStudents extends SimActor ("es", this) 
    {
        def act ()
        {
          
          if (table.busy) 
              tableQ.waitIn ()
            else tableQ.noWait ()
            table.utilize ()
            table.release ()
          
          diningHallExit.leave ()
        } // act

    } // Students

} // DiningHallModel class
