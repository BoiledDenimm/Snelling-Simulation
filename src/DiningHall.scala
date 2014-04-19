//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Mark Hoefer
 */

import scalation.process._
import scalation.model.Modelable
import scalation.random.{Discrete, Uniform, Variate}
import scalation.linalgebra.VectorD
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHall` object is used to run the `DiningHallModel` class.
 */
object DiningHall extends App with Modelable
{
    val nArrivals = 500       // 
    val iArrivalRV = Uniform(0.5, 0.3)
    
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
        val bm = new DiningHallModel ("Snelling", nArrivals, nCardScanners, nPizzaServers, nGrillServers, nMexicanServers, nSandwichServers,
            nMainServers, nTables, iArrivalRV, cardScannerRV, pizzaRV, grillRV, mexicanRV, sandwichRV, mainRV, whatToEatRV, tableRV, moveRV, aniRatio)
        bm.simulate ()
    } // simulate

    simulate (0.0)

} // DiningHall object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHallModel` class defines a simple process-interaction model of DumpTrucks
 *  where dump trucks are loaded and weighed before traveling a certain amount of time to
 *  complete their journey back to the loading queue. 
 *  @param name        the name of the Dump Trucks model
 *  @param nArrivals   the number of arrivals/loads to generate per truck(stopping condition)
 *  @param nCardScanners    the number of loaders
 *  @param nScales     the number of scales
 *  @param iArrivalRV  the inter-arrival time between trucks (all arrive at the same time)
 *  @param loadingRV   the loading time distribution
 *  @param weighingRV  the weighing time distribution
 *  @param travelRV    the travel time distribution for motion from scale to railroad and back to loadingQ
 *  @param moveRV      the travel time distribution for motion from loader to scale (negligible)
 *  @param aniRatio    the ratio of simulation speed vs. animation speed
 */
class DiningHallModel (name: String, nArrivals: Int,  nCardScanners: Int, nPizzaServers: Int, nGrillServers: Int, nMexicanServers: Int, nSandwichServers: Int, 
    nMainServers: Int, nTables: Int, iArrivalRV: Variate, cardScannerRV: Variate, pizzaRV: Variate, grillRV: Variate, mexicanRV: Variate, sandwichRV: Variate, 
    mainRV: Variate, whatToEatRV: Variate, tableRV: Variate, moveRV: Variate, aniRatio: Double)
      extends Model (name, aniRatio)
{
	
    val entrance = Source ("Welcome to Snelling", this, Student, 0, nArrivals, iArrivalRV, (25, 300))
    
    val scannerQ  	= WaitQueue ("scannerQ", (100, 300))
    val scanner      = Resource ("scanner", scannerQ, nCardScanners, cardScannerRV, (120, 300))
    val toScannerQ 	= new Transport ("toScannerQ", entrance, scannerQ, moveRV)
    
    val pizzaQ  	= WaitQueue ("pizzaQ", (200, 100))
    val pizza      = Resource ("pizza", pizzaQ, nPizzaServers, pizzaRV, (220, 100))
    val toPizzaQ 	= new Transport ("toPizzaQ", scanner, pizzaQ, moveRV)
    
    val grillQ  	= WaitQueue ("grillQ", (200, 200))
    val grill      = Resource ("grill", grillQ, nGrillServers, grillRV, (220, 200))
    val toGrillQ 	= new Transport ("toGrillQ", scanner, grillQ, moveRV)
    
    val mexicanQ  	= WaitQueue ("mexicanQ", (200, 300))
    val mexican      = Resource ("mexican", mexicanQ, nMexicanServers, mexicanRV, (220, 300))
    val toMexicanQ 	= new Transport ("toMexicanQ", scanner, mexicanQ, moveRV)
    
    val sandwichQ  	= WaitQueue ("sandwichQ", (200, 400))
    val sandwich      = Resource ("sandwich", sandwichQ, nSandwichServers, sandwichRV, (220, 400))
    val toSandwichQ 	= new Transport ("toSandwichQ", scanner, sandwichQ, moveRV)
    
    val mainQ  	= WaitQueue ("mainQ", (200, 500))
    val main      = Resource ("main", mainQ, nMainServers, mainRV, (220, 500))
    val toMainQ 	= new Transport ("toMainQ", scanner, mainQ, moveRV)
    
    val tableQ  	= WaitQueue ("tableQ", (300, 300))
    val table      = Resource ("table", tableQ, nTables, tableRV, (320, 300))
    val toTableQ1 	= new Transport ("toTableQ", pizza, tableQ, moveRV)
    val toTableQ2 	= new Transport ("toTableQ", mexican, tableQ, moveRV)
    val toTableQ3 	= new Transport ("toTableQ", grill, tableQ, moveRV)
    val toTableQ4 	= new Transport ("toTableQ", sandwich, tableQ, moveRV)
    val toTableQ5 	= new Transport ("toTableQ", main, tableQ, moveRV)
    
    val diningHallExit = Sink ("Snelling Exit", (320, 600))
    val toDiningHallExit 	= new Transport ("toDiningHallExit", table, diningHallExit, moveRV)

    addComponent (entrance, scannerQ, scanner, toScannerQ, pizzaQ, pizza, toPizzaQ, grillQ, grill, toGrillQ, mexicanQ, mexican, toMexicanQ, 
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

} // DiningHallModel class
