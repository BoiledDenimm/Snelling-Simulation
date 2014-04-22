//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Mark Hoefer
 */

import scalation.process._
import scalation.model.Modelable
import scalation.random.{Discrete, Uniform, Variate, Normal}
import scalation.linalgebra.VectorD
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHallv2` object is used to run the `DiningHallModelv2` class.
 */
object DiningHallv2 extends App with Modelable
{
    val nArrivals = 50       // 
    val iArrivalRV = Uniform(0.5, 0.3)
    
    val nCardScanners = 1
    val cardScannerRV = Uniform(0.5, 0.3)
    
    val decisionRV = Discrete(VectorD(0),VectorD(0))
    
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
    
    val whatFirstPDist: VectorD = VectorD(0.5, 0.5)
    val whatFirstInts: VectorD = VectorD(1,2) //1=food, 2=table
    val whatFirstRV = Discrete(whatFirstPDist, whatFirstInts)
    
    val nTables = 50
    val tableRV = Uniform(1, 0.5)
    val moveRV = Discrete(VectorD(1,1,1),VectorD(1,1,1))
    val aniRatio = 500      // the ratio of simulation speed vs. animation speed
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `DiningHallModelv2`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val bm = new DiningHallModelv2 ("Snelling", nArrivals, nCardScanners, nPizzaServers, nGrillServers, nMexicanServers, nSandwichServers,
            nMainServers, nTables, iArrivalRV, cardScannerRV, decisionRV, pizzaRV, grillRV, mexicanRV, sandwichRV, mainRV, whatToEatRV, whatFirstRV, 
            tableRV, moveRV, aniRatio)
        bm.simulate ()
    } // simulate

    simulate (0.0)

} // DiningHallv2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiningHallModelv2` class defines a process-interaction model of a Dining Hall
 *  where students enter the dining hall and then they either set their down the back pack 
 *  to save a seat/table or get food, if they move to save a seat first the students will
 *  then move to get food and if they don't the student will get food and then try to sit down
 *  to eat.
 *  @param name        		the name of the model
 *  @param nArrivals   		the number of arrivals
 *  @param nCardScanners    the number of card scanners
 *  @param nPizzaServers    the number of pizza servers
 *  @param nGrillServers   	the number of grill servers
 *  @param nMexicanServers  the number of mexican food servers
 *  @param nSandwichServers the number of sandwich servers
 *  @param nMainServers     the number of main food servers
 *  @param nTables     		the number of tables
 *  @param iArrivalRV  		the inter-arrival time between students
 *  @param cardScannerRV   	the card scanning time distribution
 *  @param decisionRV   	the time it takes to decide what to do first (0)
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
class DiningHallModelv2 (name: String, nArrivals: Int,  nCardScanners: Int, nPizzaServers: Int, nGrillServers: Int, nMexicanServers: Int, nSandwichServers: Int, 
    nMainServers: Int, nTables: Int, iArrivalRV: Variate, cardScannerRV: Variate, decisionRV: Variate, pizzaRV: Variate, grillRV: Variate, mexicanRV: Variate, 
    sandwichRV: Variate, mainRV: Variate, whatToEatRV: Variate, whatFirstRV: Variate, tableRV: Variate, moveRV: Variate, aniRatio: Double)
      extends Model (name, aniRatio)
{
	
    val entrance = Source ("Entrance", this, Student, 0, nArrivals, iArrivalRV, (100, 300))
    
    val scannerQ  	= WaitQueue ("scannerQ", (200, 300))
    val scanner     = Resource ("scanner", scannerQ, nCardScanners, cardScannerRV, (221, 295))
    val toScannerQ 	= new Transport ("toScannerQ", entrance, scannerQ, moveRV)
    
    val decisionQ 	= WaitQueue ("decisionQ", (330, 230))
    val decision    = Resource ("decision", decisionQ, nArrivals, decisionRV, (351, 225))
    val toDecisionQ = new Transport ("toDecisionQ", scanner, decisionQ, moveRV)
    
    val getFoodQ 	= WaitQueue ("getFoodQ", (330, 370))
    val getFood    = Resource ("getFood", getFoodQ, nArrivals, decisionRV, (351, 365))
    val toGetFoodQ = new Transport ("toGetFoodQ", decision, getFoodQ, moveRV)
    
    val pizzaQ  	= WaitQueue ("pizzaQ", (575, 100))
    val pizza       = Resource ("pizza", pizzaQ, nPizzaServers, pizzaRV, (596, 95))
    val toPizzaQ 	= new Transport ("toPizzaQ", getFood, pizzaQ, moveRV)
    
    val grillQ  	= WaitQueue ("grillQ", (575, 200))
    val grill       = Resource ("grill", grillQ, nGrillServers, grillRV, (596, 195))
    val toGrillQ 	= new Transport ("toGrillQ", getFood, grillQ, moveRV)
    
    val mexicanQ  	= WaitQueue ("mexicanQ", (575, 300))
    val mexican     = Resource ("mexican", mexicanQ, nMexicanServers, mexicanRV, (596, 295))
    val toMexicanQ 	= new Transport ("toMexicanQ", getFood, mexicanQ, moveRV)
    
    val sandwichQ  	= WaitQueue ("sandwichQ", (575, 400))
    val sandwich    = Resource ("sandwich", sandwichQ, nSandwichServers, sandwichRV, (596, 395))
    val toSandwichQ = new Transport ("toSandwichQ", getFood, sandwichQ, moveRV)
    
    val mainQ  		= WaitQueue ("mainQ", (575, 500))
    val main      	= Resource ("main", mainQ, nMainServers, mainRV, (596, 495))
    val toMainQ 	= new Transport ("toMainQ", getFood, mainQ, moveRV)
    
    val tableQ  	= WaitQueue ("tableQ", (815, 300))
    val table       = Resource ("table", tableQ, nTables, tableRV, (836, 295))
    val toTableQ1 	= new Transport ("toTableQ", pizza, tableQ, moveRV)
    val toTableQ2 	= new Transport ("toTableQ", mexican, tableQ, moveRV)
    val toTableQ3 	= new Transport ("toTableQ", grill, tableQ, moveRV)
    val toTableQ4 	= new Transport ("toTableQ", sandwich, tableQ, moveRV)
    val toTableQ5 	= new Transport ("toTableQ", main, tableQ, moveRV)
    
    val diningHallExit 	 = Sink ("Exit", (846, 500))
    val toDiningHallExit = new Transport ("toDiningHallExit", table, diningHallExit, moveRV)
    val toTableQFirst = new Route ("toTableQFirst", 1, decision, tableQ, moveRV, true, 0.0, 0.8)
    val toFoodLine = new Route ("toFoodLine", 1, tableQ, getFoodQ, moveRV, true, 0.0, -0.8)

    addComponent (entrance, scannerQ, scanner, toScannerQ, decisionQ, decision, toDecisionQ, getFoodQ, getFood, toGetFoodQ, pizzaQ, pizza, toPizzaQ, grillQ, grill, toGrillQ, 
        mexicanQ, mexican, toMexicanQ, sandwichQ, sandwich, toSandwichQ, mainQ, main, toMainQ, tableQ, table, toTableQ1, toTableQ2, toTableQ3, 
        toTableQ4, toTableQ5, diningHallExit, toDiningHallExit, toTableQFirst, toFoodLine)

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
            
            toDecisionQ.move ()
            if (decision.busy) 
              decisionQ.waitIn ()
            else decisionQ.noWait ()
            decision.utilize ()
            decision.release ()
            
            val whatFirst = whatFirstRV.igen
            if (whatFirst == 2) {
	            toTableQFirst.lane (0)
	            if (table.busy) 
	              tableQ.waitIn ()
	            else tableQ.noWait ()
	            table.utilize ()
	            table.release ()
	            
	            toFoodLine.lane (0)
	            if (getFood.busy) 
	              getFoodQ.waitIn ()
	            else getFoodQ.noWait ()
	            getFood.utilize ()
	            getFood.release ()
            }
            else if (whatFirst==1){
             
              toGetFoodQ.move ()
	            if (getFood.busy) 
	              getFoodQ.waitIn ()
	            else getFoodQ.noWait ()
	            getFood.utilize ()
	            getFood.release ()
              
            }
            
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

} // DiningHallModelv2 class
