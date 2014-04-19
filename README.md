Snelling Dining Commons
===================
CSCI 4210 - Simulation & Modeling
Term Project
4/19/14

Written by Mark Hoefer and his good friend Sir William Isaac Henry Newton III

To run the simulation model:
	Place the file DiningHall.scala inside of $SCALATION/src/main/scala/SnellingDiningCommons
	    **SnellingDiningCommons** is the root directory for the project**

From $SCALATION, run:
	sbt compile
	./scar DiningHall

Within DiningHall.scala, the variable nArrivals specifies the number of students to simulate eating at Snelling Dining Commons.  This will alter the length of the simulation and the resulting statistics.

Change aniRatio if you would like to alter the speed of the simulation's animation.
    Note: > aniRatio's = slower animation speed
          < aniRotio's = faster animation speed