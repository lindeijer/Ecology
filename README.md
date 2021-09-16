# Ecology
An ecology simulation: a population is cycled. 

where type Population is Map[Fauna,Int].

A cycle is a function Fauna => Population => Int.

The system cycle is a function Population => Population. 

For now, we cycle with a fixed period, best thought of a year progressing through al four seasons.


Ultimately:

* massive, tremendous number of flora and fauna interacting with / feeding upon each other.


Cycles

* a tanh function to model reproduction. Model wrt H which is the amount of prey 1 animal needs to survive. 
  * Less implies famine - death due to starvation. 
  * More implies death by natural causes only.
  * Reproduction is 50% at this level of food.