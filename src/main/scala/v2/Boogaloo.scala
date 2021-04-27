package nl.dgl.ecology
package v2

object Fauna extends Enumeration {
  val Wolf, Deer,Rabbit = Value
}

import Fauna._

object MyFirstEcoSystemV2 extends LotkaVolterraEcoSystem {

  // Fauna deer = new Fauna("Deer", 1.05, 100);
  // Fauna wolf = new Fauna("Wolf", 0.9, 10);

  fauna(Wolf,0.9)

  fauna(Deer,1.05)

  predation(Wolf,Deer,0.001,0.0001)

  fauna(Rabbit,_(Rabbit)+1 )

  //

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("fauna="+MyFirstEcoSystemV2.fauna2rr)
    println("predation="+MyFirstEcoSystemV2.predations)

    var pNext:Population = Map(Deer->1000,Wolf->10,Rabbit-> 52)
    println( "p0="+pNext)

    for(_ <- 1 to 100){
      pNext = cycle(pNext)
      println( "pNext="+pNext)
    }

  }

}

///////////////////////

case class Predation(predator:Fauna.Value,prey:Fauna.Value,predatorRate:Double,preyRate:Double)

class LotkaVolterraEcoSystem extends EcoSystem {

  var fauna2rr = Map.empty[Fauna.Value,Double]
  var predations = List.empty[Predation]
  var hunter2predations = List.empty[Fauna.Value =>Int]

  def fauna(fauna:Fauna.Value,rr:Double): Unit = fauna2rr += (fauna -> rr)

  def predation(predator:Fauna.Value,prey:Fauna.Value,predatorRate:Double,preyRate:Double): Unit = {
    predations =  Predation(predator, prey, predatorRate, preyRate) :: predations
    super.fauna(predator,lotkaVolterraCycle(predator))
    super.fauna(prey,lotkaVolterraCycle(prey))
  }

  def lotkaVolterraCycle(fauna:Fauna.Value)(population: Population): Int = {
    val pop = population(fauna)
    val asFauna = pop*fauna2rr(fauna)
    val asPrey = predations.filter(_.prey.equals(fauna))//
      .map(p=> population(p.predator)*pop*p.preyRate) //
      .sum
    val asPredator = predations.filter(_.predator.equals(fauna))//
      .map(p=> population(p.prey)*pop*p.predatorRate)
      .sum
    (asFauna + asPredator + asPrey).toInt
  }



}

