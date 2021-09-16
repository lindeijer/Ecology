package nl.dgl.ecology
package v2

import scala.collection.mutable
import scala.math._


object Fauna extends Enumeration {
  type Fauna = Value

  case class FaunaVal(maxAge: Int, reproductionRate: Double) extends super.Val {}

  import scala.language.implicitConversions

  implicit def valueToFaunaVal(x: Value): FaunaVal = x.asInstanceOf[FaunaVal]

  implicit def valueToEnumIndex(x: Value): Int = x.id

  val $$$ = FaunaVal(0, 0.0)
  val wolf = FaunaVal(25, 1.1)
  val deer = FaunaVal(10, 1.11)
  val rabbit = FaunaVal(5, 1.9)
  val fox = FaunaVal(15, 1.1)
  val grass = FaunaVal(Int.MaxValue, 1.0)
}

object Population {

  def apply(ppp: Array[Int]) = {
    var result = mutable.HashMap[String, Int]()
    for (i <- 0 to ppp.length - 1) {
      result += (Fauna(i).toString -> ppp(i))
    }
    result
  }
}

////////////////

case class Plant(about: Fauna.Value) {}

case class Food(about: Fauna.Value, min: Int, max: Int) {}

case class Prey(about: Fauna.Value, min: Int, max: Int) {}

case class Feeder(about: Fauna.Value) {

  var foods = List[Food]();

  def eats(food: Fauna.Value, min: Int, max: Int): Feeder = {
    foods = Food(food, min, max) :: foods;
    this
  }
}

case class FeederCycle(feeder: Feeder, ppp: Array[Int]) {

  val p: Double = ppp(feeder.about.id).toDouble

  def potentialReproduction(food: Food) = {
    val pFood = ppp(food.about.id).toDouble
    val x = pFood / p;
    val potentialReproductionWithRespectToFood = EcoSystem.potentialRepro(food.min)(x)
    potentialReproductionWithRespectToFood
  }

  val potentialsReproductionSum: Double = feeder.foods.map(potentialReproduction).sum

  val offspring: Int = {
    val feederOffspringRate = (feeder.about.reproductionRate - 1.0) * (if (potentialsReproductionSum > 1) 1 else potentialsReproductionSum)
    (p * feederOffspringRate).toInt
  }

  val deaths: Int = {
    val feederSurvivalRate = (if (potentialsReproductionSum > 1) 1.0 else potentialsReproductionSum)
    (p * (1.0 - feederSurvivalRate)).toInt
  }

  def potentialEat(food: Food) = {
    val pFood = ppp(food.about.id).toDouble
    val x = pFood / p;
    val potentialFrenzyWithRespectToFood = EcoSystem.potentialFrenzy(food.max)(x)
    potentialFrenzyWithRespectToFood
  }

  val potentialsFrenzySum: Double = feeder.foods.map(potentialEat).sum

  val eats: Seq[(Int, Int)] = {
    val feederEatFactor = if (potentialsReproductionSum < 1) 1 else 1 / potentialsReproductionSum
    feeder.foods.map(food => (food.about.id, (p * food.max * feederEatFactor * potentialEat(food)).toInt))
  }

  val pNext: Int = max(0, p.toInt + offspring - deaths)

  override def toString() = {
    "FeederCycle(" + feeder.about + ",p=" + p + ",offspring=" + offspring + ",deaths=" + deaths + ",pNext=" + pNext + ",potReproSum=" + potentialsReproductionSum + ",potFzySum=" + potentialsFrenzySum +",eats=" + eats + ")"
  }

}


/////////////

object EcoSystem {
  def potentialRepro(min: Double)(pPred: Double): Double = {
    val x = (8.0 / min) * (pPred - min) // 8 rather than 2 for a steeper curve, a more sudden change from excess to famine.
    0.5 * tanh(x) + 0.5
  }

  def potentialFrenzy(max: Double)(pPred: Double): Double = {
    val x = (1.0 / max) * (pPred - 2 * max) // frenzy has a mellow curve pushed to the right. Full frenzy during real excess.
    0.5 * tanh(x) + 0.5
  }
}

class EcoSystem {

  var predators = List[Feeder]()
  var herbivores = List[Feeder]()
  var plants = List[Plant]()


  def predator(predator: Fauna.Value) = {
    predators = new Feeder(predator) :: predators
    predators.head
  }

  def herbivore(herbivore: Fauna.Value) = {
    herbivores = new Feeder(herbivore) :: herbivores
    herbivores.head
  }

  def plant(plant: Fauna.Value) = {
    plants = new Plant(plant) :: plants
    plants.head
  }

  def cycle(ppp: Array[Int]) = {

    println("ppp=" + Population(ppp))

    // compute predator effects wrt ppp
    val predatorCycles = predators.map(predator => FeederCycle(predator, ppp))

    println("predatorCycles=" + predatorCycles)

    // compute herbivore effects wrt ppp
    val herbivoreCycles = herbivores.map(herbivore => FeederCycle(herbivore, ppp))

    println("herbivoreCycles=" + herbivoreCycles)

    val qqq = new Array[Int](ppp.size)

    // compute plant effects
    plants.foreach(plant => {
      qqq(plant.about.id) = ppp(plant.about.id)
    }) // perfect recovery

    println("plants=" + plants)

    // set predator population
    predatorCycles.foreach(pC => qqq(pC.feeder.about.id) = pC.pNext)

    // set herbivore population
    herbivoreCycles.foreach(hC => qqq(hC.feeder.about.id) = hC.pNext)

    predatorCycles.foreach(predatorCycle => {
      predatorCycle.eats.foreach(pk => {
        val prey = pk._1
        val kill = pk._2
        qqq(prey) = qqq(prey) - kill
      })
    })

    val xxx = qqq.map(pop => {
      if (pop < 0) 0
      else pop
    })

    xxx(0) = ppp(0) + 1

    println("xxx=" + Population(xxx))

    xxx

  } // end of EcoSystem cycle

}

