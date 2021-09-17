package nl.dgl.ecology

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable


object Fauna extends Enumeration {
  type Fauna = Value

  case class FaunaVal(maxAge: Int, reproductionRate: Double) extends super.Val with FaunaFlora {}

  import scala.language.implicitConversions

  implicit def valueToFaunaVal(x: Value): FaunaVal = x.asInstanceOf[FaunaVal]

  implicit def valueToEnumIndex(x: Value): Int = x.id

  val $$$ = FaunaVal(0, 0.0)
  val wolf = FaunaVal(25, 1.1)
  val deer = FaunaVal(10, 1.11)
  val rabbit = FaunaVal(5, 1.9)
  val fox = FaunaVal(15, 1.1)
  val grass = FaunaVal(Int.MaxValue, 1.0)

  def toMap(ppp: Array[Int]) = {
    var result = mutable.HashMap[String, Int]()
    for (i <- 0 to ppp.length - 1) {
      result += (Fauna(i).toString -> ppp(i))
    }
    result
  }
}

////////////////////////////

class EcoSystemTest extends AnyFlatSpec with should.Matchers {

  import Fauna._
  import EcoSystem._

  "PotentialsSum" should "compute" in {
    object EcoSys extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      plant(grass)
    }
    // wold = 2 deer 38 wolf.min= 10
    potentialRepro(10.0)(38.0/2) should be(0.9734030064231342)
    var p = new Array[Int](Fauna.values.size)
    p(wolf) = 2
    p(deer) = 38
    val WOLF = EcoSys.predators.head
    val wolfCycle = FeederCycle(WOLF, p)
    wolfCycle.potentialsReproductionSum should be(0.9734030064231342) // 0.9734030064231342
  }


  "Zoogaloo" should "build" in {
    object Zoogaloo extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      plant(grass)
    }
    Zoogaloo.predators.size should be(1)
    Zoogaloo.predators.head.about should be(Fauna.wolf)
    Zoogaloo.predators.head.about.maxAge should be(25)
    Zoogaloo.predators.head.foods.size should be(1)
    Zoogaloo.predators.head.foods.head.about should be(Fauna.deer)
    Zoogaloo.predators.head.foods.head.min should be(10)
    Zoogaloo.predators.head.foods.head.max should be(13)
    Zoogaloo.herbivores.size should be(1)
    Zoogaloo.herbivores.head.about should be(Fauna.deer)
  }

  "In Zoogaloo the Wolf" should "have potentials wrt Deer" in {
    object Zoogaloo extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      plant(grass)
    }
    var p = new Array[Int](Fauna.values.size)
    p(wolf) = 11
    p(deer) = 165
    // predator is wolf.
    // prey is deer.
    val WOLF = Zoogaloo.predators.head
    val wolfCycle = FeederCycle(WOLF, p)
    wolfCycle.p should be(11)
    val pRatio = p(deer).toDouble / p(wolf)
    val WOLF_DEER = wolfCycle.feeder.foods.head
    val wolfPDeerPotential = wolfCycle.potentialReproduction(WOLF_DEER);
    wolfPDeerPotential should be(potentialRepro(WOLF_DEER.min)(pRatio))
    wolfPDeerPotential should be(0.8807970779778824)
  }

  "In Zoogaloo the Wolf" should "cyclr" in {
    object Zoogaloo extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      plant(grass)
    }
    var p = new Array[Int](Fauna.values.size)
    p(wolf) = 12
    p(deer) = 180
    //
    val WOLF = Zoogaloo.predators.head
    val wolfCycle = FeederCycle(WOLF, p)
    potentialRepro(10.0)(15) should be(0.8807970779778824) // 180 / 12 = 15
    potentialRepro(2.0)(3) should be(0.8807970779778824) // normalised
    wolfCycle.potentialsReproductionSum should be(0.8807970779778824) // f(10)(3) = f(2)(3) = 0.881
    wolfCycle.offspring should be(1) // 0.881 * 0.1 * 12 = 1.0572 -> 1
    wolfCycle.deaths should be(1) // 12 * (1 - 0.881)  = 1.428 -> 1
    wolfCycle.pNext should be(12)
    val wolfKills = wolfCycle.eats
    wolfKills.length should be(1)
    wolfKills.head._1 should be(deer.id)
    // wolfKills.head._2 should be(156) // 12 * 13
  }

  "In Zoogaloo the 11 Wolf" should "consume all 165 Deer" in {
    object Zoogaloo extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      plant(grass)
    }
    var p = new Array[Int](Fauna.values.size)
    p(wolf) = 11
    p(deer) = 333
    p(grass) = Int.MaxValue
    //
    val p1 = Zoogaloo.cycle(p).pNext
    p1(wolf) should be(12)
    //p1(deer) should be(223) // p=333 offspring=33 wolf=143
    //val p2 = Zoogaloo.cycle(p1).pNext
    //p2(wolf) should be(13)
    //p2(deer) should be(0)
    //val p3 = Zoogaloo.cycle(p2).pNext
    //p3(wolf) should be(1)
    //p3(deer) should be(0)
  }

  "A Predator with two Prey" should "split the frenzy" in {
    object Foogaloo extends EcoSystem {
      predator(wolf) eats(deer, min = 10, max = 13) eats(rabbit, min = 10, max = 13)
      herbivore(deer) eats(grass,101,202)
      herbivore(rabbit) eats(grass,101,202)
    }
    var p = new Array[Int](Fauna.values.size)
    p(wolf) = 50
    p(deer) = 1000
    p(rabbit) = 1000
    p(grass) = Int.MaxValue
    val p1 = Foogaloo.cycle(p).pNext
    p1(wolf) should be(55)
    //p1(deer) should be(770)
    //p1(rabbit) should be(1569)
    //val p2 = Foogaloo.cycle(p1).pNext
    //p2(wolf) should be(60)
    //p2(deer) should be(0)
    //p2(rabbit) should be(0)
    //val p3 = Foogaloo.cycle(p2).pNext
    //p3(wolf) should be(3)
    //p3(deer) should be(0)
    //p3(rabbit) should be(0)
  }

}