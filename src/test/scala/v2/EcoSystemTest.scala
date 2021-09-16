package nl.dgl.ecology
package v2

import v2.mypackage.{Fauna, Population, TrophicCycle}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object TestFauna extends Enumeration {
  val Wolf, Deer, Rabbit = Value
}

import v2.EcoSystem0._
import v2.TestFauna._

class EcoSystemTest extends AnyFlatSpec with should.Matchers {

  "An EcoSystem" should "let rabbits grow and grow" in {
4
    def lotkaVolterra(fna: Fauna, birthRate: Double, interactions: Map[Fauna, Double]): TrophicCycle = p => {
      p(fna) * birthRate + interactions.map({ case (f, i) => p(fna) * p(f) * i }).sum
    }

    val ecoSystem = fauna(Rabbit, p => p(Rabbit) + 1) +
      //fauna(Wolf,   p => 0.9 * p(Wolf) - 0.01 * p(Deer) * p(Wolf)) +
      //fauna(Deer,   p => 1.1 * p(Deer) + 0.01 * p(Wolf) * p(Deer))
      fauna(Wolf, lotkaVolterra(Wolf, 0.9, Map(Deer -> -0.01))) + //  p => 0.9 * p(Wolf) - 0.01 * p(Deer) * p(Wolf)) +
      fauna(Deer, lotkaVolterra(Deer, 1.1, Map(Wolf -> 0.01))) //  p => 1.1 * p(Wolf) + 0.01 * p(Wolf) * p(Deer)) +


    var p0: Population = Map(Rabbit -> 52, Wolf -> 10, Deer -> 1000)
    var p1 = ecoSystem.cycle(p0)
    var p2 = ecoSystem.cycle(p1)

    //

    p0(Rabbit) should be(52)
    p1(Rabbit) should be(53)
    p2(Rabbit) should be(54)

    //

    p0(Wolf) should be(10)
    p0(Deer) should be(1000)

    p1(Wolf) should be(-91)
    p1(Deer) should be(1200.0)

    p2(Wolf) should be(1010.1)
    p2(Deer) should be(228.0)


  }

}