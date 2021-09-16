package nl.dgl.ecology
package v2

import v2.mypackage._

package object mypackage {
  type Fauna = AnyRef
  type TrophicCycle = Population => Double
  type Population = Map[Fauna, Double]
}



class EcoSystem0(val fauna2trophicCycles:Map[Fauna, TrophicCycle]) {

  def this() {
    this(Map.empty)
  }

  def cycle(population: Population): Population = {
    population.map { case (fauna, _) => (fauna, fauna2trophicCycles(fauna)(population)) }
  }

  def +(other: EcoSystem0): EcoSystem0 = {
    new EcoSystem0(this.fauna2trophicCycles ++ other.fauna2trophicCycles)
  }

}

object EcoSystem0 extends EcoSystem0 {

  def fauna(fauna: Fauna,cycle: TrophicCycle): EcoSystem0 = {
    new EcoSystem0(Map(fauna->cycle))
  }

}

