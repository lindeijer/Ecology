package nl.dgl.ecology
package v2

class EcoSystem {

  var fauna2cycle = Map.empty[Fauna.Value, Cycle]

  def fauna(fauna: Fauna.Value, cycle: Cycle): Unit = fauna2cycle += (fauna -> cycle)

  def cycle(population: Population): Population = {
    val f2c = fauna2cycle; // or is ref fauna2cycle unchanged within this cycle whilst fauna is called concurrently?
    population.map { case (fauna, _) => (fauna, f2c(fauna)(population)) }
  }

  type Cycle = Population => Int
  type Population = Map[Fauna.Value, Int]

}