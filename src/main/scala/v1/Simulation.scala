package nl.dgl.ecology
package v1



case class Fauna(minPop:Int,rr:Double) {}

object Simulation {

  val Deer = Fauna(1000,2.3)
  val Wolf = Fauna(10,2.3)


  def main_(args: Array[String]): Unit = {
    println("Hello, world!")

    val interaction: Map[Fauna,Map[Fauna,Double]]  = Map(//
      Wolf -> Map(Deer -> 0.001) , //
      Deer -> Map(Wolf -> -0.01) //
    )

    val population = Map(Wolf -> 33, Deer -> 52)

    println("population="+population)


    val cycle: ((Fauna,Int)) => (Fauna,Int) = {
      case (fauna, pop) =>
        val reproPop = pop * fauna.rr;
        val interPop = interaction(fauna).map(fi=> fi match { case (f,i)=>pop*population(f)*i }).sum
        (fauna, (reproPop +interPop).asInstanceOf[Int])
    }

    val nextPop = population.map(cycle)

    println("nextPop="+nextPop)

    val nnextPop = nextPop.map(cycle);
    println("nnextPop="+nnextPop)

  }

}





