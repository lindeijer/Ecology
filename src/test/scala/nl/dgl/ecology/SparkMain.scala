package nl.dgl.ecology

import org.apache.spark.sql.{Row, SparkSession}

import Fauna._

object Boogaloo extends EcoSystem {

  predator(wolf) eats(deer, min = 10, max = 15) // eats(rabbit, min = 20, max = 40)
  herbivore(deer) eats(grass,1,2)
  // predator(fox) eats(rabbit, min = 11, max = 20)
  plant(grass)

}

object Main {

  def cycle () : Seq[Row] = {

    var populations = Seq[Row]()

    println("Fauna.values.size="+Fauna.values.size)

    var population = new Array[Int](Fauna.values.size)

    population(wolf) = 11
    population(deer) = 400
    //population(rabbit) = 0
    //population(fox) = 0
    population(grass) = Int.MaxValue

    populations = populations :+ Row.fromSeq(population)

    var a = 0;
    for (a <- 1 to 10) {
      println("-----")
      val ecoCycleNext = Boogaloo.cycle(population)
      println("  predatorCycles="+ecoCycleNext.predatorCycles)
      println("  predatorCycles="+ecoCycleNext.herbivoreCycles)
      println("  pNext="+Fauna.toMap(ecoCycleNext.pNext))
      populations = populations :+ Row.fromSeq(population)
      population = ecoCycleNext.pNext;
    }

    populations

  } // end of main

  def main(args: Array[String]): Unit = {
    cycle();
  }

}





object SparkMain {

  val spark : SparkSession = SparkSession.builder().getOrCreate()

  def main(args: Array[String]): Unit = {

    import org.apache.spark.sql.types.{IntegerType, StructField, StructType};

    var populations :Seq[Row] = Main.cycle()

    val empSchema = Fauna.values.toList . map(f=>StructField(f.toString,IntegerType,true))

    val pops = // populations.toDF();
      spark.createDataFrame(//
        spark.sparkContext.parallelize(populations) , // RDD[T] RDD[Array[Int]]
        StructType(empSchema))

    pops.createOrReplaceTempView("pops");

  }
}

