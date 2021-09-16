package nl.dgl.ecology
package v1

import org.apache.spark.sql.SparkSession

object SimpleApp {


    val logFile = "README.md" // Should be some file on your system
    val spark = SparkSession.builder.appName(" Application").getOrCreate()
    val logData = spark.read.textFile(logFile).cache()
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println(s"Lines with a: $numAs, Lines with b: $numBs")
    spark.stop()


}

// ~/Programs/spark-3.1.2-bin-hadoop3.2/bin/spark-submit
// --class "nl.dgl.ecology.v1.SimpleApp"
// --master local[4]  target/scala-2.12/ecology_2.12-0.1.jar
