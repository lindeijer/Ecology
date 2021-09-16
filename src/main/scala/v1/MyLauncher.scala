package nl.dgl.ecology
package v1

import org.apache.spark.launcher.SparkAppHandle
import org.apache.spark.launcher.SparkLauncher

object MyLauncher {

  val sahl = new SparkAppHandle.Listener() {
    override def stateChanged(handle: SparkAppHandle): Unit = ???
    override def infoChanged(handle: SparkAppHandle): Unit = ???
  };

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val handle = new SparkLauncher()
      .setAppResource("target/scala-2.12/ecology_2.12-0.1.jar")
      .setMainClass("nl.dgl.ecology.v1.SimpleApp")
      .setMaster("spark://makemake:7077")
      .setConf(SparkLauncher.DRIVER_MEMORY, "2g")
      .setSparkHome("/home/david/Programs/spark-3.1.2-bin-hadoop3.2")
      .startApplication()
    // Use handle API to monitor / control application.
    //handle.apply(sahl)
  }
}


// ~/Programs/spark-3.1.2-bin-hadoop3.2/bin/spark-submit
// --class "nl.dgl.ecology.v1.SimpleApp"
// --master local[4]  target/scala-2.12/ecology_2.12-0.1.jar