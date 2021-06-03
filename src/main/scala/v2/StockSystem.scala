package nl.dgl.ecology
package v2

class StockFlowSystem {

  type TypeOfStock = AnyRef
  type Stock = Map[TypeOfStock,Double] // (node,level) at a particular time

  type FlowRate = (Double,Stock) => Double // flow = stream(stock)  ... deltaStock ... srcOut == dstIn

  var streams = List.empty[(TypeOfStock,FlowRate,TypeOfStock)]

  def stream(src:TypeOfStock,flowRate:FlowRate,dst:TypeOfStock): Unit = {
    streams = streams :+ (src,flowRate,dst);
  }

  def stepFlow(dT: Double,stock:Stock): Stock = {

    def addFlows (l:Double,r:Double):Double = l+r

    val node2flow = streams //
      .map { case (src,flowRate,dst) => (src, flowRate(dT,stock),dst) }
      .flatMap { case (src,flow,dst) => List((src,-flow),(dst,+flow)) }
      // .groupMapReduce ({ case (node, _) => node })  ( {case (_, flow) => flow})  ( {case (l,r) => l + r} );
      //    def groupMapReduce[K, B](key: (A) => K)(f: (A) => B)(reduce: (B, B) => B): Map[K, B]
      //    It is equivalent to groupBy(key).mapValues(_.map(f).reduce(reduce)), but more efficient.
      .groupBy({ case (node, _) => node })
      .mapValues(  _.map( { case (_, flow) => flow } ).reduce( addFlows ))

    stock .map { case (node, level) => (node ,level + node2flow(node) )}
  }


}


