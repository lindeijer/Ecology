package nl.dgl.ecology
package v2

class StockFlowSystem {

  type TypeOfStock = AnyRef
  type Stock = Map[TypeOfStock,Double] // (node,level) at a particular time

  type FlowRate = (Double,Stock) => Double // flow = stream(stock)  ... deltaStock ... srcOut == dstIn

  var streams = List.empty[(TypeOfStock,FlowRate,TypeOfStock)]

  def stream(src:TypeOfStock,flowRate:FlowRate,dst:TypeOfStock): Unit = {
    streams = streams.appended((src,flowRate,dst));
  }

  def stepFlow(dT: Double,stock:Stock): Stock = {
    val node2flow = streams //
      .map { case (src,flowRate,dst) => (src, flowRate(dT,stock),dst) }
      .flatMap { case (src,flow,dst) => List((src,-flow),(dst,+flow)) }
      .groupMapReduce ({ case (node, _) => node })( {case (_, flow) => flow})( {case (l,r) => l + r} );

    stock .map { case (node, level) => (node ,level + node2flow(node) )}
  }


}


