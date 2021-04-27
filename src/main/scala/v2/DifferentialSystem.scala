package nl.dgl.ecology
package v2

// System dynamics (SD) is an approach to understanding the nonlinear behaviour of complex systems over time using
// stocks, flows, internal feedback loops, table functions and time delays.

// System Dynamics is a methodology to analyse complex dynamic systems
// The primary elements of system dynamics diagrams are feedback, accumulation of flows into stocks and time delays.

class DifferentialSystem { // Dynamic System

  type TypeOfStock = AnyRef

  type Stock = Map[TypeOfStock,Double] // at a particular time

  type StockChangeRate = (Double,Stock) => Double // change = rate(stock)

  var stockType2stockChangeRate = Map.empty[AnyRef,StockChangeRate]

  def setStockChangeRate(typeOfStock:AnyRef,scr:StockChangeRate): Unit = stockType2stockChangeRate += (typeOfStock->scr);


  def stepStockChange(dT: Double,stock:Stock): Stock = {
    val st2scr = stockType2stockChangeRate // the StockChangeRates should not change during this function.
    stock.map { case (typeOfStock, _) => (typeOfStock, st2scr(typeOfStock)(dT,stock)) }
  }

  def stepStock(dT: Double,stock:Stock): Stock = {
    stepStockChange(dT,stock) map { case (typeOfStock, stockChange) => (typeOfStock, stock(typeOfStock) + stockChange ) }
  }
}

// for Lotka-Volterra dT must be 1 ...
// for stock flow in - or one is the + of another : A = B = N .... but then endless supply ....




