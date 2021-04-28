package nl.dgl.ecology
package v2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.collection.mutable

class StockFlowSystemTest extends AnyFlatSpec with should.Matchers {

  "A StockFlowSystem" should "flow stock from A to B" in {

    val sfs : StockFlowSystem = new StockFlowSystem

    val src:sfs.TypeOfStock = "SRC"
    val dst:sfs.TypeOfStock = "DST"

    sfs.stream(src,flowRate = (dT, stock) => {
      stock(src) * dT
    },dst = dst)

    val stock0 = Map(src->5.0,dst->0.0)

    val stock1 = sfs.stepFlow(0.5,stock0)

    stock1(src) should be (2.5)
    stock1(dst) should be (2.5)
  }

}

////////////////////////////////////////////

object CubeCalculator extends App {
  def cube(x: Int) = {
    x * x * x
  }
}

// ---=



class CubeCalculatorTest extends AnyFunSuite {
  test("CubeCalculator.cube") {
    assert(CubeCalculator.cube(3) === 27)
  }
}

///////////////////////////////////////////////////


import scala.collection.mutable

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new mutable.Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new mutable.Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }
}
