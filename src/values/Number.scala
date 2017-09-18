/*
 * Maxim Tiourin
 */

package values

import expressions._

class Number(val value: Double) extends Literal with Value {	
	def this(num: String) {
		this(num.toDouble)
	}
  
	def +(other: Number): Number = {
	  new Number(value + other.value)
	}
	
	def -(other: Number): Number = {
	  new Number(value - other.value)
	}
	
	def *(other: Number): Number = {
	  new Number(value * other.value)
	}
	
	def /(other: Number): Number = {
	  new Number(value / other.value)
	}
	
	def ==(other: Number): Boole = {
	  new Boole(value == other.value)
	}
	
	def <(other: Number): Boole = {
	  new Boole(value < other.value)
	}
	
	override def toString(): String = {
	  value + ""
	}
}

object Number {
	def apply(v: Double) = { new Number(v) }
	def test() {
	  val x = new Number(100)
	  val y = new Number(25)
	  val xs = new Number("145.5")
	  val ys = new Number("54.5")
    
	  println("Testing Number\n---------------")
	  println("x = " + x)
	  println("y = " + y)
	  println("xs = " + xs)
	  println("ys = " + ys)
	  println()
	  println("xs + ys => Expect: 200, Result: " + (xs + ys))
	  println()
	  println("x + y => Expect: 125, Result: " + (x + y))
	  println("y + x => Expect: 125, Result: " + (y + x))
	  println()
	  println("x - y => Expect: 75, Result: " + (x - y))
	  println("y - x => Expect: -75, Result: " + (y - x))
	  println()
	  println("x * y => Expect: 2500, Result: " + (x * y))
	  println("y * x => Expect: 2500, Result: " + (y * x))
	  println()
	  println("x / y => Expect: 4, Result: " + (x / y))
	  println("y / x => Expect: 0.25, Result: " + (y / x))
	  println()
	  println("x == y => Expect: false, Result: " + (x == y))
	  println("x == new Number(100) => Expect: true, Result: " + (x == new Number(100)))
	  println("y == new Number(25) => Expect: true, Result: " + (y == new Number(25)))
	  println()
	  println("x < y => Expect: false, Result: " + (x < y))
	  println("y < x => Expect: true, Result: " + (y < x))
	  println()
	}
}