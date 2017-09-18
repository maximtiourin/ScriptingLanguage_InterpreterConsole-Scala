/*
 * Maxim Tiourin
 */

package values

import expressions._

class Boole(val value: Boolean) extends Literal with Value {	
	def this(bool: String) {
	  this(bool.toBoolean)
	}
	
	def &&(other: Boole): Boole = {
	  new Boole(value && other.value)
	}
	
	def ||(other: Boole): Boole = {
	  new Boole(value || other.value)
	}
	
	def unary_! = new Boole(!value)
	
	override def toString(): String = {
	  value + ""
	}
}

object Boole {
  def apply(v: Boolean) { new Boole(v) }
  def test {
    val t = new Boole(true)
    val f = new Boole(false)
    
    println("Testing Boole\n---------------")
    println("true boole from string 'true' => Expect: true, Result: " + (new Boole("true")))
    println("false boole from string 'false' => Expect: false, Result: " + (new Boole("false")))
    println()
    println("true && false => Expect: false, Result: " + (t && f))
    println("false && true => Expect: false, Result: " + (f && t))
    println("true && true => Expect: true, Result: " + (t && t))
    println("false && false => Expect: false, Result: " + (f && f))
    println()
    println("true || false => Expect: true, Result: " + (t || f))
    println("false || true => Expect: true, Result: " + (f || t))
    println("true || true => Expect: true, Result: " + (t || t))
    println("false || false => Expect: false, Result: " + (f || f))
    println()
    println("!true => Expect: false, Result: " + (!t))
    println("!false => Expect: true, Result: " + (!f))
    println("!(!true) => Expect: true, Result: " + (!(!t)))
    println("!(!false) => Expect: false, Result: " + (!(!f)))
    println()
    println("(true && false) || ((true && true) && !(false || false)) => Expect: true, Result: " + ((t && f) || ((t && t) && !(f || f))))
    println()
  }
}