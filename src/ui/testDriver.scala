/*
 * Maxim Tiourin
 */

package ui

import values._
import expressions._

object testDriver {
	def main(args: Array[String]) = {
	  //Tests for Values
	  Number.test
	  Boole.test
	  Environment.test
	  
	  //Tests for system.scala
	  try {
		  val add = new Identifier("add")
		  val sub = new Identifier("sub")
		  val mul = new Identifier("mul")
		  val div = new Identifier("div")
		  val equals = new Identifier("equals")
		  val less = new Identifier("less")
		  val numbers = List(new Number(1), new Number(2), new Number(3), new Number(4), new Number(5), new Number(6))
		  val numbers2 = List(new Number(4), new Number(4), new Number(4), new Number(4), new Number(4), new Number(4))
		  val numbers3 = List(new Number(1), new Number(2), new Number(3), new Number(6), new Number(5), new Number(4))
		  val numbers4 = List(new Number(6), new Number(5), new Number(4), new Number(3), new Number(2), new Number(1))
		  println("Testing system\n---------------")
		  println("add(1, 2, 3, 4, 5, 6); Expect = 21, Result = " + system.execute(add, numbers))
		  println("sub(1, 2, 3, 4, 5, 6); Expect = -19, Result = " + system.execute(sub, numbers))
		  println("mul(1, 2, 3, 4, 5, 6); Expect = 720, Result = " + system.execute(mul, numbers))
		  println("div(1, 2, 3, 4, 5, 6); Expect ~ 0.001389, Result = " + system.execute(div, numbers))
		  println("equals(1, 2, 3, 4, 5, 6); Expect = false, Result = " + system.execute(equals, numbers))
		  println("equals(4, 4, 4, 4, 4, 4); Expect = true, Result = " + system.execute(equals, numbers2))
		  println("less(1, 2, 3, 4, 5, 6); Expect = true, Result = " + system.execute(less, numbers))
		  println("less(1, 2, 3, 6, 5, 4); Expect = false, Result = " + system.execute(less, numbers3))
		  println("less(6, 5, 4, 3, 2, 1); Expect = false, Result = " + system.execute(less, numbers4))
		  println()
	  } catch {
	    case e: TypeException => println(e.error)
	    case _: Throwable => println("unknown error") 
	  }
	  
	  //Tests for Expressions
	try {
	  val env = new Environment()
	  val add = new Identifier("add")
	  val sub = new Identifier("sub")
	  val mul = new Identifier("mul")
	  val div = new Identifier("div")
	  val equals = new Identifier("equals")
	  val less = new Identifier("less")
	  val numbers = List(new Number(1), new Number(2), new Number(3), new Number(4), new Number(5), new Number(6))
	  val numbers2 = List(new Number(4), new Number(4), new Number(4), new Number(4), new Number(4), new Number(4))
	  val numbers3 = List(new Number(1), new Number(2), new Number(3), new Number(6), new Number(5), new Number(4))
	  val numbers4 = List(new Number(6), new Number(5), new Number(4), new Number(3), new Number(2), new Number(1))
	  val number = new Number(3)
	  val boole = new Boole(true)
	  val identifier = new Identifier("myIdentifier")
	  
	  val addFunc = new Identifier("addFunc")
	  val subFunc = new Identifier("subFunc")
	  val mulFunc = new Identifier("mulFunc")
	  val divFunc = new Identifier("divFunc")
	  val equalsFunc = new Identifier("equalsFunc")
	  val equalsFunc2 = new Identifier("equalsFunc2")
	  val lessFunc = new Identifier("lessFunc")
	  val lessFunc2 = new Identifier("lessFunc2")
	  val lessFunc3 = new Identifier("lessFunc3")
	  
	  env.put(addFunc, (new FunCall(add, numbers)).execute(env))
	  env.put(subFunc, (new FunCall(sub, numbers)).execute(env))
	  env.put(mulFunc, (new FunCall(mul, numbers)).execute(env))
	  env.put(divFunc, (new FunCall(div, numbers)).execute(env))
	  env.put(equalsFunc, (new FunCall(equals, numbers)).execute(env))
	  env.put(equalsFunc2, (new FunCall(equals, numbers2)).execute(env))
	  env.put(lessFunc, (new FunCall(less, numbers)).execute(env))
	  env.put(lessFunc2, (new FunCall(less, numbers3)).execute(env))
	  env.put(lessFunc3, (new FunCall(less, numbers4)).execute(env))
	  
	  env.put(identifier, new Number(345))
	  
	  println("Testing Expressions\n---------------")
	  println("env find addFunc(1, 2, 3, 4, 5, 6); Expect = 21, Result = " + env.find(addFunc))
	  println("env find subFunc(1, 2, 3, 4, 5, 6); Expect = -19, Result = " + env.find(subFunc))
	  println("env find mulFunc(1, 2, 3, 4, 5, 6); Expect = 720, Result = " + env.find(mulFunc))
	  println("env find divFunc(1, 2, 3, 4, 5, 6); Expect ~ 0.001389, Result = " + env.find(divFunc))
	  println("env find equalsFunc(1, 2, 3, 4, 5, 6); Expect = false, Result = " + env.find(equalsFunc))
	  println("env find equalsFunc2(4, 4, 4, 4, 4, 4); Expect = true, Result = " + env.find(equalsFunc2))
	  println("env find lessFunc(1, 2, 3, 4, 5, 6); Expect = true, Result = " + env.find(lessFunc))
	  println("env find lessFunc2(1, 2, 3, 6, 5, 4); Expect = false, Result = " + env.find(lessFunc2))
	  println("env find lessFunc3(6, 5, 4, 3, 2, 1); Expect = false, Result = " + env.find(lessFunc3))
	  println()
	  println("execute Number(3); Expect = 3, Result = " + number.execute(env))
	  println("execute Boole(true); Expect = true, Result = " + boole.execute(env))
	  println("execute Identifier(\"myIdentifier\") where env value = 345; Expect = 345, Result = " + identifier.execute(env))
	  println()
	  } catch {
	    case e: UndefinedException => println(e.error)
	    case e: TypeException => println(e.error)
	    case _: Throwable => println("unknown error") 
	  }
	}
}