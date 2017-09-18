/*
 * Maxim Tiourin
 */

package ui

import expressions._
import values._

object console {
  val parser = new InterpreterParser
  val globalEnv = new Environment()
  
  def execute(cmmd: String): String = {
    val tree = parser.parseAll(parser.expression, cmmd)
    tree match {
      case t: parser.Failure => throw new SyntaxException(t)
      case _ => "" + tree.get.execute(globalEnv)
    }
  }
  
  def repl() {
    var quit = false
    while (!quit) {
      try {
        print("-> ")
        val cmd = readLine()
        if (cmd == "quit") {
          println("Bye")
          quit = true
        } 
        else {
          println(execute(cmd))
        }
      } 
      catch {
        case e: SyntaxException => {
          println(e.error)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: UndefinedException => {
          println("Undefined identifier: " + e.error)
        }
        case e: TypeException => {
          println(e.error)
        }
      } 
      finally {
        Console.flush
      }
    }
  }
  
  def main(args: Array[String]) {
    repl
  }
}