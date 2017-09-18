/*
 * Maxim Tiourin
 */

package expressions

import ui._
import values._

case class Block(locals: List[Expression]) extends SpecialForm {
	def execute(env: Environment): Value = {
	  val localenv = new Environment(env)
	  
	  if (locals.length > 1) {
		  val rvlc = locals.reverse //Get reverse list so the last expression in the block can be returned by head
		  
		  rvlc.tail.reverse.map(_.execute(localenv)) //Take all of the tail expressions, reverse them back, and execute in order
		  rvlc.head.execute(localenv) //Save the last expression of the locals for last, execute it, and return it as the value
	  }
	  else if (locals.length == 1) {
	    locals.head.execute(localenv) //Just execute the only expression and return it as the value
	  }
	  else {
	    throw new TypeException("block requires > 0 expressions")
	  }
	}
}