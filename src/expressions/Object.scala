/*
 * Maxim Tiourin
 */

package expressions

import ui._
import values._

case class Object(exps: List[Expression]) extends SpecialForm {
	def execute(env: Environment): Value = {
	  val localenv = new Environment(env)
	  
	  if (exps.length > 0) {
		  exps.map(_.execute(localenv))
		  localenv
	  }
	  else {
	    throw new TypeException("object requires > 0 expressions")
	  }
	}
}