/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Disjunction(operands: List[Expression]) extends SpecialForm {
	def execute(env: Environment): Value = {
	  if (operands.isEmpty) throw new TypeException("disjunction expects > 0 inputs")
	  
	  for (op <- operands) {
	    val v1 = op.execute(env)
	    
	    if (v1.isInstanceOf[Boole]) {
	      val v2 = v1.asInstanceOf[Boole]
	      if (v2.value == true) return new Boole(true)
	    }
	    else {
	      throw new TypeException("all disjunction inputs must be booles")
	    }
	  }
	  
	  return new Boole(false)
	}
}