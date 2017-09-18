/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Conjunction(operands: List[Expression]) extends SpecialForm {
	def execute(env: Environment): Value = {
	  if (operands.isEmpty) throw new TypeException("conjunction expects > 0 inputs")
	  
	  for (op <- operands) {
	    val v1 = op.execute(env)
	    
	    if (v1.isInstanceOf[Boole]) {
	      val v2 = v1.asInstanceOf[Boole]
	      if (v2.value == false) return new Boole(false)
	    }
	    else {
	      throw new TypeException("all conjunction inputs must be booles")
	    }
	  }
	  
	  return new Boole(true)
	}
}