/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Conditional(operands: List[Expression]) extends SpecialForm {
	def execute(env: Environment): Value = {
	  if (operands.length <= 1 || operands.length > 3) throw new TypeException("conditional expects > 1 inputs with a maximum of 3")
	  val op0 = operands(0).execute(env)
	  val ok = op0.isInstanceOf[Boole]
	  if (!ok) throw new TypeException("first conditional operand must evaluate as boole")
	  val condition = op0.asInstanceOf[Boole]
	  
	  if (condition.value == true) {
	    //TRUE -> DO OPERAND1
	    operands(1).execute(env)
	  }
	  else if (operands.length == 3) {
	    //FALSE -> DO OPERAND2
	    operands(2).execute(env)
	  }
	  else {
	    //FALSE -> DO NOTHING
	    Notification.OK
	  }
	}
}