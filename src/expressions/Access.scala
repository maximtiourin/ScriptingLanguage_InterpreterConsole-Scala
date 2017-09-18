/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Access(obj: Expression, field: Identifier) extends SpecialForm {
	def execute(env: Environment): Value = {
	  val exec = obj.execute(env)
	  
	  if (!exec.isInstanceOf[Environment]) throw new TypeException("Access obj.execute must be an environment")
	  
	  val oenv = exec.asInstanceOf[Environment]
	  
	  val find = oenv.find(field)
	  
	  if (find.toString == Notification.UNKNOWN.toString) {
	    throw new TypeException("Access field(" + field + ") - " + find.asInstanceOf[Notification])
	  }
	  else {
	    find
	  }
	}
}