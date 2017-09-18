/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Assignment(id: Identifier, exp: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
	  val (findvalue, findenv) = env.findWithEnv(id)
	  
	  if (findvalue.toString == Notification.UNKNOWN.toString) throw new UndefinedException(id.name)
	  if (!findvalue.isInstanceOf[Variable]) throw new TypeException("Assignment cannot assign new value to non-variable")
	  
	  findenv.put(id, new Variable(exp.execute(env)))
	  Notification.DONE
	}
}