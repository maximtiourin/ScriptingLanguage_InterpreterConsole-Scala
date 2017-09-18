/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class Identifier(name: String) extends Expression with Serializable {
	def execute(env: Environment): Value = {
	  val v = env.find(this)
	  if (v.toString == Notification.UNKNOWN.toString) throw new UndefinedException(name)
	  else v
	}
	
	override def toString(): String = {
	  name
	}
}