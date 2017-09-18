/*
 * Maxim Tiourin
 */

package expressions

import values._
import expressions._

case class Lambda(parameters: List[Identifier] = Nil, body: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
	  new Closure(parameters, body, env)
	}
}