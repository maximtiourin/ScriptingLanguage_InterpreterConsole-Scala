/*
 * Maxim Tiourin
 */

package values

class Variable(val content: Value) extends Value {
	override def toString(): String = {
	  "Variable(" + content.toString + ")"
	}
}