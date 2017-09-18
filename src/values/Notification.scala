/*
 * Maxim Tiourin
 */

package values

class Notification(val msg: String) extends Value {
	override def toString(): String = {
	  msg
	}
}

object Notification {
  def apply(msg: String) = { new Notification(msg) }
  def VARIABLE_UPDATED() = apply("variable updated")
  def BINDING_CREATED() = apply("binding created")
  def UNKNOWN() = apply("unknown")
  def ERROR() = apply("error")
  def OK() = apply("ok")
  def DONE() = apply("done")
}