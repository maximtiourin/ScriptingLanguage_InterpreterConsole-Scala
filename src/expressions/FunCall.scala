/*
 * Maxim Tiourin
 */

package expressions

import values._
import ui._

case class FunCall(operator: Expression, operands: List[Expression] = Nil) extends Expression {
	def execute(env: Environment): Value = {	  
	  val args: List[Value] = operands.map(_.execute(env))	  
	  
	  try {
	    val opexec = operator.execute(env)
	    
	    if (opexec.isInstanceOf[Closure]) {
	      //It is a closure
	      opexec.asInstanceOf[Closure].apply(args)
	    }
	    else {
	      //Not a closure, but didn't throw UndefinedException, so just try system anyway
	      system.execute(operator.asInstanceOf[Identifier], args)
	    }
	  }
	  catch {
	    case e: UndefinedException => {
	      //Operator is an identifier that is undefined, try system...
	      system.execute(operator.asInstanceOf[Identifier], args)
	    }
	  }
	}
}