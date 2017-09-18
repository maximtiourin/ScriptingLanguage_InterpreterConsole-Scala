/*
 * Maxim Tiourin
 */

package values

import expressions._
import ui._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val localEnv = new Environment(defEnv)
    
    if (args.length != params.length) {
      throw new TypeException("closure argument list " + args + " must be same length as parameter list " + params)
    }
    else {
      localEnv.put(params, args) //Bind params to args
      body.execute(localEnv) //Execute body given local environment
    }
  }
  
  def clone(env: Environment): Closure = {
    new Closure(params, body, env)
  }
}