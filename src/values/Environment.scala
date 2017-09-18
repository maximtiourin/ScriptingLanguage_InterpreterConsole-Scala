/*
 * Maxim Tiourin
 */

package values

import scala.collection.mutable.HashMap
import expressions._

class Environment(val nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {  
	def put(names: List[Identifier], vals: List[Value]) {
	  for ((n, v) <- (names zip vals)) {
	    this.put(n, v)
	  }
	}
	
	def find(id: Identifier): Value = {
	  if (this.contains(id)) {
	    get(id).get
	  }
	  else if (nextEnv != null) {
	    nextEnv.find(id)
	  }
	  else {
	    Notification.UNKNOWN
	  }
	}
	
	/*
	 * Returns both the value found, and the environment in which it was found, to aid with assignment
	 */
	def findWithEnv(id: Identifier): (Value, Environment) = {
	  if (this.contains(id)) {
	    (get(id).get, this)
	  }
	  else if (nextEnv != null) {
	    nextEnv.findWithEnv(id)
	  }
	  else {
	    (Notification.UNKNOWN, this)
	  }
	}
  
	def next(): Environment = {
	  nextEnv
	}
}

object Environment {
  def apply(nextEnv: Environment = null) { new Environment(nextEnv) }
  def test {
    val env: Environment = new Environment()
    
    val x = new Identifier("x")
    val y = new Identifier("y")
    val isWorking = new Identifier("isWorking")
    val isReady = new Identifier("isReady")
    
    val names = List(x, y, isWorking, isReady)
    val vals: List[Value] = List(new Number(5), new Number(3), new Boole(true), new Boole(false))
    
    env.put(names, vals)
    
    println("Testing Environment\n---------------")
    println("Find Identifier 'x', value : expect = 5, result = " + env.find(x))
    println("Find Identifier 'y', value : expect = 3, result = " + env.find(y))
    println("Find Identifier 'isWorking', value : expect = true, result = " + env.find(isWorking))
    println("Find Identifier 'isReady', value : expect = false, result = " + env.find(isReady))
    println("Find Identifier 'z', value : expect = unknown, result = " + env.find(new Identifier("z")))
    println("Find Identifier 'quit', value : expect = unknown, result = " + env.find(new Identifier("quit")))
    println()
  }
}