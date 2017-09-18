/*
 * Maxim Tiourin
 */

package ui

import expressions._
import values._

object system {
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => less(args)
      case "not" => not(args)
      case "var" => varFunc(args)
      case "val" => valFunc(args)
      case "clone" => clone(args)
      case "write" => write(args)
      case "read" => read(args)
      case "prompt" => prompt(args)
      case _ => throw new UndefinedException(opcode.name)
    }
  }
  
  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduceLeft(_+_)
  }
  
  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduceLeft(_-_)
  }
  
  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduceLeft(_*_)
  }
  
  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    val tailArgs = args2.tail
    if (tailArgs.filter(_.value == 0.0).length > 0) throw new TypeException("division can not divide by zero")
    args2.reduceLeft(_/_)
  }
  
  private def equals(vals: List[Value]): Value = {
    if (vals.length <= 1) throw new TypeException("equality expects > 1 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all equality inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    val cmp = args2(0)
    val equality = args2.tail.filterNot(_.value == cmp.value)
    if (equality.length > 0) new Boole(false) else new Boole(true)
  }
  
  private def less(vals: List[Value]): Value = {
    if (vals.length <= 1) throw new TypeException("less than comparison expects > 1 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all less than comparison inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
       
    def helper(cmp: Number, list: List[Number]): Value = {
      if (list.length > 0) {
        if ((cmp < list.head).value) helper(list.head, list.tail) else new Boole(false)
      }
      else {
        new Boole(true)
      }
    }
    
    helper(args2.head, args2.tail)
  }
  
  private def not(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("not expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("all not inputs must be booles")
    val args2 = vals.map(_.asInstanceOf[Boole])
    new Boole(args2.filter(_.value == true).length <= 0)
  }
  
  /* Returns a variable containing the value */
  private def varFunc(vals: List[Value]): Value = {
    if (vals.isEmpty || vals.length > 1) throw new TypeException("var expects 1 input")
    new Variable(vals.head)
  }
  
  /* Returns the value contained within the variable */
  private def valFunc(vals: List[Value]): Value = {
    if (vals.isEmpty || vals.length > 1) throw new TypeException("val expects 1 input")
    if (vals.head.isInstanceOf[Variable]) {
      vals.head.asInstanceOf[Variable].content
    }
    else {
      throw new TypeException("val expects a variable to dereference")
    }
  }
  
  /* Returns a copy of the environment, making sure to properly copy closures */
  private def clone(vals: List[Value]): Value = {
    if (vals.isEmpty || vals.length > 1) throw new TypeException("clone expects 1 input")
    if (vals.head.isInstanceOf[Environment]) {
      val master = vals.head.asInstanceOf[Environment]
      val copy = new Environment(master.nextEnv)
      
      var newvalues: List[Value] = Nil
      
      for (value <- master.values.toList) {
        if (value.isInstanceOf[Closure]) {
          newvalues = newvalues :+ value.asInstanceOf[Closure].clone(copy)
        }
        else {
          newvalues = newvalues :+ value
        }
      }
      
      copy.put(master.keys.toList, newvalues)
      copy
    }
    else {
      throw new TypeException("clone expects an environment to copy")
    }
  }
  
  private def write(vals: List[Value]): Value = {
    println(vals(0))
    Notification.DONE
  }
  
  private def read(vals: List[Value]): Value = {
    val result = readDouble()
    Number(result)
  }
  
  private def prompt(vals: List[Value]): Value = {
    print("=> ") 
    Notification.DONE
  }
}