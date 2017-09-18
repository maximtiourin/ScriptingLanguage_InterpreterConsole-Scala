/*
 * Maxim Tiourin
 */

package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class InterpreterParser extends RegexParsers {
	/* EXPRESSION */
	def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")
	
	/* LAMBDA */
	def lambda: Parser[Expression] = "lambda" ~> parameters ~ expression ^^ {
	  case p~e => Lambda(p, e)
	}
	
	/* PARAMETERS */
	def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
	  case None => Nil
	  case Some(id1 ~ Nil) => List(id1)
	  case Some(id1 ~ ids) => id1 +: ids
	  case _ => Nil
	}
	
	/* BLOCK */
	def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
	  case exp1 ~ Nil => Block(List(exp1))
	  case exp1 ~ exps => Block(exp1 +: exps)
	}
	
	/* DECLARATION */
	def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
	  case d~id~eq~exp => Declaration(id, exp)
	}
	
	/* ASSIGNMENT */
	def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
	  case id ~ "=" ~ exp => Assignment(id, exp)
	}
	
	/* DEREF */
	def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
	  case v => FunCall(Identifier("val"), List(v))
	}
	
	/* ITERATION */
	def iteration: Parser[Expression] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
	  case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => Iteration(List(exp1, exp2))
	}
	
	/* OBJECT */
	def obj: Parser[Expression] = "object" ~> "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
	  case exp ~ Nil => Object(List(exp))
	  case exp ~ exps => Object(exp +: exps)
	}
	
	/* ACCESS */
	def access: Parser[Expression] = term ~ opt("." ~> identifier) ^^ {
	  case t ~ None => t
	  case t ~ Some(id) => Access(t, id)
	}
	
	/* CONDITIONAL */
	def conditional: Parser[Expression] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
	  case i~p1~exp1~p2~exp2~None => Conditional(List(exp1, exp2))
	  case i~p1~exp1~p2~exp2~Some(els~exp3) => Conditional(List(exp1, exp2, exp3))
	}
	
	/* DISJUNCTION */
	def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
	  case tree1 ~ Nil => tree1
	  case tree1 ~ tree2 => Disjunction(tree1 +: tree2)
	}
	
	/* CONJUNCTION */
	def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
	  case tree1 ~ Nil => tree1
	  case tree1 ~ tree2 => Conjunction(tree1 +: tree2)
	}
	
	/* EQUALITY */
	def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
	  case tree1 ~ Nil => tree1
	  case tree1 ~ tree2 => FunCall(Identifier("equals"), tree1 +: tree2)
	}
	
	/* INEQUALITY */
	def inequality: Parser[Expression] = sum ~ rep("<" ~> sum) ^^ {
	  case tree1 ~ Nil => tree1
	  case tree1 ~ tree2 => FunCall(Identifier("less"), tree1 +: tree2)
	}
	  
	/* SUM */
	def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product ^^ { case "+" ~ p => p case "-" ~ p => negate(p) }) ^^ {
      case p ~ Nil => p
      case p ~ rest => FunCall(Identifier("add"), p +: rest)
	}
	
	/* PRODUCT */
	def product: Parser[Expression] = funcall ~ rep(("*"|"/") ~ funcall ^^ { case "*" ~ f => f case "/" ~ f => inverse(f) }) ^^ {
	  case f ~ Nil => f
	  case f ~ rest => FunCall(Identifier("mul"), f +: rest)
	}
	
	/* FUNCALL*/
	def funcall: Parser[Expression] = access ~ opt(operands) ^^ {
	  case t ~ None => t
	  case t ~ Some(ops) => FunCall(t, ops)
	}
	
	/* OPERANDS */
	def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
	  case None => Nil
	  case Some(exp1 ~ exptree) => exp1 +: exptree
	}
	
	/* TERM */
	def term: Parser[Expression] = obj | assignment | iteration | deref | lambda | block | literal | identifier | "(" ~> expression <~ ")"
	
	/* LITERAL */
	def literal: Parser[Literal] = boole | number
	
	/* IDENTIFIER */
	def identifier: Parser[Identifier] = """[a-zA-Z][0-9a-zA-Z]*""".r ^^ {
	  case id => Identifier(id)
	}
	
	/* BOOLE */
	def boole: Parser[Boole] = ("true" | "false") ^^ {
	  case bool => new Boole(bool.toBoolean)
	}
	
	/* NUMBER */
	def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
	  case num => new Number(num.toDouble)
	}
	
	
	//Helper Functions -------------------------------------------------------------------------
	def negate(exp: Expression): Expression = {
	  FunCall(Identifier("sub"), List(Number(0), exp))
	}
	
	def inverse(exp: Expression): Expression = {
	  FunCall(Identifier("div"), List(Number(1), exp))
	}
}