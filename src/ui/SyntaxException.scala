/*
 * Maxim Tiourin
 */

package ui

import scala.util.parsing.combinator._

class SyntaxException(val result: Parsers#Failure = null) extends InterpreterException("Syntax Error") {

}