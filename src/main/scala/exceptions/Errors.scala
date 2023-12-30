package exceptions

final case class ParseError(private val message: String = "") extends Exception(message)
final case class SyntaxError(private val message: String = "") extends Exception(message)
