import exceptions.ParseError
import parser.Parser
import reasoner.Reasoner

import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        val source = Source.fromFile("./src/main/resources/input.tex")
        var input = source.getLines().mkString("", "\n", "\n")
        
        input = input.filter(_ >= ' ')
        source.close()

        val (rest, formula) = Parser.splitCNFintoSeparateFormulae(input)

        if (!rest.isEmpty()) {
            throw ParseError("Could not parse formula completely")
        }
        
        val reasoner = Reasoner(formula)
    }   
}
