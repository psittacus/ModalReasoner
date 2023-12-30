package parser

import dto.{Atom, BoxFormula, Bracket, Conjunction, Disjunction, Formula, Implication, NegatedFormula}
import exceptions.{ParseError, SyntaxError}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object Parser {
    private var formulae: ListBuffer[Formula] = new ListBuffer()
    private val atom: Regex = "^[a-zA-Z].+$".r
    private val box: Regex = "^\\\\Box.+$".r
    private val diamond: Regex = "^\\\\Diamond.+$".r
    private val bracketOpen: Regex = "^\\(.+".r
    private val bracketClosed: Regex = "^\\).*".r
    private val and: Regex = "^\\\\land.+$".r
    private val or: Regex = "^\\\\lor.+$".r
    private val impl: Regex = "^\\\\implies.+$".r
    private val neg: Regex = "^\\\\neg.+$".r
    //TODO add negation

    /**
     * Plan: Traverse the input, extract every part of the cnf formula:
     * Box p and (q or p) and Box(Box q implies (not q or not p))
     * --> Box p, (q or p), Box(Box q implies (not q or not p))
     *
     * @param input
     * @return
     */
    def splitCNFintoSeparateFormulae(input: String): (String, ListBuffer[Formula]) = {
        var rest = input
        
        while (!rest.equals("")) {
            if (rest.startsWith(" ")) {
                return splitCNFintoSeparateFormulae(rest.drop(1))
            }

            rest match {
                case box() =>
                    val (r, f) = recursiveParsing(rest.drop(4), 0)
                    rest = r
                    formulae = formulae.append(BoxFormula(f))
                case diamond() =>
                    val (r, f) = recursiveParsing(rest.drop(8), 0)
                    rest = r
                    formulae = formulae.append(NegatedFormula(BoxFormula(NegatedFormula(f))))
                case bracketOpen() =>
                    val (r, f) = recursiveParsing(rest.drop(1), 1)
                    rest = r
                    formulae = formulae.append(Bracket(f))
                case neg() => 
                    val (r, f) = recursiveParsing(rest.drop(4), 0)
                    rest = r
                    formulae = formulae.append(NegatedFormula(f))
                case atom() =>
                    val name = rest.charAt(0)
                    rest = rest.drop(1)
                    formulae = formulae.append(Atom(name.toString))
                case other => throw ParseError("Expected Box, Diamond, ( or atom, but got [" + other + "]")
            }
        }

        (rest, formulae)
    }

    private def recursiveParsing(input: String, brackets: Integer): (String, Formula) = {
        var rest = input
        var formula: Formula = null

        if(rest.isBlank) {
            return (rest, formula)
        }
        
        if (rest.startsWith(" ")) {
            return recursiveParsing(input.drop(1), brackets)
        }

        rest match {
            case box() =>
                val (r, f) = recursiveParsing(rest.drop(4), brackets)
                rest = r
                formula = BoxFormula(f)
            case diamond() =>
                val (r, f) = recursiveParsing(rest.drop(8), brackets)
                rest = r
                formula = NegatedFormula(BoxFormula(NegatedFormula(f)))
            case bracketOpen() =>
                val (r, f) = recursiveParsing(rest.drop(1), brackets + 1)
                rest = r
                formula = Bracket(f)
            case bracketClosed() =>
                if (brackets > 0) {
                    val (r, f) = recursiveParsing(rest.drop(1), brackets - 1)
                    rest = r
                    formula = f
                } else {
                    throw SyntaxError("There seems to be one \")\" to much")
                }
            case and() =>
                if (brackets > 0) {
                    val (r, f) = recursiveParsing(rest.drop(5), brackets)
                    rest = r
                    formula = Conjunction(null, f)
                } else {
                    rest = rest.drop(5)
                }
            case or() =>
                if (brackets > 0) {
                    val (r, f) = recursiveParsing(rest.drop(4), brackets)
                    rest = r
                    formula = Disjunction(null, f)
                } else {
                    throw SyntaxError("Expected \\land but got \\lor. Malformed CNF!")
                }
            case impl() =>
                if (brackets > 0) {
                    val (r, f) = recursiveParsing(rest.drop(8), brackets)
                    rest = r
                    formula = Implication(null, f)
                } else {
                    throw SyntaxError("Expected \\land but got \\implies. Malformed CNF!")
                }
            case neg() =>
                val (r, f) = recursiveParsing(rest.drop(4), brackets)
                rest = r
                formula = NegatedFormula(f)
            case atom() =>
                val name = rest.charAt(0)
                rest = rest.drop(1)
                //if we are in a bracket, do a lookup, if \implies, \land or \lor follows
                 if (brackets > 0) {
                    val (r, f) = recursiveParsing(rest, brackets)
                    f match {
                        case Conjunction(l, r) =>
                            if (l == null) {
                                formula = Conjunction(Atom(name.toString), r)
                            }
                        case Disjunction(l, r) =>
                            if (l == null) {
                                formula = Disjunction(Atom(name.toString), r)
                            }
                        case Implication(l, r) =>
                            if (l == null) {
                                formula = Implication(Atom(name.toString), r)
                            }
                        case _ => 
                            formula = Atom(name.toString)
                    }
                     rest = r
                } else {
                    formula = Atom(name.toString)
                }
            case other => throw ParseError("Expected Box, Diamond, ( or atom, but got [" + other + "]")
        }

        (rest, formula)
    }
}
