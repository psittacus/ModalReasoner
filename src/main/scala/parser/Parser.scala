package parser

import dto.{Atom, BoxFormula, Bracket, BracketClosed, Conjunction, Disjunction, Formula, Implication, NegatedFormula}
import exceptions.{ParseError, SyntaxError}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object Parser {
    private var formulae: ListBuffer[Formula] = new ListBuffer()
    private val atom: Regex = "^[a-zA-Z⊥⊤].*$".r
    private val box: Regex = "^\\\\Box.+$".r
    private val diamond: Regex = "^\\\\Diamond.+$".r
    private val bracketOpen: Regex = "^\\(.+".r
    private val bracketClosed: Regex = "^\\).*".r
    private val and: Regex = "^\\\\land.+$".r
    private val or: Regex = "^\\\\lor.+$".r
    private val impl: Regex = "^\\\\implies.+$".r
    private val neg: Regex = "^\\\\neg.+$".r

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
                case and() =>
                    rest = rest.drop(5)
                case other => throw ParseError("Expected Box, Diamond, ( or atom, but got [" + other + "]")
            }
        }

        (rest, formulae)
    }

    //TODO Conjunction, Disjunction und Implication werden nicht correct geparsed: \Diamond p \implies \Diamond q wird \Diamond(p\implies\Diamond q)
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
                f match {
                    case Conjunction(left, right) =>
                        formula = Conjunction(BoxFormula(left), right)
                    case Disjunction(left, right) =>
                        formula = Disjunction(BoxFormula(left), right)
                    case Implication(left, right) =>
                        formula = Implication(BoxFormula(left), right)
                    case _ =>
                        formula = BoxFormula(f)
                }
            case diamond() =>
                val (r, f) = recursiveParsing(rest.drop(8), brackets)
                rest = r
                f match {
                    case Conjunction(left, right) =>
                        formula = Conjunction(NegatedFormula(BoxFormula(NegatedFormula(left))), right)
                    case Disjunction(left, right) =>
                        formula = Disjunction(NegatedFormula(BoxFormula(NegatedFormula(left))), right)
                    case Implication(left, right) =>
                        formula = Implication(NegatedFormula(BoxFormula(NegatedFormula(left))), right)
                    case _ =>
                        formula = NegatedFormula(BoxFormula(NegatedFormula(f)))
                }
            case bracketOpen() =>
                val (r, f) = recursiveParsing(rest.drop(1), brackets + 1)
                rest = r
                //e.g.:             (p \lor (q \land r) \lor \neg q)
                // and we are here:         -----------
                formula = Bracket(f)
                if(brackets > 0) {
                    val (re, fo) = recursiveParsing(rest, brackets)
                    if(fo != null) {
                        fo match {
                            case Conjunction(_, right) =>
                                formula = Conjunction(formula, right)
                            case Disjunction(_, right) =>
                                formula = Conjunction(formula, right)
                            case Implication(_, right) =>
                                formula = Implication(formula, right)
                            case other =>
                                throw ParseError("Expected Conjunction, Disjunction or Implication, got: " + other)
                        }
                    }
                    rest = re
                }
            case bracketClosed() =>
                if (brackets > 0) {
                    /*
                    val (r, f) = recursiveParsing(rest.drop(1), brackets - 1)
                    rest = r
                    formula = f
                    */
                    rest = rest.drop(1)
                    formula = null
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
                        case Conjunction(left, right) =>
                            if (left == null) {
                                formula = Conjunction(Atom(name.toString), right)
                            }
                        case Disjunction(left, right) =>
                            if (left == null) {
                                formula = Disjunction(Atom(name.toString), right)
                            }
                        case Implication(left, right) =>
                            if (left == null) {
                                formula = Implication(Atom(name.toString), right)
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
