package reasoner

import dto.{Bracket, Conjunction, Formula, Implication, NegatedFormula}

import scala.collection.mutable.ListBuffer

case class Reasoner(formulae: ListBuffer[Formula]) {

    def negatedAndRule(formulae: ListBuffer[Formula]): Option[List[ListBuffer[Formula]]] = {
        var applied = false
        var allFormulae: ListBuffer[Formula] = formulae
        var result: List[ListBuffer[Formula]] = null
        var twoCandidates: ListBuffer[Formula] = new ListBuffer[Formula]
        
        /*
         * traverse all formulae. 
         * If the negatedAnd rule can be applied, remove the formula from allFormulae
         *  and create two separate formulae (twoCandidates) which will be the branches we continue on 
         */
        for (formula <- formulae) {
            if (!applied) {
                formula match {
                    case NegatedFormula(c) =>
                        twoCandidates = singleFormulaAndRule(c)
                        applied = true
                        allFormulae.remove(allFormulae.indexOf(formula))
                    case Implication(left, right) =>
                        val l = left
                        val r = NegatedFormula(right)
                        twoCandidates = ListBuffer(l, r)
                        applied = true
                        allFormulae.remove(allFormulae.indexOf(formula))
                    case Bracket(f) =>
                        var res = singleFormulaNegatedAndRule(f)
                        //if there are two children
                        if (res.nonEmpty && res.size == 2) {
                            twoCandidates = res
                            applied = true
                            allFormulae.remove(allFormulae.indexOf(formula))
                        }

                }
            }
        }

        result = List(allFormulae ++ ListBuffer(twoCandidates.head), allFormulae ++ ListBuffer(twoCandidates.last))
        
        if(applied) {
            Some(result)
        } else {
            None
        }
    }

    private def singleFormulaNegatedAndRule(formula: Formula): ListBuffer[Formula] = {
        var result: ListBuffer[Formula] = new ListBuffer[Formula]

        formula match {
            case NegatedFormula(f) =>
                result = singleFormulaAndRule(f)
            case Bracket(f) =>
                result = singleFormulaNegatedAndRule(f)
            case other =>
                result.append(other)
        }

        result
    }

    //AND rule
    def andRule(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        var result: ListBuffer[Formula] = new ListBuffer[Formula]

        for (formula <- formulae) {
            formula match {
                case Conjunction(left, right) =>
                    result.append(left)
                    result.append(right)
                case Bracket(c) =>
                    result.appendAll(singleFormulaAndRule(c))
                case rest =>
                    result.append(rest)
            }
        }

        result
    }

    private def singleFormulaAndRule(formula: Formula): ListBuffer[Formula] = {
        val result: ListBuffer[Formula] = new ListBuffer[Formula]

        formula match {
            case Conjunction(left, right) =>
                result.append(left)
                result.append(right)
            case Bracket(c) =>
                val f = singleFormulaAndRule(c)
                //means and rule could not be applied
                if (f.size == 1) {
                    result.append(formula)
                } else {
                    result.appendAll(f)
                }
        }

        result
    }
}
