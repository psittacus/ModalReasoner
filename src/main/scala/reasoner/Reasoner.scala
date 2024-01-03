package reasoner

import dto.{Bottom, Bracket, Conjunction, Formula, Implication, NegatedFormula}

import scala.collection.mutable.ListBuffer

case class Reasoner(formulae: ListBuffer[Formula]) {

    def isFormulaSatisfiable(formulae: ListBuffer[Formula]): Boolean = {
        
    }
    
    /*
     * Axiom rule
     */
    private def axiomRule(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        var clash = false 
        
        for(formula <- formulae) {
            for(i <- formulae.indexOf(formula) until formulae.size) {
                if(NegatedFormula(formula).equals(formulae.apply(i))) {
                    clash = true
                }
            }
        }
        
        if(clash) {
            ListBuffer(Bottom())
        } else {
            formulae
        }
    }
    
    /*
     * Double negation rule
     */
    private def doubleNegationRule(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        val result: ListBuffer[Formula] = new ListBuffer[Formula]
        
        for (formula <- formulae) {
            val r = singleFormulaDoubleNegationRule(formula, 0)
            if(r.isEmpty) {
                result.append(formula)
            } else {
                result.append(r.get)
            }
        }
        
        result
    }
    
    private def singleFormulaDoubleNegationRule(formula: Formula, negations: Integer): Option[Formula] = {
        var result: Option[Formula] = null
        formula match {
            case NegatedFormula(f) => 
                if(negations == 1) {
                    result = Some(f)
                } else {
                    result = singleFormulaDoubleNegationRule(f, negations + 1)
                }
            case Bracket(f) => 
                result = singleFormulaDoubleNegationRule(f, negations)
            case _ => 
                result = None
        }
        
        result
    }
    /*
     * Negated and rule 
     */
    private def negatedAndRule(formulae: ListBuffer[Formula]): Option[List[ListBuffer[Formula]]] = {
        var applied = false
        val allFormulae: ListBuffer[Formula] = formulae
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
    private def andRule(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        val result: ListBuffer[Formula] = new ListBuffer[Formula]

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
