package reasoner

import dto.{Atom, Bottom, BoxFormula, Bracket, Conjunction, Disjunction, Formula, Implication, NegatedFormula}

import scala.collection.mutable.ListBuffer

case class Reasoner() {

    def isFormulaSatisfiable(formulae: ListBuffer[Formula]): Boolean = {
        var tempFormulae: ListBuffer[Formula] = formulae
        var newFormulae: ListBuffer[Formula] = new ListBuffer[Formula]
        while (!tempFormulae.equals(newFormulae)) {
            tempFormulae = newFormulae
            newFormulae = beautifyFormulae(formulae)
            newFormulae = andRule(newFormulae)
            newFormulae = doubleNegationRule(newFormulae)
            val o1 = negatedAndRule(newFormulae)
            if(o1.nonEmpty) {
                val formList = o1.get
                if(isFormulaSatisfiable(formList.head)) {
                    return true
                } else {
                    return isFormulaSatisfiable(formList.last)
                }
            }
            val o2 = notBoxRule(newFormulae)
            if(o2.nonEmpty) {
                val branches : List[ListBuffer[Formula]] = o2.get
                for(formula <- branches) {
                    if(!isFormulaSatisfiable(formula)) {
                        return false
                    }
                }
            }
        }

        ???
    }

    /*
     * Helper methods
     */

    private def beautifyFormulae(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        var result: ListBuffer[Formula] = new ListBuffer[Formula]
        // Umformung von: p OR q == -(-p AND -q)
        for (formula <- formulae) {
            result.append(singleFormulaBeautifyFormula(formula))
        }

        result
    }

    private def singleFormulaBeautifyFormula(formula: Formula): Formula = {
        formula match {
            case Disjunction(left, right) =>
                NegatedFormula(Conjunction(NegatedFormula(singleFormulaBeautifyFormula(left)), NegatedFormula(singleFormulaBeautifyFormula(right))))
            case Implication(left, right) =>
                NegatedFormula(Conjunction(singleFormulaBeautifyFormula(left), NegatedFormula(singleFormulaBeautifyFormula(right))))
            case Bracket(f) =>
                singleFormulaBeautifyFormula(f)
            case BoxFormula(f) =>
                BoxFormula(singleFormulaBeautifyFormula(f))
            case NegatedFormula(f) =>
                NegatedFormula(singleFormulaBeautifyFormula(f))    
            case _ =>
                formula
        }
    }

    /*
     * Not Box rule
     */

    private def notBoxRule(formulae: ListBuffer[Formula]): Option[List[ListBuffer[Formula]]] = {
        var result: ListBuffer[Formula] = new ListBuffer[Formula]
        var children: ListBuffer[Formula] = new ListBuffer[Formula]

        //check if all formulae have a leading box
        for (formula <- formulae) {
            if (!propositionalSaturated(formula, 0)) {
                return None
            }

            val t = singleFormulaNotBoxRule(formula, negated = false)
            if (t.nonEmpty) {
                val (form, branch) = t.get
                if (form != null) {
                    if (branch) {
                        children.append(form)
                    } else {
                        result.append(form)
                    }
                }
            } else {
                return None
            }

        }

        var list: List[ListBuffer[Formula]] = List()
        
        for(formula <- children) {
            list = (result ++ ListBuffer(formula)) :: list
        }
        
        Some(list)
    }

    private def singleFormulaNotBoxRule(formula: Formula, negated: Boolean): Option[(Formula, Boolean)] = {
        var result: Formula = null
        var ownBranch = false

        formula match {
            case BoxFormula(f) =>
                result = f
                if (negated) {
                    ownBranch = true
                }
            case NegatedFormula(c) =>
                val o = singleFormulaNotBoxRule(c, negated = true)
                if (o.nonEmpty) {
                    val (form, t) = o.get
                    if (form == null) {
                        return Some(form, t)
                    } else {
                        result = NegatedFormula(form)
                        ownBranch = t
                    }
                }
            case Bracket(f) =>
                val o = singleFormulaNotBoxRule(f, negated)
                if (o.nonEmpty) {
                    val (form, t) = o.get
                    if (form == null) {
                        return Some(result, ownBranch)
                    } else {
                        result = Bracket(form)
                        ownBranch = t
                    }
                } else {
                    return None
                }
            case Atom(_) =>
                result = null //remove formula from tree
            case _ =>
                return None
        }

        Some(result, ownBranch)
    }

    private def propositionalSaturated(formula: Formula, negationCounter: Integer): Boolean = {
        formula match {
            case Bracket(f) =>
                propositionalSaturated(f, negationCounter)
            case NegatedFormula(f) =>
                if (negationCounter > 0) {
                    false
                } else {
                    propositionalSaturated(f, negationCounter + 1)
                }
            case Atom(_) =>
                true
            case _ =>
                false
        }
    }

    /*
     * Axiom rule
     */
    private def axiomRule(formulae: ListBuffer[Formula]): ListBuffer[Formula] = {
        var clash = false

        for (formula <- formulae) {
            for (i <- formulae.indexOf(formula) until formulae.size) {
                if (NegatedFormula(formula).equals(formulae.apply(i))) {
                    clash = true
                }
            }
        }

        if (clash) {
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
            if (r.isEmpty) {
                result.append(formula)
            } else {
                result.append(r.get._1)
            }
        }

        result
    }

    private def singleFormulaDoubleNegationRule(formula: Formula, negations: Integer): Option[(Formula,Boolean)] = {
        var result: Option[(Formula,Boolean)] = null
        var applied = false
        formula match {
            case NegatedFormula(f) =>
                if (negations == 1) {
                    applied = true
                    result = Some(f,applied)
                } else {
                    val opt = singleFormulaDoubleNegationRule(f, negations + 1)
                    if(opt.nonEmpty) {
                        val (form, appl) = opt.get
                        if(appl) {
                            result = Some(form,applied)
                        } else {
                            result = Some(NegatedFormula(form),applied)
                        }
                    } else {
                        result = None
                    }
                }
            case Bracket(f) =>
                val opt = singleFormulaDoubleNegationRule(f, negations)
                if(opt.nonEmpty) {
                    val (form, appl) = opt.get
                    if(appl) {
                        result = Some(form,appl)
                    } else {
                        result = Some(Bracket(form),applied)
                    }
                } else {
                    result = None
                }
            case BoxFormula(f) =>
                val form = singleFormulaDoubleNegationRule(f, negations)
                if(form.nonEmpty) {
                    result = Some(BoxFormula(form.get._1),applied)
                } else {
                    result = None
                }
            case Conjunction(l, r) =>
                val ol = singleFormulaDoubleNegationRule(l, 0)
                val or = singleFormulaDoubleNegationRule(r, 0)
                val sl = if(ol.nonEmpty) ol.get._1 else l
                val sr = if(or.nonEmpty) or.get._1 else r
                result = Some(Conjunction(sl, sr),applied)
            case Disjunction(l, r) =>
                val ol = singleFormulaDoubleNegationRule(l, 0)
                val or = singleFormulaDoubleNegationRule(r, 0)
                val sl = if(ol.nonEmpty) ol.get._1 else l
                val sr = if(or.nonEmpty) or.get._1 else r
                result = Some(Disjunction(sl, sr),applied)
            case _ => 
                return None
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
                        val tmp = singleFormulaAndRule(c)
                        if(tmp.nonEmpty && tmp.size == 2) {
                            twoCandidates = tmp
                            applied = true
                            allFormulae.remove(allFormulae.indexOf(formula))
                        }
                    case Implication(left, right) =>
                        val l = left
                        val r = NegatedFormula(right)
                        twoCandidates = ListBuffer(l, r)
                        applied = true
                        allFormulae.remove(allFormulae.indexOf(formula))
                    case Bracket(f) =>
                        val tmp = singleFormulaNegatedAndRule(f)
                        //if there are two children
                        if (tmp.nonEmpty && tmp.size == 2) {
                            twoCandidates = tmp
                            applied = true
                            allFormulae.remove(allFormulae.indexOf(formula))
                        }
                    case _ => 
                        applied = false
                }
            }
        }

        if (applied) {
            if(twoCandidates.nonEmpty && twoCandidates.size == 2) {
                result = List(allFormulae ++ ListBuffer(twoCandidates.head), allFormulae ++ ListBuffer(twoCandidates.last))
            }
            
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
                case _ =>
                    result.append(formula)
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
            case _ =>
                result.append(formula)
        }

        result
    }
}
