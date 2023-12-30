package dto

abstract class Formula {
  def print(): String
}

case class Bottom() extends Formula {
  override def print(): String = {
    "⊥"
  }
}

case class Top() extends Formula {
  override def print(): String = {
    "⊤"
  }
}

case class Atom(name: String) extends Formula {
  private def atomName = name
  def print(): String = {
    atomName
  }
}

case class NegatedFormula(formula: Formula) extends Formula {
  private def negatedFormula : Formula = formula
  def print(): String = {
    "¬" + negatedFormula.print()
  }
}

case class Conjunction(left: Formula, right: Formula) extends Formula {
  private def leftChild: Formula = left
  private def rightChild : Formula = right
  def print(): String = {
    leftChild.print() + "∧" + rightChild.print()
  }
}

case class Disjunction(left:Formula,right:Formula) extends Formula {
  private def leftChild: Formula = left
  private def rightChild: Formula = right
  def print(): String = {
    leftChild.print() + "∨" + rightChild.print()
  }
}

case class Implication(left:Formula, right: Formula) extends Formula {
  private def leftChild: Formula = left
  private def rightChild: Formula = right
  def print(): String = {
    leftChild.print() + "→" + rightChild.print()
  }
}

case class Bracket(c: Formula) extends Formula {
  private def child: Formula = c
  def print(): String = {
    "("+ child.print() + ")"
  }
}

case class BoxFormula(formula: Formula) extends Formula {
  def print(): String = {
    "□"
  }
}