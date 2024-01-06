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
  var atomName = name
  def print(): String = {
    atomName
  }
}

case class NegatedFormula(formula: Formula) extends Formula {
  var child : Formula = formula

  def print(): String = {
    "¬[" + child.print() + "]"
  }
}

case class Conjunction(left: Formula, right: Formula) extends Formula {
  var leftChild: Formula = left
  var rightChild : Formula = right
  def print(): String = {
    "[" + leftChild.print() + "∧" + rightChild.print() + "]"
  }
}

case class Disjunction(left:Formula,right:Formula) extends Formula {
  var leftChild: Formula = left
  var rightChild: Formula = right
  def print(): String = {
    "[" + leftChild.print() + "∨" + rightChild.print() + "]"
  }
}

case class Implication(left:Formula, right: Formula) extends Formula {
  var leftChild: Formula = left
  var rightChild: Formula = right
  def print(): String = {
    "[" + leftChild.print() + "→" + rightChild.print() + "]"
  }
}

case class Bracket(c: Formula) extends Formula {
  var child: Formula = c
  def print(): String = {
    "("+ child.print() + ")"
  }
}

case class BracketClosed(c: Formula) extends Formula {
  var child: Formula = c

  def print(): String = {
    ")" + child.print()
  }

}

case class BoxFormula(formula: Formula) extends Formula {
  var child: Formula = formula
  def print(): String = {
    "□[" + child.print() + "]"
  }
}