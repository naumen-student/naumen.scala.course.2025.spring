trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private var evaluating = false

  override def toString: String = {
    if (evaluating) "cyclic"
    else {
      evaluating = true
      val result = table.getCell(ix, iy) match {
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
      evaluating = false
      result
    }
  }
}