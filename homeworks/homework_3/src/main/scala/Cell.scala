trait Cell {
  def asString(visited: Set[ReferenceCell]): String

  override def toString: String = asString(Set.empty)
}

class EmptyCell extends Cell {
  def asString(visited: Set[ReferenceCell]): String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  def asString(visited: Set[ReferenceCell]): String = number.toString
}

class StringCell(val text: String) extends Cell {
  def asString(visited: Set[ReferenceCell]): String = text
}

class ReferenceCell(val row: Int, val col: Int, val table: Table) extends Cell {
  def asString(visited: Set[ReferenceCell]): String =
    if (visited(this)) "cyclic"
    else
      table.getCell(row, col) match {
        case None       => "outOfRange"
        case Some(cell) => cell.asString(visited + this)
      }
}
