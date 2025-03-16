trait Cell {
  def asString(visited: Set[ReferenceCell]): String

  override def toString: String = asString(Set.empty)
}

class EmptyCell extends Cell {
  override def asString(visited: Set[ReferenceCell]): String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def asString(visited: Set[ReferenceCell]): String = number.toString
}

class StringCell(text: String) extends Cell {
  override def asString(visited: Set[ReferenceCell]): String = text
}

class ReferenceCell(row: Int, col: Int, table: Table) extends Cell {
  override def asString(visited: Set[ReferenceCell]): String =
    if (visited.contains(this)) "cyclic"
    else table.getCell(row, col).fold("outOfRange")(_.asString(visited + this))
}
