trait Cell {
  def toString: String
}
class EmptyCell extends Cell {
  override def toString: String = "empty"
}
class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}
class StringCell(val text: String) extends Cell {
  override def toString: String = text
}
class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  private def toStringImpl(visited: Set[ReferenceCell] = Set()): String = {
    if (visited.contains(this)) {
      "cyclic"
    } else {
      table.getCell(ix, iy) match {
        case Some(refCell: ReferenceCell) => refCell.toStringImpl(visited + this)
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
    }
  }

  override def toString: String = toStringImpl()
}