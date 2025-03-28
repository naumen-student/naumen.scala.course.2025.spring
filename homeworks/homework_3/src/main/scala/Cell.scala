trait Cell {
  def toString: String
}

case class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

case class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

case class StringCell(content: String) extends Cell {
  override def toString: String = content
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = {
    def resolve(ref: ReferenceCell, visited: Set[ReferenceCell]): String = {
      if (visited.contains(ref)) "cyclic"
      else ref.table.getCell(ref.ix, ref.iy) match {
        case None => "outOfRange"
        case Some(cell) => cell match {
          case nextRef: ReferenceCell => resolve(nextRef, visited + ref)
          case other => other.toString
        }
      }
    }
    resolve(this, Set.empty)
  }
}