sealed trait Cell {
  def toString(): String
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
  override def toString: String = {
    def resolveRef(currentRef: ReferenceCell, visited: Set[(Int, Int)]): String = {
      if (visited.contains((currentRef.ix, currentRef.iy))) "cyclic"
      else {
        currentRef.table.getCell(currentRef.ix, currentRef.iy) match {
          case None => "outOfRange"
          case Some(cell: EmptyCell) => "empty"
          case Some(cell: NumberCell) => cell.toString
          case Some(cell: StringCell) => cell.toString
          case Some(cell: ReferenceCell) => resolveRef(cell, visited + ((currentRef.ix, currentRef.iy)))
        }
      }
    }

    resolveRef(this, Set.empty)
  }
}