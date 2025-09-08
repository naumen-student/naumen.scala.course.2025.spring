trait Cell {
  def toString : String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString: String = {
    if (hasCycle(this, Set())) {
      return "cyclic"
    }

    table.getCell(ix, iy) match {
      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }

  def hasCycle(cell: ReferenceCell, visited: Set[ReferenceCell]): Boolean = {
    if (visited.contains(cell)) true
    else {
      table.getCell(cell.ix, cell.iy) match {
        case Some(ref: ReferenceCell) => hasCycle(ref, visited + cell)
        case _ => false
      }
    }
  }
}