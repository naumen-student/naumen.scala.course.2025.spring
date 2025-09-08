sealed trait Cell {
  def String(): String
}

class CellEmpty extends Cell {
  override def toString(): String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString(): String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString(): String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def toStringImpl(prev: Option[ReferenceCell] = None, visited: Set[(Int, Int)] = Set.empty): String = {
    if (visited.contains((ix, iy))) {
      "cyclic"
    } else {
      table.getCell(ix, iy) match {
        case Some(_: ReferenceCell) if prev.isDefined && prev.get == this => "cyclic"
        case Some(refCell: ReferenceCell) => refCell.toStringImpl(Some(refCell), visited + ((ix, iy)))
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
    }
  }

  override def toString: String = toStringImpl()
}
