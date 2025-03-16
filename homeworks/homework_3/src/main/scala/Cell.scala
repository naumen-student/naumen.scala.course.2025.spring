trait Cell {
  def toString: String
}

class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def getReferenceCells(previousCells: Set[ReferenceCell] = Set.empty): String = {
    table.getCell(ix, iy) match {
      case Some(refCell: ReferenceCell) =>
        if (previousCells.contains(refCell))
          "cyclic"
        else
          refCell.getReferenceCells(previousCells + this)
      case Some(cell: Cell) => cell.toString
      case None => "outOfRange"
    }
  }

  override def toString: String = getReferenceCells()
}