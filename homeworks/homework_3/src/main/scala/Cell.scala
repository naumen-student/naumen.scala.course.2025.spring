trait Cell

class EmptyCell extends Cell{
  override def toString: String = "empty"
}

class NumberCell(num: Int) extends Cell {
  override def toString: String = num.toString
}

class StringCell(str: String) extends Cell{
  override def toString: String = str
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  private def getRefCell: Option[Cell] = table.getCell(ix, iy)
  private def toStringImpl(refCellHistory : Set[ReferenceCell] = Set.empty) : String = {
    getRefCell match {
      case Some(refCell: ReferenceCell) =>
        if (refCellHistory.contains(this)) "cyclic"
        else refCell.toStringImpl(refCellHistory + refCell)
      case Some(cell: Cell) => cell.toString
      case None => "outOfRange"
    }
  }
  override def toString: String = toStringImpl()
}
