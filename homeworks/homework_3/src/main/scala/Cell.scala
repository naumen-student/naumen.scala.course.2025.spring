sealed trait Cell:
  def toString: String

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(str: String) extends Cell {
  override def toString: String = str
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {

  override def toString: String = toStringImpl()

  private def toStringImpl(cellsHistory: Set[ReferenceCell] = Set.empty): String = {
    table.getCell(ix, iy).map{
      case referenceCell: ReferenceCell =>
        if(!cellsHistory.contains(this))
          referenceCell.toStringImpl(cellsHistory ++ Set(this))
        else
          "cyclic"
      case cell: Cell => cell.toString
    }.getOrElse("outOfRange")
  }
}

