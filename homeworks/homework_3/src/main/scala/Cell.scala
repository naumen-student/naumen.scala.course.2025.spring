trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  private def refToCell: Option[Cell] = table.getCell(ix, iy)

  override def toString: String = {
    table.getCell(ix, iy).map {
      case cell: ReferenceCell =>
        cell.refToCell.map(
          referenceCellToNextCell => if (this != referenceCellToNextCell)
            referenceCellToNextCell.toString
          else "cyclic"
        ).getOrElse("outOfRange")

      case cell => cell.toString
    }
  }.getOrElse("outOfRange")
}