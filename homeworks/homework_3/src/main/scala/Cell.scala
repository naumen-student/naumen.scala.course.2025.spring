import scala.annotation.tailrec

trait Cell {

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
  private def getRefCell : Option[Cell] = table.getCell(ix, iy)

  @tailrec
  private def toStringWithRefs(set: Set[ReferenceCell] = Set.empty, current: ReferenceCell) : String = {
    current.getRefCell match {
      case Some(referenceCell: ReferenceCell) => if (set contains referenceCell) "cyclic"
      else toStringWithRefs(set + current, referenceCell)
      case Some(cell: Cell) => cell.toString
      case None => "outOfRange"
    }
  }
  override def toString: String = {
    toStringWithRefs(current = this)
    }
}