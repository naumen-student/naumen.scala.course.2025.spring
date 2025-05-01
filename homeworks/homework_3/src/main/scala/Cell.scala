import scala.annotation.tailrec

trait Cell {
  def toString : String
}

class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

class NumberCell(val num: Int) extends Cell {
  override def toString: String = num.toString
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class ReferenceCell(x: Int, y: Int, table: Table) extends Cell {
  @tailrec
  private def getStringCell(prev: Option[ReferenceCell] = None): String = {
    table.getCell(x, y) match {
      case Some(_ : ReferenceCell)
        if prev.isDefined && prev.get == this => "cyclic"
      case Some(cell : ReferenceCell) =>
        cell.getStringCell(Some(cell))
      case Some(cell) =>
        cell.toString
      case None =>
        "outOfRange"
    }
  }

  override def toString: String = getStringCell()
}