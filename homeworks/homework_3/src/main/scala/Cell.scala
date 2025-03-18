import scala.annotation.tailrec

sealed trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(x: Int, y: Int, table: Table) extends Cell {

  private val col = x
  private val row = y

  private def getCellContent: String = {
    val visitedCells = scala.collection.mutable.Set[ReferenceCell]()

    def resolveReference(ix: Int, iy: Int): String = {
      table.getCell(ix, iy) match {
        case Some(ref: ReferenceCell) if visitedCells.contains(ref) => "cyclic"
        case Some(ref: ReferenceCell) =>
          visitedCells += ref
          resolveReference(ref.col, ref.row)
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
    }

    resolveReference(col, row)
  }

  override def toString: String = getCellContent
}
