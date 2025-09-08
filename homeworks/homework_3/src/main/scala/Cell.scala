import scala.collection.mutable

trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = resolveReference(Set())

  private def resolveReference(visited: Set[(Int, Int)]): String = {
    if (visited.contains((ix, iy))) "cyclic"
    else table.getCell(ix, iy) match {
      case Some(cell: ReferenceCell) => cell.resolveReference(visited + ((ix, iy)))
      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }
}