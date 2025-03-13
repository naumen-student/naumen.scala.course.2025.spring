import scala.annotation.tailrec

trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class StringCell(text: String) extends Cell {
  override def toString: String = text
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell { // Добавили val, чтобы ix и iy были доступны
  override def toString: String = {
    if (isCyclicReference(ix, iy, Set.empty[(Int, Int)])) {
      "cyclic"
    } else {
      table.getCell(ix, iy) match {
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
    }
  }

  @tailrec
  private def isCyclicReference(x: Int, y: Int, visitedCells: Set[(Int, Int)]): Boolean = {
    if (visitedCells.contains((x, y))) {
      return true
    }
    table.getCell(x, y) match {
      case Some(cell: ReferenceCell) =>
        isCyclicReference(cell.ix, cell.iy, visitedCells + ((x, y)))
      case _ => false
    }
  }
}
