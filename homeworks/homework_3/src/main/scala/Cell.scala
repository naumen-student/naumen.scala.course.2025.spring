trait Cell {
  def toString : String
}

class EmptyCell extends Cell { // Пустая ячейка
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell { // Ячейка с 32-битным целым числом
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell { // Ячейка с текстом
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell { // Ячейка, содержащая ссылку на другую ячейку
  val x: Int = ix
  val y: Int = iy

  override def toString(): String = {
    if (x < 0 || x >= table.w || y < 0 || y >= table.h) {
      "outOfRange"
    } else {
      val visitedCells = collection.mutable.Set[ReferenceCell]()

      def isCyclic(nextCell: ReferenceCell): Boolean = {
        if (visitedCells.contains(nextCell)) true
        else {
          visitedCells.add(nextCell)
          table.getCell(nextCell.x, nextCell.y) match {
            case Some(cell: ReferenceCell) => isCyclic(cell)
            case None => false
            case _ => false
          }
        }
      }

      def retrieveCell(cell: ReferenceCell): String = {
        table.getCell(cell.x, cell.y) match {
          case Some(cell: ReferenceCell) => retrieveCell(cell)
          case Some(cell) => cell.toString
          case None => "outOfRange"
        }
      }

      if (isCyclic(this)) {
        "cyclic"
      } else {
        retrieveCell(this)
      }
    }
  }
}