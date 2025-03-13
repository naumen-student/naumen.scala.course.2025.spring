trait Cell {
  def toString(): String
}

// Пустая ячейка
class EmptyCell extends Cell {
  override def toString(): String = "empty"
}

// Ячейка с числом
class NumberCell(number: Int) extends Cell {
  override def toString(): String = number.toString
}

// Ячейка с текстом
class StringCell(text: String) extends Cell {
  override def toString(): String = text
}

// Ячейка с ссылкой на другую ячейку
class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString(): String = {
    val targetCellOpt = table.getCell(ix, iy)
    targetCellOpt match {
      case Some(targetCell) =>
        if (targetCell == this) "cyclic"
        else targetCell.toString()
      case None => "outOfRange"
    }
  }
}