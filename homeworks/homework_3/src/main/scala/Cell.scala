sealed trait Cell

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
  private def getIx: Int = ix
  private def getIy: Int = iy

  override def toString: String = {
    toStringImpl(Set.empty)
  }

  private def toStringImpl(cellsHistory: Set[ReferenceCell]): String = {
    table.getCell(ix, iy).map {
      case referenceCell: ReferenceCell =>
        if (cellsHistory.contains(referenceCell)) {
          "cyclic"
        } else {
          referenceCell.toStringImpl(cellsHistory + this)
        }
      case cell: Cell => cell.toString
    }.getOrElse("outOfRange")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val table = new Table(3, 3)

    // Заполнение ячеек
    table.setCell(0, 0, new NumberCell(42))
    table.setCell(1, 1, new StringCell("Hello"))
    table.setCell(2, 2, new ReferenceCell(0, 0, table))

    // Вывод содержимого таблицы
    println(table)
  }
}