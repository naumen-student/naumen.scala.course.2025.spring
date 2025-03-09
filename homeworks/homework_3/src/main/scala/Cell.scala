trait Cell {
 def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {

  private[this] var visited = false

  override def toString: String = {
    def resetVisited(): Unit = visited = false

    (ix >= 0 && iy >= 0 && ix < table.width && iy < table.height) match {
      case false => "outOfRange"
      case true => visited match {
        case true => "cyclic"
        case false => visited = true
          try {
            table.getCell(ix, iy) match {
              case Some(cell) => cell.toString()
              case None => "outOfRange"
            }
          } finally {
            resetVisited()
          }
      }
    }
  }
}