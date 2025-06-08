trait Cell {
  def toString(): String
}

class EmptyCell extends Cell {
  override def toString(): String = "empty"
}

class NumberCell(val number: Int) extends Cell {
  override def toString(): String = number.toString
}

class StringCell(val text: String) extends Cell {
  override def toString(): String = text
}

class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def toString(): String = {
    var visited = Set[(Int, Int)]()

    def resolve(x: Int, y: Int, depth: Int = 0): String = {
      if (visited.contains((x, y))) return "cyclic"
      visited += ((x, y))

      table.getCell(x, y) match {
        case None => "outOfRange"
        case Some(cell) => cell match {
          case ref: ReferenceCell => resolve(ref.ix, ref.iy, depth + 1)
          case cell => cell.toString()
        }
      }
    }

    resolve(ix, iy)
  }
}
