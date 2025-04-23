abstract class Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(val value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(val row: Int, val col: Int, val table: Table) extends Cell {
  override def toString: String = {
    def checkCycle(r: Int, c: Int, visited: Set[(Int, Int)]): Boolean = {
      val pos = (r, c)
      if (visited.contains(pos)) {
        return true
      }
      
      table.getCell(r, c) match {
        case Some(ref: ReferenceCell) => 
          checkCycle(ref.row, ref.col, visited + pos)
        case _ => false
      }
    }
    
    if (checkCycle(row, col, Set.empty)) {
      "cyclic"
    } else {
      table.getCell(row, col) match {
        case Some(cell) => cell.toString
        case None => "empty"
      }
    }
  }
}
