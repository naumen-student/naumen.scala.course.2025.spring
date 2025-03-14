import scala.collection.mutable

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
  override def toString: String = {
    if (ix < 0 || iy < 0 || ix >= table.width || iy >= table.height) {
      "outOfRange"
    }
    else {
      table.getCell(ix, iy) match {
        case Some(refCell: ReferenceCell) =>
          if (refCell.isCyclic) "cyclic"
          else refCell.toString
        case Some(cell) => cell.toString
        case None => "outOfRange"
      }
    }
  }

  def isCyclic: Boolean = {
    var current: Option[Cell] = Some(this)
    val visited = mutable.Set[ReferenceCell]()

    while (current.isDefined) {
      current.get match {
        case ref: ReferenceCell =>
          if (visited.contains(ref)) return true
          visited.add(ref)
          current = table.getCell(ref.ix, ref.iy)
        case _ => return false
      }
    }
    false
  }
}