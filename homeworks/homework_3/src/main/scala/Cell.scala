import scala.annotation.tailrec

trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(val n: Int) extends Cell {
  override def toString: String = n.toString
}

class StringCell(val str: String) extends Cell {
  override def toString: String = str
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {

  override def toString: String = exploreRefChain(Set.empty)

  @tailrec
  private def exploreRefChain(visited: Set[ReferenceCell]): String = {
    val cell = table.getCell(ix, iy)
    cell match {
      case None => "outOfRange"
      case Some(c: ReferenceCell) if (visited.contains(c)) => "cyclic"
      case Some(c: ReferenceCell) => c.exploreRefChain(visited + this)
      case Some(c) => c.toString
    }
  }

}