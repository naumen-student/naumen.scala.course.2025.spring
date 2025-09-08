trait Cell[+T] {
  val value: T
}

class EmptyCell extends Cell[String] {
  override val value: String = "empty"
  override def toString: String = value
}

class NumberCell(override val value: Int) extends Cell[Int] {
  override def toString: String = value.toString
}

class StringCell(override val value: String) extends Cell[String] {
  override def toString: String = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell[Cell[Any]] {
  override val value: Cell[Any] = table.getCell(ix, iy).get

  private def safeToString(history: Set[ReferenceCell] = Set.empty): String = table.getCell(ix, iy).map {
    case refCell: ReferenceCell => Some(history.contains(this))
      .filter(res => res)
      .map(_ => "cyclic")
      .getOrElse(refCell.safeToString(history ++ Set(refCell)))
    case cell: Cell[Any] => cell.toString
  }.getOrElse("outOfRange")

  override def toString: String = safeToString()
}
