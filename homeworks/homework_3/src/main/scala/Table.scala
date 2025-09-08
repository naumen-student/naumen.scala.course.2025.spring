class Table(width: Int, height: Int) {
  private def indexing (x: Int, y: Int): Int =  x + y * width
  private val cells: Array[Cell[Any]] = Array.fill[Cell[Any]](width * height)(new EmptyCell)
  private def isInRange(x: Int, y: Int): Option[Boolean] = {
    Some(x >= 0 && x < width && y >= 0 && y < height).filter(res => res)
  }

  def getCell(x: Int, y: Int): Option[Cell[Any]] = {
    isInRange(x, y)
      .map(_ => cells(indexing(x, y)))
  }

  def setCell(x: Int, y: Int, cell: Cell[Any]): Unit = {
    isInRange(x, y)
      .filter(res => res)
      .foreach(_ => cells(indexing(x, y)) = cell)
  }
}
