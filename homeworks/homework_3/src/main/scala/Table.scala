class Table(val width: Int, val height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def getCellIndex(x: Int, y: Int) = x + y * width

  def getCell(x: Int, y: Int) : Option[Cell] = {
    if (x < 0 || y < 0 || x >= width ||  y >= height)
      None
    else
      Some(cells(getCellIndex(x, y)))
  }

  def setCell(x: Int, y: Int, cell: Cell) : Unit = {
    cells(getCellIndex(x, y)) = cell
  }
}