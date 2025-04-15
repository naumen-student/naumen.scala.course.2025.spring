class Table(width: Int, height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def isValid(x: Int, y: Int): Boolean =
    x >= 0 && x < height && y >= 0 && y < width

  def getCell(x: Int, y: Int): Option[Cell] = {
    if (isValid(x, y)) {
      return Some(cells(y * width + x))
    }
    None
  }

  def setCell(x: Int, y: Int, cell: Cell): Unit = {
    if (isValid(x, y)) {
      cells(y * width + x) = cell
    }
  }
}