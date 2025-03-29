class Table(val width: Int, val height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def isValid(row: Int, col: Int): Boolean =
    row >= 0 && row < height && col >= 0 && col < width

  def getCell(row: Int, col: Int): Option[Cell] =
    Option.when(isValid(row, col))(cells(row * width + col))

  def setCell(row: Int, col: Int, cell: Cell): Unit = {
    if (isValid(row, col)) cells(row * width + col) = cell
  }
}