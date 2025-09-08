class Table(val rows: Int, val cols: Int) {
  private val cells: Array[Array[Cell]] = Array.fill(rows, cols)(new EmptyCell)

  def getCell(row: Int, col: Int): Option[Cell] = {
    if (row >= 0 && row < rows && col >= 0 && col < cols) {
      Some(cells(row)(col))
    } else {
      None
    }
  }

  def setCell(row: Int, col: Int, cell: Cell): Unit = {
    if (row >= 0 && row < rows && col >= 0 && col < cols) {
      cells(row)(col) = cell
    }
  }
}
