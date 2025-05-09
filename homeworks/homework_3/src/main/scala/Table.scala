class Table(val width: Int, val height: Int) {
  private val tableCell: Array[Array[Cell]] = Array.ofDim(height, width)

  for {
    i <- 0 until height
    j <- 0 until width
  } tableCell(i)(j) = new EmptyCell

  def getCell(x: Int, y: Int): Option[Cell] = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      Some(tableCell(y)(x))
    } else {
      None
    }
  }

  def setCell(x: Int, y: Int, cell: Cell): Unit = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      tableCell(y)(x) = cell
    }
  }
}