class Table(val width: Int, val height: Int) {
  private val cells: Array[Array[Cell]] = Array.fill(height, width)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width || iy >= height) None
    else Some(cells(iy)(ix))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < width && iy < height) {
      cells(iy)(ix) = cell
    }
  }
}