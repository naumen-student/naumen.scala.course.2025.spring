class Table(width: Int, height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def getCellIndex(ix: Int, iy: Int) = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width || iy >= height)
      None
    else
      Some(cells(getCellIndex(ix, iy)))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = cells(getCellIndex(ix, iy)) = cell
}