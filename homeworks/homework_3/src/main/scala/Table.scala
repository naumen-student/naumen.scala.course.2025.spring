class Table(width: Int, height: Int) {
  private val cells: ArrayBuffer[Cell] = ArrayBuffer.fill(width * height)(new EmptyCell)

  // Получение ячейки по индексам
  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      Some(cells(ix + iy * width))
    } else {
      None
    }
  }

  // Установка ячейки по индексам
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(ix + iy * width) = cell
    }
  }
}