class Table(w: Int, h: Int) {

  private val cells: Array[Cell] = Array.fill(w * h)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < w && iy >= 0 && iy < h)
      Some(cells(ix + iy * w))
    else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    cells.update(ix + iy * w, cell)
  }

}