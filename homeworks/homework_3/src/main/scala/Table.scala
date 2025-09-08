class Table(width: Int, height: Int) {

  private val table: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  private def rangeContains(ix: Int, iy: Int): Option[Boolean] = {
    Some(-1 < ix && ix < width &&  -1 < iy && iy < width).filter(p => p)
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    rangeContains(ix, iy).map(_ => table(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    rangeContains(ix, iy).foreach(_ => table(ix + iy * width) = cell)
  }
} 