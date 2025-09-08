class Table(width: Int, height: Int ) {

  private val cellsArr: Array[Cell] = {
    Array.fill[Cell](width * height)(new EmptyCell)
  }

  private def inRange(ix: Int, iy: Int): Option[Boolean] = {
    Some(true).filter(_ => ix > -1 && ix < width && iy > -1 && iy < height)
  }

  private def idx(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    inRange(ix, iy).map(_ => cellsArr(idx(ix, iy)))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    inRange (ix, iy).foreach {_ =>
      cellsArr (idx (ix, iy) ) = cell
    }
  }

}

