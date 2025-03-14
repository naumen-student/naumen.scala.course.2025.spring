class Table(width: Int, height: Int) {
  private val cellsArr: Array[Cell] = Array.fill[Cell](width * height)(new EmptyCell)

  private def inRange(ix: Int, iy: Int): Boolean = {
    ix >= 0 && ix < width && iy >= 0 && iy < height
  }

  private def idx(ix: Int, iy: Int): Int = ix + iy * width

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (inRange(ix, iy)) {
      Some(cellsArr(idx(ix, iy)))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (inRange(ix, iy)) {
      cellsArr(idx(ix, iy)) = cell
    }
  }

  override def toString: String = {
    (0 until height).map { y =>
      (0 until width).map { x =>
        getCell(x, y).map(_.toString).getOrElse("*empty*")
      }.mkString("\t")
    }.mkString("\n")
  }
}