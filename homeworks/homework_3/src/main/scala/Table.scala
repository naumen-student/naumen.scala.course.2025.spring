class Table(width: Int, height: Int){
  private val cells: Array[Cell] = Array.fill[Cell](width * height)(new EmptyCell)
  private def inRange(ix: Int, iy: Int): Boolean = ix >= 0 && ix < width && iy >= 0 && iy < height
  private  def getIndex(ix: Int, iy: Int) : Int = ix + iy * width
  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (inRange(ix, iy))
      Some(cells(getIndex(ix, iy)))
    else
      None
  }
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (inRange(ix, iy))
      cells(getIndex(ix, iy)) = cell
  }
}
