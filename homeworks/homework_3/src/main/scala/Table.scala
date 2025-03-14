
class Table(width: Int, height: Int) {
  private val cells = Array.fill[Cell](width * height)(new EmptyCell)
  private def getIndex(ix: Int, iy: Int) = ix + iy * width

  def getCell(ix: Int, iy: Int) : Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width  || iy >= height) None
    else Some(cells(getIndex(ix, iy)))
  }

  def setCell(ix: Int, iy: Int, cell: Cell) : Unit = {
    cells(getIndex(ix, iy)) = cell
  }
}