import scala.collection.mutable

class Table(width: Int, height: Int) {
  private val cells = mutable.Map[(Int, Int), Cell]()

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) cells.get((ix, iy)).orElse(Some(new EmptyCell))
    else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells((ix, iy)) = cell
    }
  }
}