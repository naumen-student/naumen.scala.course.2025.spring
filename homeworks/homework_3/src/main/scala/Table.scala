import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  private val cells = mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  private def index(ix: Int, iy: Int): Option[Int] =
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) Some(ix + iy * width)
    else None

  def getCell(ix: Int, iy: Int): Option[Cell] = index(ix, iy).map(cells(_))

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    index(ix, iy).foreach(i => cells(i) = cell)
  }
}