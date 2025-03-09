import scala.collection.mutable

class Table(val width: Int, val height: Int) {

  private val cells: mutable.ArrayBuffer[Cell] = mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && iy >= 0 && ix <= width && iy <= height) {
      Some(cells(ix + iy * width))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) cells(ix + iy * width) = cell
  }
}