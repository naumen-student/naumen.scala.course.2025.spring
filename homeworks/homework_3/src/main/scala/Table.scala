import scala.collection.mutable.ArrayBuffer

class Table(val width: Int, val height: Int) {
  private val cells: ArrayBuffer[Cell] = ArrayBuffer.fill(width * height)(new EmptyCell)

  private def isValidPosition(ix: Int, iy: Int): Boolean = {
    ix >= 0 && ix < width && iy >= 0 && iy < height
  }

  private def getIndex(ix: Int, iy: Int): Int = {
    ix + iy * width
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isValidPosition(ix, iy)) {
      Some(cells(getIndex(ix, iy)))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isValidPosition(ix, iy)) {
      cells(getIndex(ix, iy)) = cell
    }
  }
}
