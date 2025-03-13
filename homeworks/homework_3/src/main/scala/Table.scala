import scala.collection.mutable.ArrayBuffer

class Table(width: Int, height: Int) {
  private val cells = ArrayBuffer.fill[Cell](width * height)(new EmptyCell())

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isInTableBounds(ix, iy)) Some(cells(ix + iy * width)) else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isInTableBounds(ix, iy)) {
      cells(ix + iy * width) = cell
    }
  }

  private def isInTableBounds(ix: Int, iy: Int): Boolean = {
    ix >= 0 && ix < width && iy >= 0 && iy < height
  }
}