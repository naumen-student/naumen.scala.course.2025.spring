import scala.collection.mutable.ArrayBuffer

class Table(val width: Int, val height: Int) {
  private val cells: ArrayBuffer[Cell] = ArrayBuffer.fill(width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix > width || iy > height)
      None
    else Some(cells(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height)
      cells(ix + iy * width) = cell
  }

}