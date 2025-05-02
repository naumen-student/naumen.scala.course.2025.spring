import scala.collection.mutable

class Table(val width: Int, val height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  def getCell(row: Int, col: Int): Option[Cell] = {
    if (row < 0 || row >= height || col < 0 || col >= width) None
    else Some(cells(row * width + col))
  }

  def setCell(row: Int, col: Int, cell: Cell): Unit = {
    if (row >= 0 && row < height && col >= 0 && col < width) {
      cells(row * width + col) = cell
    }
  }
}
