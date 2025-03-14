import scala.collection.mutable

class Table(width: Int, height: Int) {
  private val cells: Array[Array[Cell]] = Array.fill(height)(Array.fill(width)(new EmptyCell))

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) Some(cells(iy)(ix))
    else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(iy)(ix) = cell
    }
  }

  def resolveReference(ix: Int, iy: Int, visited: Set[(Int, Int)]): String = {
    if (visited.contains((ix, iy))) "cyclic"
    else getCell(ix, iy) match {
      case Some(cell: ReferenceCell) => resolveReference(cell.ix, cell.iy, visited + ((ix, iy)))
      case Some(cell) => cell.toString
      case None => "outOfRange"
    }
  }
}