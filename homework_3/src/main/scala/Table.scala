// Table.scala
import scala.collection.mutable.ArrayBuffer

class Table(width: Int, height: Int) {
    private val cells: ArrayBuffer[Cell] = ArrayBuffer.fill(width * height)(new EmptyCell)

    private def getIndex(ix: Int, iy: Int): Int = ix + iy * width

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
        Some(cells(getIndex(ix, iy)))
        } else {
        None
        }
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
        cells(getIndex(ix, iy)) = cell
        }
    }

    def isCyclicReference(ix: Int, iy: Int, refCell: ReferenceCell): Boolean = {
        var currentCell: Cell = refCell
        var visitedCells = Set[Cell]()

        while (currentCell.isInstanceOf[ReferenceCell]) {
        if (visitedCells.contains(currentCell)) {
            return true
        }
        visitedCells += currentCell
        val ref = currentCell.asInstanceOf[ReferenceCell]
        currentCell = getCell(ref.ix, ref.iy).getOrElse(new EmptyCell)
        }
        false
    }
}