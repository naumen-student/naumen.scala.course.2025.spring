class Table(width: Int, height: Int) {
    private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        if (ix < 0 || iy < 0 || ix >= width || iy >= height) {
            None
        } else {
            Some(cells(iy * width + ix))
        }
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
            cells(iy * width + ix) = cell
        }
    }
}