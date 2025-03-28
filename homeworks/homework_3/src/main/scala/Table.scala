class Table(width: Int, height: Int) {
    private val table = Array.fill[Cell](width * height)(new EmptyCell)

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) Some(table(ix + iy * width))
        else None
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) table(ix + iy * width) = cell
    }
}
