// Класс таблицы с фиксированной шириной и высотой
class Table(val width: Int, val height: Int) {
  // Храним ячейки в одномерном массиве; индекс вычисляется как: ix + iy * width.
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  // Возвращает ячейку по индексам (ix - номер колонки, iy - номер строки),
  // либо None, если координаты вне границ таблицы.
  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix < 0 || iy < 0 || ix >= width || iy >= height) None
    else Some(cells(ix + iy * width))
  }

  // Устанавливает ячейку по заданным индексам (если индексы корректны).
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && iy >= 0 && ix < width && iy < height) {
      cells(ix + iy * width) = cell
    }
  }
}
