// Определяем базовый интерфейс для ячеек
trait Cell {
  // Метод resolve используется для рекурсивного разрешения ссылки с передачей набора уже посещённых координат.
  // Для обычных ячеек (не ReferenceCell) по умолчанию возвращается toString.
  def resolve(visited: Set[(Int, Int)]): String = toString
  override def toString: String
}

// Пустая ячейка
class EmptyCell extends Cell {
  override def resolve(visited: Set[(Int, Int)]): String = "empty"
  override def toString: String = "empty"
}

// Ячейка с 32-битным целым числом
class NumberCell(val number: Int) extends Cell {
  override def toString: String = number.toString
}

// Ячейка с текстом
class StringCell(val str: String) extends Cell {
  override def toString: String = str
}

// Ячейка-ссылка на другую ячейку таблицы
class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  override def resolve(visited: Set[(Int, Int)]): String = {
    // Если целевая координата уже встречалась в цепочке разрешения, то обнаружен цикл.
    if (visited.contains((ix, iy))) "cyclic"
    else {
      table.getCell(ix, iy) match {
        case None => "outOfRange" // Если координаты за пределами таблицы
        case Some(cell) =>
          // Рекурсивно разрешаем ссылку, добавляя текущую целевую координату в visited.
          cell.resolve(visited + ((ix, iy)))
      }
    }
  }
  override def toString: String = resolve(Set())
}
