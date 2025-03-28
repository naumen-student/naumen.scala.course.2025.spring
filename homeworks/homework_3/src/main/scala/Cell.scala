trait Cell {
    def toString : String
}

// Пустая ячейка
class EmptyCell extends Cell {
    override def toString(): String = "empty"
}

// Ячейка с числом
class NumberCell(number: Int) extends Cell {
    override def toString(): String = number.toString
}

// Ячейка с текстом
class StringCell(text: String) extends Cell {
    override def toString(): String = text
}

// Ячейка с ссылкой на другую ячейку
class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
    override def toString: String = table.getCell(ix, iy).map {
        case refCell: ReferenceCell =>
            table.getCell(refCell.ix, refCell.iy).filter(_ != this).map(_.toString).getOrElse("cyclic")
        case cell: Cell => cell.toString
    }.getOrElse("outOfRange")
}