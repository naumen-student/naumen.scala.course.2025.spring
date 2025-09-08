// Cell.scala
trait Cell {
    def toString: String
    }

    class EmptyCell extends Cell {
    override def toString: String = "empty"
    }

    class NumberCell(number: Int) extends Cell {
    override def toString: String = number.toString
    }

    class StringCell(text: String) extends Cell {
    override def toString: String = text
    }

    class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
    override def toString: String = {
        if (table.isCyclicReference(ix, iy, this)) {
        return "cyclic"
        }
        table.getCell(ix, iy) match {
        case Some(cell) => cell.toString
        case None => "outOfRange"
        }
    }
}
