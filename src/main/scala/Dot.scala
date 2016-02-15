/**
  * Created by chris on 2/15/16.
  */

case class FieldDotId(name: String) {
    override def toString = name
}

object Dot {

    def createFieldDotId(id: Id, position: Int) = FieldDotId(s"field_${id}_${position}")

    def messageLabel(name: String, id: Id, color: String) = {
        s"""[label=\"$name\\n$id", color="$color"]"""
    }

    def fieldLabel(name: String, color: String) = {
        s"""[label=\"$name", color="$color"]"""
    }

}
