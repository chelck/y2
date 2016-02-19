/**
  * Created by chris on 2/15/16.
  */

case class FieldDotId(name: String) {
    override def toString = name
}

object Dot {

    def createFieldId(id: MessageId, position: Int) = FieldDotId(s"field_${id}_${position}")

    def messageLabel(name: String, id: MessageId, color: String) = {
        s"""[label=\"$name\\n$id", color="$color"]"""
    }

    def fieldLabel(name: String, color: String) = {
        s"""[label=\"$name", color="$color"]"""
    }

    def relationship(parent: String, child: String): String = {
        s"$parent -> $child\n"
    }

    def defaultRelationship(parent: String, child: String): String = {
        s"""$parent -> $child [color="orange"];\n"""
    }

}
