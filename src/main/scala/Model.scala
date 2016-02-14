
sealed trait Field

final case class PrimitiveField(name: String, command: String, position: Int) extends Field
final case class MiscField(name: String, command: String) extends Field
final case class UnknownField(name: String, command: String, position: Int) extends Field

final case class ReferenceField(name: String,
                                position: Int,
                                version: Option[String],
                                xxx: Option[Int]) extends Field


case class Id(n: Int) {
    override def toString = n.toString
}

case class Message(name: String, id: Id, fields: Seq[Field]) {

    def dot: String = {
        s"""
           |$id [label="$name", color="blue"];
      """.stripMargin
    }
}



object Model {

    def createField(name: String, keys: Map[String, String]): Field = {
        val command = keys("read_command")
        val position = keys("position").toInt

        command match {
            case "addCurrentTimestamp" => MiscField(name, command)
            case "addCurrentTimestampSec" => MiscField(name, command)
            case "addInteger" => MiscField(name, command)
            case "readBinfile" => MiscField(name, command)

            case "readByte" => PrimitiveField(name, command, position)
            case "readInt" => PrimitiveField(name, command, position)
            case "readShort" => PrimitiveField(name, command, position)
            case "readUnsignedByte" => PrimitiveField(name, command, position)
            case "readUnsignedShort" => PrimitiveField(name, command, position)
            case "readStringArray" => PrimitiveField(name, command, position)
            case "readLong" => PrimitiveField(name, command, position)
            case "readQfloat" => PrimitiveField(name, command, position)
            case "readByteArray" => PrimitiveField(name, command, position)

            case "reference" => createReference(name, position, keys)

            case _ => UnknownField(name, command, position)
        }
    }

    def createReference(name: String, position: Int, keys: Map[String, String]) : Field = {
        ReferenceField(name,
                       position,
                       keys.get("base_key"),
                       keys.get("base_value").map(_.toInt))
    }
}

class Model(messages: Map[Id, Message]) {

    def dot(id: Id): String = {
        val insides = messages.get(id).map(_.dot).mkString("")
        s"""
           |digraph G {
           | ${insides}
           |}
          """.stripMargin

    }
}