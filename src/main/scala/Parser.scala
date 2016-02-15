import scala.xml.Node


object Parser {

    def parseKeys(field: Node): Map[String, String] = {
        var entries = List[(String, String)]()
        for (e <- field \ "entry") {
            entries = ((e \ "@key").text, e.text) :: entries
        }

        entries.filter(!_._2.isEmpty).toMap
    }

    def parse(filename: String) = {
        val xml = scala.xml.XML.loadFile(filename)

        var messages = List[(Id, Message)]()

        for (m <- xml \ "message") {
            val name = (m \ "@name").text
            val id = Id((m \ "@type").text.toInt)
            val fields = (m \ "field").map((n: Node) => parseField((n \ "@name").text, parseKeys(n)))

            messages = (id, Message(name, id, fields)) :: messages
        }
        new Model(messages.toMap)
    }

    def parseField(name: String, keys: Map[String, String]): Field = {
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

            case "reference" => parseReference(name, position, keys)

            case _ => UnknownField(name, command, position)
        }
    }

    def parseReference(name: String, position: Int, keys: Map[String, String]): Field = {
        ReferenceField(name,
                       position,
                       keys.get("default_message_type").map(Id(_)),
                       keys.get("base_key").filter(!_.isEmpty),
                       keys.get("base_value").map(Id(_)))
    }

}


