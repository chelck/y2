import scala.xml.Node


object Parser {

    def parse(filename: String) = {
        val xml = scala.xml.XML.loadFile(filename)

        var messages = List[(Id, Message)]()

        for (m <- xml \ "message") {
            val name = (m \ "@name").text
            val id = Id((m \ "@type").text.toInt)
            val fields = (m \ "field").map((n: Node) => parseField(id, (n \ "@name").text, parseKeys(n)))

            messages = (id, Message(name, id, fields)) :: messages
        }
        new Model(messages.toMap)
    }


    def parseKeys(field: Node): Map[String, String] = {
        var entries = List[(String, String)]()
        for (e <- field \ "entry") {
            entries = ((e \ "@key").text, e.text) :: entries
        }

        entries.filter(!_._2.isEmpty).toMap
    }


    def parseField(parent: Id, name: String, keys: Map[String, String]): Field = {
        val command = keys("read_command")
        val position = keys("position").toInt

        command match {
            case "addCurrentTimestamp" => MiscField(parent, name, command)
            case "addCurrentTimestampSec" => MiscField(parent, name, command)
            case "addInteger" => MiscField(parent, name, command)
            case "readBinfile" => MiscField(parent, name, command)

            case "readByte" => PrimitiveField(parent, name, command, position)
            case "readInt" => PrimitiveField(parent, name, command, position)
            case "readShort" => PrimitiveField(parent, name, command, position)
            case "readUnsignedByte" => PrimitiveField(parent, name, command, position)
            case "readUnsignedShort" => PrimitiveField(parent, name, command, position)
            case "readStringArray" => PrimitiveField(parent, name, command, position)
            case "readLong" => PrimitiveField(parent, name, command, position)
            case "readQfloat" => PrimitiveField(parent, name, command, position)
            case "readByteArray" => PrimitiveField(parent, name, command, position)

            case "reference" => parseReference(parent, name, position, keys)

            case _ => UnknownField(parent, name, command, position)
        }
    }

    def parseReference(parent: Id, name: String, position: Int, keys: Map[String, String]): Field = {
        ReferenceField(parent,
                       name,
                       position,
                       keys.get("default_message_type").map(Id(_)),
                       keys.get("base_key").filter(!_.isEmpty),
                       keys.get("base_value").map(Id(_)))
    }

}


