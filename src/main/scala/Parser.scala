import scala.xml.Node


object Parser {

    def parseEntries(field: Node) = {
        var entries = List[(String, String)]()
        for (e <- field \ "entry") {
            entries = ((e \ "@key").text, e.text) :: entries
        }

        entries.toMap
    }

    def parse(filename: String) = {
        val xml = scala.xml.XML.loadFile(filename)

        var messages = List[(Id, Message)]()

        for (m <- xml \ "message") {
            val name = (m \ "@name").text
            val id = Id((m \ "@type").text.toInt)
            val fields = (m \ "field").map((n: Node) => Model.createField((n \ "@name").text, parseEntries(n)))

            messages = (id, Message(name, id, fields)) :: messages
        }
        new Model(messages.toMap)
    }
}


