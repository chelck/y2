import scala.xml.Elem
import scala.xml.Node

/**
  * Created by chris on 2/12/16.
  */

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

        var messages = List[Message]()

        for (m <- xml \ "message") {
            val name = (m \ "@name").text
            val id = Id((m \ "@type").text.toInt)
            val fields = (m \ "field").map((n: Node) => Model.createField((n \ "@name").text, parseEntries(n)))

            messages = Message(name, id, fields) :: messages
        }
        new Model(messages)
    }
}


