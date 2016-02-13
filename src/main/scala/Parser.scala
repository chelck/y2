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
        println(filename)

        val foo = scala.xml.XML.loadFile(filename)

        val s = (foo \ "message").map(_.label)

        for (m <- foo \ "message") {
            val fields = (m \ "field").map((n: Node) => Model.createField((n \ "@name").text, parseEntries(n)))

           for (f <- fields) {
               f match {
                   case UnknownField(name, command) => print(name, command)
                   case ReferenceField(name) => print(name)

                   case _ =>
               }


           }
        }
    }

}
