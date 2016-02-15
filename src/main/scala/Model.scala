
sealed trait Field {
    def dot(model: Model, parent: Id): String = ""
}

final case class PrimitiveField(parent: Id, name: String, command: String, position: Int) extends Field

final case class MiscField(parent: Id, name: String, command: String) extends Field
final case class UnknownField(parent: Id, name: String, command: String, position: Int) extends Field

final case class ReferenceField(parent: Id,
                                name: String,
                                position: Int,
                                defaultId: Option[Id],
                                versionKey: Option[String],
                                baseId: Option[Id]) extends Field {

    override def dot(model: Model, parent: Id) = {
        val fieldKey = Dot.createFieldDotId(parent, position)
        val children = model.foo(parent, versionKey, baseId)

        s"""
           |$fieldKey ${Dot.fieldLabel(name, "blue")};
           |$parent -> $fieldKey
           |${defaultId.map(model.dotDefault(fieldKey.name, _)).mkString("\n")}
           |${children.map(model.dot(fieldKey.name, _)).mkString("\n")}
           |""".stripMargin
    }
}



case class Message(name: String, id: Id, fields: Seq[Field]) {

    def dot(model: Model): String = {
        dotNode + fields.map(_.dot(model, id)).mkString("\n")

    }

    def dotNode: String = s"""$id ${Dot.messageLabel(name, id, "blue")};"""
}




class Model(messages: Map[Id, Message]) {
    val versions = Map("30_message_version" -> List(1,2,3,4,5,6,7,8,9))

    def createVersionKey(parent: Id, fieldName: String) = s"${parent}_${fieldName}"

    def getVersions(parent: Id, versionKey: Option[String]): List[Int] = {
        versionKey match {
            case Some(key) => versions.getOrElse(createVersionKey(parent, key), List())
            case None => List()
        }
    }

    def foo(parent: Id, versionField: Option[String], baseValue: Option[Id]): List[Id] = {
        val v = getVersions(parent, versionField)

        (versionField, baseValue) match {
            case (Some(field), Some(base)) => base :: base.plus(1) :: v.map(base.plus(_))
            case (Some(field), None) => List()
            case (None, Some(base)) => base :: base.plus(1) :: v.map(base.plus(_))
            case (None, None) => List()

        }
    }

    def graph(id: Id): String = {
        val insides = messages.get(id).map(_.dot(this)).mkString("")
        s"""
           |digraph G {
           |${dot("root", id)}
           |}
          """.stripMargin

    }

    def dot(parent: String, id: Id): String = {
        s"$parent -> $id\n" +
        messages.get(id).map(_.dot(this)).mkString("")
    }

    def dotDefault(parent: String, id: Id): String = {
        s"""$parent -> $id [color="orange"];\n""" +
          messages.get(id).map(_.dot(this)).mkString("")
    }


}