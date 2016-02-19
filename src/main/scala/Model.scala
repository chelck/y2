
sealed trait Field {
    def dot(model: Model, parent: MessageId): String = ""
}

final case class PrimitiveField(parent: MessageId, name: String, command: String, position: Int) extends Field

final case class MiscField(parent: MessageId, name: String, command: String) extends Field
final case class UnknownField(parent: MessageId, name: String, command: String, position: Int) extends Field

final case class ReferenceField(parent: MessageId,
                                name: String,
                                position: Int,
                                defaultId: Option[MessageId],
                                versionKey: Option[String],
                                baseId: Option[MessageId]) extends Field {

    override def dot(model: Model, parent: MessageId) = {
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



case class Message(id: MessageId, name: String, fields: Seq[Field]) {

    def dot(model: Model): String = {
        dotNode + fields.map(_.dot(model, id)).mkString("\n")

    }

    def dotNode: String = s"""$id ${Dot.messageLabel(name, id, "blue")};"""
}




class Model(messages: Map[MessageId, Message]) {
    val versions = Map("30_message_version" -> List(1,2,3,4,5,6,7,8,9))

    def createVersionKey(parent: MessageId, fieldName: String) = s"${parent}_${fieldName}"

    def getVersions(parent: MessageId, versionKey: Option[String]): List[Int] = {
        versionKey match {
            case Some(key) => versions.getOrElse(createVersionKey(parent, key), List())
            case None => List()
        }
    }

    def foo(parent: MessageId, versionField: Option[String], baseValue: Option[MessageId]): List[MessageId] = {
        val v = getVersions(parent, versionField)

        (versionField, baseValue) match {
            case (Some(field), Some(base)) => base :: base.plus(1) :: v.map(base.plus(_))
            case (Some(field), None) => List()
            case (None, Some(base)) => base :: base.plus(1) :: v.map(base.plus(_))
            case (None, None) => List()

        }
    }

    def graph(id: MessageId): String = {
        s"""
           |digraph G {
           |${dot("root", id)}
           |}
          """.stripMargin

    }

    def dot(parent: String, child: MessageId): String = {
        s"$parent -> $child\n" +
        messages.get(child).map(_.dot(this)).mkString("")
    }

    def dotDefault(parent: String, child: MessageId): String = {
        s"""$parent -> $child [color="orange"];\n""" +
          messages.get(child).map(_.dot(this)).mkString("")
    }


}