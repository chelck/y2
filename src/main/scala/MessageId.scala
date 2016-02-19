
object MessageId {
    def apply(s: String): MessageId = MessageId(s.toInt)

    //def apply(sid: Option[String]): Option[Id] = sid.map((s: String) => Id(s.toInt))
}

case class MessageId(n: Int) {
    def plus(n: Int): MessageId = MessageId(this.n + n)
    override def toString = n.toString
}


