
object Id {
    def apply(s: String): Id = Id(s.toInt)

    //def apply(sid: Option[String]): Option[Id] = sid.map((s: String) => Id(s.toInt))
}

case class Id(n: Int) {
    def plus(n: Int): Id = Id(this.n + n)
    override def toString = n.toString
}


