/**
  * Created by chris on 2/12/16.
  */
object Main extends App {
    val model = Parser.parse("lfr_messages.xml")
    println(model.graph(Id(20177)))
}
