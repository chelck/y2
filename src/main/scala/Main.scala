/**
  * Created by chris on 2/12/16.
  */
object Main extends App {
    print("hello\n")

    val model = Parser.parse("lfr_messages.xml")
    println(model.dot(Id(20177)))
}
