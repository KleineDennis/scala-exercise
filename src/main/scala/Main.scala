import scala.io.Source

object Main extends App {
  if (args.length > 0) {
    val lines = Source.fromFile(args(0)).getLines().toList
    val f = (a: String, b: String) => if (a.length > b .length) a else b
    val longestline = lines.reduceLeft(f(_,_))
    val maxWidth =  widthOfLength(longestline)

    for (line <- lines) {
      val numSpace = maxWidth - widthOfLength(line)
      val padding = " " * numSpace
      println(padding + line.length + " | " + line)
    }

  } else
    Console.err.println("Please enter filename")

  def widthOfLength(s: String) = s.length.toString.length

}