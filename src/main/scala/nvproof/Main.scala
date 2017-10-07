package nvproof

object Main {
  def main(argv: Array[String]): Unit = {
    print("> ")
    val fileName = readLine()
    val fileContents = readFile(fileName)

    val proof = Parsers.parse(Parsers.proof, fileContents)

    proof match {
      case Parsers.Success(matched,_) => for (ln <- matched) yield println(ln)
      case Parsers.Failure(msg,_) => println("FAILURE: " + msg)
      case Parsers.Error(msg,_) => println("ERROR: " + msg)
    }
  }

  def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    try source.mkString finally source.close()
  }
}
