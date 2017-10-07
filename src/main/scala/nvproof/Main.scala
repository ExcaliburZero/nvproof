package nvproof

object Main {
  def main(argv: Array[String]): Unit = {
    val fileName = argv(0)
    val fileContents = readFile(fileName)

    val proof = Parsers.parse(Parsers.proof, fileContents)

    proof match {
      case Parsers.Success(matched,_) => validateProof(matched)
      case Parsers.Failure(msg,_) => println("FAILURE: " + msg)
      case Parsers.Error(msg,_) => println("ERROR: " + msg)
    }
  }

  def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    try source.mkString finally source.close()
  }

  def validateProof(proof: AST.Proof): Unit = {
    for (ln <- proof) yield println(ln)
    println("-------")
    
    Validation.validate(proof) match {
      case None =>
        Console.out.println(Console.GREEN + "Valid ✓" + Console.RESET)
      case Some(errors) =>
        Console.out.println(Console.RED + "Invalid ❌" + Console.RESET)
        for (e <- errors) yield println(e.msg)
    }
  }
}
