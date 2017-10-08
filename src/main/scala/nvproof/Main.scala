package nvproof

object Main {
  def main(argv: Array[String]): Unit = {
    val parallel = false
    val quiet = false

    val fileName = argv(0)
    val fileContents = readFile(fileName)

    val proof = Parsers.parse(Parsers.proof, fileContents)

    proof match {
      case Parsers.Success(matched,_) => validateProof(matched, parallel, quiet)
      case Parsers.Failure(msg,_) => println("FAILURE: " + msg)
      case Parsers.Error(msg,_) => println("ERROR: " + msg)
    }
  }

  def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    try source.mkString finally source.close()
  }

  def validateProof(proof: AST.Proof, parallel: Boolean, quiet: Boolean): Unit = {
    if (!quiet) {
      for (ln <- proof) yield println(ln)
      println("-------")
    }
    
    Validation.validate(proof, parallel) match {
      case None =>
        Console.out.println(Console.GREEN + "Valid ✓" + Console.RESET)
      case Some(errors) =>
        Console.out.println(Console.RED + "Invalid ❌" + Console.RESET)
        for (e <- errors) yield println(e.msg)
        System.exit(1)
    }
  }
}
