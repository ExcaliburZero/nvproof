package nvproof

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object Main {
  private val QUIT: String = ":q"
  private val SAVE: String = ":s"

  def main(argv: Array[String]): Unit = {
    argv.toList match {
      case Nil =>
        println("Subcommand needed")
        System.exit(1)
      case subcommand :: xs =>
        subcommand match {
          case "interpreter" =>
            interpreter(xs.toArray, Nil)
          case "compiler" =>
            compiler(xs.toArray)
          case _ =>
            println("Invalid subcommand: " + subcommand)
            System.exit(1)
        }
    }
  }

  def compiler(argv: Array[String]): Unit = {
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

  @tailrec
  def interpreter(argv: Array[String], prevSteps: List[Step]): Unit = {
    val parallel = false

    val promptString = (prevSteps.length + 1) + ") "
    print(promptString)
    val newStepStringRaw = readLine()
    val newStepString = if (newStepStringRaw.startsWith(promptString) || newStepStringRaw == QUIT || newStepStringRaw.startsWith(SAVE)) {
      newStepStringRaw
    } else {
      promptString + newStepStringRaw
    }

    if (newStepString == "") {
        interpreter(argv, prevSteps)
    } else if (newStepString == QUIT) {
        return
    } else if (newStepString.startsWith(SAVE)) {
      val fileName = newStepString.substring(SAVE.size + 1)
      writeFile(fileName, AST.sourcePrint(prevSteps))
      println(f"Saved proof to $fileName")
      interpreter(argv, prevSteps)
    } else {
      val newStep = Parsers.parse(Parsers.step, newStepString)

      newStep match {
        case Parsers.Success(matched,_) =>
          val proof = prevSteps ++ List(matched)
          Validation.validate(proof, parallel) match {
            case None =>
              interpreter(argv, proof)
            case Some(es) =>
              for (e <- es) yield println(e.msg)
              interpreter(argv, prevSteps)
          }
        case Parsers.Failure(msg,_) =>
          println("FAILURE: " + msg)
          interpreter(argv, prevSteps)
        case Parsers.Error(msg,_) =>
          println("ERROR: " + msg)
          interpreter(argv, prevSteps)
      }
    }
  }

  def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    try source.mkString finally source.close()
  }

  def writeFile(fileName: String, contents: String): Unit = {
    Files.write(Paths.get(fileName), contents.getBytes(StandardCharsets.UTF_8))
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
