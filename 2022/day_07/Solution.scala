class File(val name: String, val size: Int):
  val isDir: Boolean = false

class Dir(name: String,
  val parentDir: Option[Dir],
  var files: List[File],
  var children: List[Dir]) extends File(name, 0):

  override val isDir: Boolean = true

  def addFile(file: File): Unit =
    files = file :: files

  def addChild(child: Dir): Unit =
    children = child :: children

  override val size: Int =
    files.map{x =>
      x match
        case d: Dir => d.files.map(_.size).sum
        case f: File => f.size
    }.sum

@main def main(arg: String): Dir =
  import scala.io.Source
  import scala.util.matching.Regex

  val data = Source
    .fromFile(arg)
    .getLines
    .toList

  val rootDir = Dir("/", None, List(), List())
  var currentDir = rootDir

  for
    line <- data.tail
  do
    line.split(" ").toList: List[String] match
      case List("$", "ls") => ()
      case List("$", "cd", "..") => currentDir = currentDir.parentDir match
        case Some(p) => p
        case None => currentDir
      case List("$", "cd", child) => currentDir = currentDir.children.find(_.name == child) match
        case Some(childDir) => childDir
        case None => throw new RuntimeException(s"Child not found: $child")
      case List("dir", name) => currentDir.addChild(Dir(name, Some(currentDir), List(), List()))
      case List(size, name) => currentDir.addFile(File(name, Integer.parseInt(size)))
      case x => throw new RuntimeException(s"You fucked up the pattern match. Here: $x")

  rootDir
