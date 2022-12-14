class File(val name: String, val size: Int):
  override def toString: String = s"$name: $size bytes"
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

  def fileSize: Int =
    files.map(_.size).sum + children.map(_.fileSize).sum

  override def toString: String =
    s"$name: ${fileSize}"

object Helpers:
  def parseDir(lines: List[String]): Dir =
    val rootDir = Dir("/", None, List(), List())
    var currentDir = rootDir
    for
      line <- lines.tail
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
        case x => throw new RuntimeException(s"You fucked up the pattern matching. Here: $x")
    rootDir

  def getDirSizes(dir: Dir): List[Int] =
    def inner(list: List[Dir], output: List[Int]): List[Int] =
      if list.isEmpty then
        output
      else
        inner(list.map(_.children).flatten, list.map(_.fileSize) concat output)
    inner(List(dir), List())

@main def main(arg: String): Unit =
  import scala.io.Source

  val data = Source
    .fromFile(arg)
    .getLines
    .toList

  val rootDir = Helpers.parseDir(data)
  val sizes = Helpers.getDirSizes(rootDir)
  val part1Solution = sizes.filter(_ < 100000).sum
  println(part1Solution)

  val totalSpace = 70000000
  val updateSpace = 30000000

  val usedSpace = rootDir.fileSize
  val unusedSpace = totalSpace - usedSpace
  val neededSpace = updateSpace - unusedSpace

  val part2Solution = sizes.filter(_ >= neededSpace).min
  println(part2Solution)
