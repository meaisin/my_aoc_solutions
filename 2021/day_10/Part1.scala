// Verified solution :) (both parts)

class Line(val string: String):
  override def toString: String =
    s"Line[$string]"
  
  private def findFirstIllegalChar(): Option[Char] =
    def inner(line: List[Char], stack: List[Char]): Option[Char] =
      line match
        case List() => None
        case _ => {
          if Line.isOpener(line.head) then
            inner(line.tail, line.head :: stack)
          else
            if Line.isMatch(stack.head, line.head) then
              inner(line.tail, stack.tail)
            else
              Some(line.head)
        }
    inner(string.toList, List())

  private def getRemainingOpeners(): List[Char] =
    def handle(char: Char, stack: List[Char]): List[Char] =
      if Line.isOpener(char) then (char :: stack)
      else stack.tail

    def inner(line: List[Char], stack: List[Char]): List[Char] = // Assuming no corrupted lines here!
      if line.isEmpty then stack
      else inner(line.tail, handle(line.head, stack))

    inner(string.toList, List())

  val firstIllegalChar: Option[Char] = findFirstIllegalChar()

  val isCorrupted: Boolean = firstIllegalChar match
    case Some(_) => true
    case None => false

  val isNotCorrupted = !isCorrupted

  val remainingOpeners: Option[List[Char]] = 
    if isCorrupted then None
    else Some(getRemainingOpeners())

end Line
      
object Line:
  def isOpener(char: Char): Boolean =
    char match
      case '(' | '[' | '{' | '<' => true
      case _ => false

  def isCloser(char: Char): Boolean =
    char match
      case ')' | ']' | '}' | '>' => true
      case _ => false

  def isMatch(opener: Char, closer: Char): Boolean =
    (opener, closer) match
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case _ => false

  def closerFor(opener: Char): Char =
    opener match
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case _ => throw new RuntimeException("Invalid opener.")

  def syntaxValue(char: Char): Int =
    char match
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137

  def autoCompleteValue(char: Char): BigInt =
    char match
      case ')' => BigInt(1)
      case ']' => BigInt(2)
      case '}' => BigInt(3)
      case '>' => BigInt(4)

  def computeCompletionScore(score: BigInt, char: Char): BigInt =
    (score * 5) + autoCompleteValue(char)

@main def main(filename: String): Unit =
  import scala.io.Source

  val lines = Source
    .fromFile(filename)
    .getLines
    .toList
    .map(Line(_))

  val part1Solution = lines
    .map(_.firstIllegalChar)
    .filter{x =>
      x match
        case None => false
        case _ => true
    }
    .map{x =>
      x match
        case Some(y) => y
        case _ => throw new RuntimeException("'None' found.")
    }
    .map(Line.syntaxValue(_))
    .sum

  val completionScores = lines
    .filter(_.isNotCorrupted)
    .map(_.remainingOpeners)
    .map{x => x match
      case Some(y) => y
      case _ => throw new RuntimeException("'None' found.")
    }
    .map(_.map(Line.closerFor(_)))
    .map(_.foldLeft(BigInt(0)){(acc, c) => Line.computeCompletionScore(acc, c)})
    .sorted

  val part2Solution = completionScores(completionScores.length / 2)


  println(part1Solution)
  println(part2Solution)
