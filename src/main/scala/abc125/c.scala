import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Scanner {
  private val buf = new Array[Byte](1024); private var ptr = 0; private var len = 0
  @inline private def isPrintableChar(c: Int): Boolean = 33 <= c && c <= 126
  @inline private def hasNextByte(): Boolean =
    if (ptr >= len) { ptr = 0; len = System.in.read(buf); len > 0 } else { true }
  @inline private def hasNext(): Boolean = {
    while (hasNextByte() && !isPrintableChar(buf(ptr))) ptr += 1
    hasNextByte()
  }
  @inline private def readByte(): Byte =
    if (hasNextByte()) { val res = buf(ptr); ptr += 1; res } else { -1 }
  def next(): String = {
    if(!hasNext()) ???
    val sb = new StringBuilder; var b = readByte()
    while (isPrintableChar(b)) { sb.append(b.toChar); b = readByte() }
    sb.toString
  }
  def nextInt(): Int = {
    val n = nextLong()
    if (n < Int.MinValue || Int.MaxValue < n) ???
    n.toInt
  }
  def nextLong(): Long = {
    if(!hasNext()) ???
    var minus = false; var b = readByte()
    if (b == '-') { minus = true; b = readByte() }
    def go (b: Byte, n: Long = 0): Long =
      if ('0' <= b && b <= '9') { go(readByte(), n * 10 + b - '0') }
      else if (minus) { -n } else { n }
    go(b)
  }
  def nextDouble(): Double = next.toDouble
}

object Main extends App {

  @tailrec
  def gcd(a:Int,b:Int):Int = {
    if (b==0) {
      a
    } else {
      gcd (b,a%b)
    }
  }

  val n = Scanner.nextInt()
  val a = Array.fill(n)(Scanner.nextInt())

  var leftGcd = ArrayBuffer(a(0))
  var rightGcd = ArrayBuffer(a(n-1))

  (1 until (n-1)).foreach(i=>{
    leftGcd += gcd(a(i),leftGcd(i-1))
    rightGcd += gcd(a(n-i-1),rightGcd(i-1))
  })

  val result = ((0 until (n-2)).map(i=>{
    gcd(leftGcd(i),rightGcd(n-i-3))
  })++Seq(leftGcd(n-2),rightGcd(n-2))).max

  println(result)
}