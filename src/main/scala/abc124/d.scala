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
  val n = Scanner.nextInt
  val k = Scanner.nextInt
  val s = Scanner.next
  val sArray = s.toCharArray

  var turningPoints = new ArrayBuffer[Int]

  sArray.zipWithIndex.foreach(a=>{
    if (a._2 == 0) {
      turningPoints += a._2
    } else {
      if (a._1 != sArray(a._2-1)){
        turningPoints += a._2
      }
    }
  })

  val result = turningPoints.zipWithIndex.map(t=>{
    if (sArray(t._1) == '0') {
      if ((t._2+2*k)>=turningPoints.length){
        n-t._1
      } else {
        turningPoints(t._2+2*k)-t._1
      }
    } else{
      if ((t._2+2*k+1)>=turningPoints.length){
        n-t._1
      } else {
        turningPoints(t._2+2*k+1)-t._1
      }
    }
  }).max

  println(result)
}
