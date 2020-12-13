import sun.security.util.Length

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.StdIn


object RleEncodingApp extends App {

  println("Started")

  val rleEncoding = new RleEncoding
//  println(rleEncoding.encode(""))
//  println(rleEncoding.encode("X"))
//
//  println(rleEncoding.encode("ABCDE"))
//
//  println(rleEncoding.encode("AABBCCDD"))
//  println(rleEncoding.encode("XAABBCCDD"))
//
//  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB"))
//  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX" * 1_0000_000))

  StdIn.readLine()
}


trait Block {
  def length: Int
}

class EmptyBlock extends Block {
  override def length: Int = 0
}

class CompressedBlock(lengthCoBl: Int, dataCoBl: Char) extends EmptyBlock {
  override def length: Int = lengthCoBl

  def data: Char = dataCoBl

  override def toString: String = {
    "CompressedBlock(" + lengthCoBl + ", " + "'" + dataCoBl + "'" + ")"
  }
}

object CompressedBlock extends EmptyBlock {
  def apply(lengthCoBl: Int, dataCoBl: Char): CompressedBlock = new CompressedBlock(lengthCoBl, dataCoBl)

  def unapply(arg: CompressedBlock): Option[(Int, Char)] = {
    if (arg.length == 0) None else Some(arg.length, arg.data)
  }
}

class UncompressedBlock(lengthUnBl: Int, dataUnBl: ArrayBuffer[Char]) extends EmptyBlock {
  override def length: Int = lengthUnBl

  def dataBuffer: ArrayBuffer[Char] = dataUnBl

  override def toString: String = {
    "UncompressedBlock(" + lengthUnBl + ", " + dataUnBl + ")"
  }
}

object UncompressedBlock extends EmptyBlock {
  def apply(lengthUnBl: Int, dataUnBl: ArrayBuffer[Char]): UncompressedBlock = new UncompressedBlock(lengthUnBl, dataUnBl)

  def unapply(arg: UncompressedBlock): Option[(Int, ArrayBuffer[Char])] = {
    if (arg.length == 0) None else Some(arg.length, arg.dataBuffer)
  }
}

class RleEncoding {
  def encode(str: String): ListBuffer[Block] = {
    var newArray = ArrayBuffer[Char]();
    val (prev, block, result) = {
      str.toCharArray.foldLeft((None: Option[Char], None: Option[EmptyBlock], ListBuffer.empty[Block])) {
        /** Создаём пустой */
        case ((None, _, result), char) =>
          (Some(char), None, result)

        /** Создаём сжатый или не сжатый */
        case ((Some(prev), None, result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(1, char)), result)

        case ((Some(prev), None, result), char) if prev != char => newArray.clear();
          (Some(char), Some(UncompressedBlock(1, newArray += prev)), result)

        /** Обрабатываем не сжатый */

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
          (Some(char), Some(UncompressedBlock(block.length + 1, newArray += prev)), result)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(1, char)), result :+ block)

        /** Обрабатываем сжатый */
        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev != char =>
          (Some(char), None, result :+ CompressedBlock(block.length + 1, block.data))

      }
    }
    val tail = (block, prev) match {
      case (Some(block@UncompressedBlock(_, _)), Some(prev)) => result += (UncompressedBlock(newArray.length + 1, newArray.addOne(prev)))
      case (Some(block@CompressedBlock(_, data)), _) => result += (CompressedBlock(block.length + 1, data))
      case (None, Some(prev)) => result += (UncompressedBlock(1, ArrayBuffer(prev)))
      case (None, None) => ListBuffer.empty[Block]
    }
    result ++ tail

    result
  }
}

/**----------------------------------------------------------------------------*/

//trait Block {
//  def length: Int
//}
//
//class EmptyBlock extends Block {
//  override def length: Int = 0
//}
//
//class CompressedBlock(lengthCoBl: Int, dataCoBl: Char) extends EmptyBlock {
//  override def length: Int = lengthCoBl
//
//  def data: Char = dataCoBl
//
//  override def toString: String = {
//    "CompressedBlock(" + lengthCoBl + ", " + "'" + dataCoBl + "'" + ")"
//  }
//}
//
//object CompressedBlock extends EmptyBlock {
//  def apply(lengthCoBl: Int, dataCoBl: Char): CompressedBlock = new CompressedBlock(lengthCoBl, dataCoBl)
//
//  def unapply(arg: CompressedBlock): Option[(Int, Char)] = {
//    if (arg.length == 0) None else Some(arg.length, arg.data)
//  }
//}
//
//class UncompressedBlock(lengthUnBl: Int, dataUnBl: ArrayBuffer[Char]) extends EmptyBlock {
//  override def length: Int = lengthUnBl
//
//  def dataBuffer: ArrayBuffer[Char] = dataUnBl
//
//  override def toString: String = {
//    "UncompressedBlock(" + lengthUnBl + ", " + dataUnBl + ")"
//  }
//}
//
//object UncompressedBlock extends EmptyBlock {
//  def apply(lengthUnBl: Int, dataUnBl: ArrayBuffer[Char]): UncompressedBlock = new UncompressedBlock(lengthUnBl, dataUnBl)
//
//  def unapply(arg: UncompressedBlock): Option[(Int, ArrayBuffer[Char])] = {
//    if (arg.length == 0) None else Some(arg.length, arg.dataBuffer)
//  }
//}
//
//class RleEncoding {
//  def encode(str: String): ListBuffer[Block] = {
//    var newArray = ArrayBuffer[Char]();
//    val (prev, block, result) = {
//      str.toCharArray.foldLeft((None: Option[Char], None: Option[EmptyBlock], ListBuffer.empty[Block])) {
//        /** Создаём пустой */
//        case ((None, _, result), char) =>
//          (Some(char), None, result)
//
//        /** Создаём сжатый или не сжатый */
//        case ((Some(prev), None, result), char) if prev == char =>
//          (Some(char), Some(CompressedBlock(1, char)), result)
//
//        case ((Some(prev), None, result), char) if prev != char => newArray.clear();
//          (Some(char), Some(UncompressedBlock(1, newArray += prev)), result)
//
//        /** Обрабатываем не сжатый */
//
//        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
//          (Some(char), Some(UncompressedBlock(block.length + 1, newArray += prev)), result)
//
//        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev == char =>
//          (Some(char), Some(CompressedBlock(1, char)), result :+ block)
//
//        /** Обрабатываем сжатый */
//        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
//          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)
//
//        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev != char =>
//          (Some(char), None, result :+ CompressedBlock(block.length + 1, block.data))
//
//      }
//    }
//    val tail = (block, prev) match {
//      case (Some(block@UncompressedBlock(_, _)), Some(prev)) => result += (UncompressedBlock(newArray.length + 1, newArray.addOne(prev)))
//      case (Some(block@CompressedBlock(_, data)), _) => result += (CompressedBlock(block.length + 1, data))
//      case (None, Some(prev)) => result += (UncompressedBlock(1, ArrayBuffer(prev)))
//      case (None, None) => ListBuffer.empty[Block]
//    }
//    result ++ tail
//
//    result
//  }
//}

/** -------------------------- */

//trait Block {
//  def length: Int
//}
//case class UncompressedBlock(length: Int, data: Seq[Char]) extends Block
//
//case class CompressedBlock(length: Int, data: Char) extends Block
//
//class RleEncoding {
//
//  def encode(str: String): Seq[Block] = {
//    val (prev, block, result) = {
//      str.toCharArray.foldLeft((None: Option[Char], None: Option[Block], Seq.empty[Block])) {
//        case ((None, _, result), char) =>
//          (Some(char), None, result)
//
//        case ((Some(prev), None, result), char) if prev == char =>
//          (Some(char), Some(CompressedBlock(1, prev)), result)
//
//        case ((Some(prev), None, result), char) =>
//          (Some(char), Some(UncompressedBlock(1, Seq(prev))), result)
//
//        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
//          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)
//
//        case ((Some(_), Some(block@CompressedBlock(_, _)), result), char) =>
//          (Some(char), None, result :+ CompressedBlock(block.length + 1, block.data))
//
//        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
//          (Some(char), Some(UncompressedBlock(block.length + 1, block.data :+ prev)), result)
//
//        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) =>
//          (Some(char), Some(CompressedBlock(1, prev)), result :+ block)
//      }
//    }
//
//
//    val tail = (block, prev) match {
//      case (Some(block@UncompressedBlock(_, _)), Some(prev)) => Seq(UncompressedBlock(block.length + 1, block.data :+ prev))
//      case (Some(block@CompressedBlock(_, data)), _) => Seq(CompressedBlock(block.length + 1, data))
//      case (None, Some(prev)) => Seq(UncompressedBlock(1, Seq(prev)))
//      case (None, None) => Seq.empty[Block]
//    }
//
//    result ++ tail
//  }
//}
