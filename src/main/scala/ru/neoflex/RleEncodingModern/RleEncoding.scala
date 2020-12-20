package ru.neoflex.RleEncodingModern

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.StdIn


object RleEncodingApp extends App {

  println("Started")

  val rleEncoding = new RleEncoding
  println(rleEncoding.encode(""))
  println(rleEncoding.encode("X"))
  println(rleEncoding.encode("ABCDE"))
  println(rleEncoding.encode("AABBCCDD"))
  println(rleEncoding.encode("XAABBCCDD"))
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB"))
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX" * 100000))

  StdIn.readLine()
}

/**Финальный алгоритм */

trait Block {
  def length: Int

  def data: Char

  def dataBuffer: List[Char]
}

class EmptyBlock extends Block {
  var firstData = ' '

  def this(char: Char) {
    this()
    firstData = char
  }

  override def length: Int = 0

  override def data: Char = firstData

  def dataBuffer: List[Char] = List.empty

  def newChar(char: Char) = new EmptyBlock(char)
}

class CompressedBlock(lengthCoBl: Int, dataCoBl: Char) extends EmptyBlock {
  override def length: Int = lengthCoBl

  override def data: Char = dataCoBl

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

class UncompressedBlock(lengthUnBl: Int, dataUnBl: List[Char]) extends EmptyBlock {
  override def length: Int = lengthUnBl

  override def dataBuffer: List[Char] = dataUnBl

  override def toString: String = {
    "UncompressedBlock(" + lengthUnBl + ", " + dataUnBl + ")"
  }
}

object UncompressedBlock extends EmptyBlock {
  def apply(lengthUnBl: Int, dataUnBl: List[Char]): UncompressedBlock = new UncompressedBlock(lengthUnBl, dataUnBl)

  def unapply(arg: UncompressedBlock): Option[(Int, List[Char])] = {
    if (arg.length == 0) None else Some(arg.length, arg.dataBuffer)
  }
}

class BlockBuilder(var collBlock: ArrayBuffer[Block]) {
  def build() = {
    new UncompressedBlock(collBlock.head.length, collBlock.head.dataBuffer)
  }

  def removeCollSymb() = {
    collBlock.clear()
  }
}

object BlockBuilder {

  def dataBuffer = List.empty[Char]

  def apply(symbolsBuild: ArrayBuffer[Block]) = new BlockBuilder(symbolsBuild)

  def unapply(arg: BlockBuilder): Option[(Block, ArrayBuffer[Block])] = {
    if (arg.collBlock.isEmpty) None else Some(arg.collBlock.head, arg.collBlock)
  }

}

class RleEncoding {
  def encode(str: String): ListBuffer[Block] = {
    var emptyBlock = new EmptyBlock
    val emptyComBlo = CompressedBlock(0, ' ')
    val emptyUncoBlo = UncompressedBlock(0, List(' '))

    val (block, result) = {
      str.toCharArray.foldLeft((BlockBuilder(ArrayBuffer.empty), ListBuffer.empty[Block])) {

        /** Создаём пустой */
        case ((block, result), char) if emptyBlock.data == ' ' => emptyBlock = emptyBlock.newChar(char); (block, result)

        /** Создаём сжатый или не сжатый */
        case ((block, result), char) if emptyBlock.data == char && block.collBlock.isEmpty =>
          block.collBlock.addOne(CompressedBlock(2, char))
          emptyBlock = emptyBlock.newChar(char);
          (block, result)

        case ((block, result), char) if emptyBlock.data != char && block.collBlock.isEmpty =>
          block.collBlock.addOne(UncompressedBlock(1, BlockBuilder.dataBuffer :+ emptyBlock.data))
          emptyBlock = emptyBlock.newChar(char);
          (block, result)

        /** Обрабатываем не сжатый */
        case ((block, result), char) if emptyBlock.data != char && block.collBlock.head.getClass == emptyUncoBlo.getClass =>

          block.collBlock.addOne(UncompressedBlock(block.collBlock.head.length + 1, block.collBlock.head.dataBuffer :+ emptyBlock.data)).remove(0);
          emptyBlock = emptyBlock.newChar(char);

          (block, result)

        case ((block, result), char) if emptyBlock.data == char && block.collBlock.head.getClass == emptyUncoBlo.getClass =>
          result.addOne(block.build())
          block.removeCollSymb()
          block.collBlock.addOne(CompressedBlock(2, char));
          emptyBlock = emptyBlock.newChar(char);
          (block, result)

        /** Обрабатываем сжатый */

        case ((block, result), char) if emptyBlock.data == char && block.collBlock.head.getClass == emptyComBlo.getClass =>
          block.collBlock.addOne(CompressedBlock(block.collBlock.head.length + 1, emptyBlock.data)).remove(0);
          emptyBlock = emptyBlock.newChar(char);
          (block, result)

        case ((block, result), char) if emptyBlock.data != char && block.collBlock.head.getClass == emptyComBlo.getClass =>
          result += (block.collBlock.head)
          block.removeCollSymb()
          emptyBlock = emptyBlock.newChar(char);
          (block, result)
      }
    }

    if (str.nonEmpty) {
      emptyBlock = emptyBlock.newChar(str(str.length - 1))
    }

    val tail = (emptyBlock.data) match {
      case (lastChar) if lastChar.toString == " " => ListBuffer.empty[Block]
      case (lastChar) if block.collBlock.isEmpty => result += (UncompressedBlock(1, List(lastChar)))
      case (lastChar) if block.collBlock.head.getClass == emptyUncoBlo.getClass => result += UncompressedBlock(block.collBlock.head.length + 1, block.collBlock.head.dataBuffer :+ lastChar)
      case (lastChar) if block.collBlock.head.getClass == emptyComBlo.getClass => result += (CompressedBlock(block.collBlock.head.length, lastChar))
    }

    result ++ tail
    result
  }
}


/** Изменённый алгоритм с тремя параметрами */
//trait ru.neoflex.RleEncodingModern.Block {
//  def length: Int
//
//  def data: Char
//
//  def dataBuffer: List[Char]
//}
//
//class ru.neoflex.RleEncodingModern.EmptyBlock() extends ru.neoflex.RleEncodingModern.Block {
//  override def length: Int = 0
//
//  def data: Char = ' '
//
//  def dataBuffer: List[Char] = List.empty
//}
//
//object ru.neoflex.RleEncodingModern.EmptyBlock {
//
//}
//
//class ru.neoflex.RleEncodingModern.CompressedBlock(lengthCoBl: Int, dataCoBl: Char) extends ru.neoflex.RleEncodingModern.EmptyBlock {
//  override def length: Int = lengthCoBl
//
//  override def data: Char = dataCoBl
//
//  override def toString: String = {
//    "ru.neoflex.RleEncodingModern.CompressedBlock(" + lengthCoBl + ", " + "'" + dataCoBl + "'" + ")"
//  }
//}
//
//object ru.neoflex.RleEncodingModern.CompressedBlock extends ru.neoflex.RleEncodingModern.EmptyBlock {
//  def apply(lengthCoBl: Int, dataCoBl: Char): ru.neoflex.RleEncodingModern.CompressedBlock = new ru.neoflex.RleEncodingModern.CompressedBlock(lengthCoBl, dataCoBl)
//
//  def unapply(arg: ru.neoflex.RleEncodingModern.CompressedBlock): Option[(Int, Char)] = {
//    if (arg.length == 0) None else Some(arg.length, arg.data)
//  }
//}
//
//class ru.neoflex.RleEncodingModern.UncompressedBlock(lengthUnBl: Int, dataUnBl: List[Char]) extends ru.neoflex.RleEncodingModern.EmptyBlock {
//  override def length: Int = lengthUnBl
//
//  override def dataBuffer: List[Char] = dataUnBl
//
//  override def toString: String = {
//    "ru.neoflex.RleEncodingModern.UncompressedBlock(" + lengthUnBl + ", " + dataUnBl + ")"
//  }
//}
//
//object ru.neoflex.RleEncodingModern.UncompressedBlock extends ru.neoflex.RleEncodingModern.EmptyBlock {
//  def apply(lengthUnBl: Int, dataUnBl: List[Char]): ru.neoflex.RleEncodingModern.UncompressedBlock = new ru.neoflex.RleEncodingModern.UncompressedBlock(lengthUnBl, dataUnBl)
//
//  def unapply(arg: ru.neoflex.RleEncodingModern.UncompressedBlock): Option[(Int, List[Char])] = {
//    if (arg.length == 0) None else Some(arg.length, arg.dataBuffer)
//  }
//}
//
//class ru.neoflex.RleEncodingModern.BlockBuilder(var collSymb: ArrayBuffer[Char]) {
//  def build() = {
//    new ru.neoflex.RleEncodingModern.UncompressedBlock(collSymb.length, collSymb.toList)
//  }
//}
//
//object ru.neoflex.RleEncodingModern.BlockBuilder {
//
//  def dataBuffer = List.empty[Char]
//
//  def apply(symbolsBuild: ArrayBuffer[Char]) = new ru.neoflex.RleEncodingModern.BlockBuilder(symbolsBuild)
//
//  def unapply(arg: ru.neoflex.RleEncodingModern.BlockBuilder): Option[(Char, ArrayBuffer[Char])] = {
//    if (arg.collSymb.isEmpty) None else Some(arg.collSymb.head, arg.collSymb)
//  }
//
//}
//
//class ru.neoflex.RleEncodingModern.RleEncoding {
//  def encode(str: String): ListBuffer[ru.neoflex.RleEncodingModern.Block] = {
//    val emptyComBlo = ru.neoflex.RleEncodingModern.CompressedBlock(0, ' ')
//    val emptyUncoBlo = ru.neoflex.RleEncodingModern.UncompressedBlock(0, List(' '))
//
//    val helperArray = ArrayBuffer[ru.neoflex.RleEncodingModern.Block]()
//
//    val (prev, block, result) = {
//      str.toCharArray.foldLeft((None: Option[Char], ru.neoflex.RleEncodingModern.BlockBuilder(ArrayBuffer.empty), ListBuffer.empty[ru.neoflex.RleEncodingModern.Block])) {
//
//        /** Создаём пустой */
//        case ((None, block, result), char) =>
//          (Some(char), block, result)
//
//        /** Создаём сжатый или не сжатый */
//        case ((Some(prev), block, result), char) if prev == char && helperArray.isEmpty =>
//          helperArray.addOne(ru.neoflex.RleEncodingModern.CompressedBlock(2, char)).head
//          (Some(char), block, result)
//
//        case ((Some(prev), block, result), char) if prev != char && helperArray.isEmpty =>
//          helperArray.addOne(ru.neoflex.RleEncodingModern.UncompressedBlock(1, ru.neoflex.RleEncodingModern.BlockBuilder.dataBuffer :+ prev))
//          (Some(char), block, result)
//
//        /** Обрабатываем не сжатый */
//        case ((Some(prev), block, result), char) if prev != char && helperArray.head.getClass == emptyUncoBlo.getClass =>
//          helperArray.addOne(ru.neoflex.RleEncodingModern.UncompressedBlock(helperArray.head.length + 1, helperArray.head.dataBuffer :+ prev)).remove(0);
//          (Some(char), block, result)
//
//        case ((Some(prev), block, result), char) if prev == char && helperArray.head.getClass == emptyUncoBlo.getClass =>
//          block.collSymb.addAll(helperArray.head.dataBuffer)
//          result.addOne(block.build())
//          helperArray.clear();
//          helperArray.addOne(ru.neoflex.RleEncodingModern.CompressedBlock(2, char));
//          (Some(char), block, result)
//
//        /** Обрабатываем сжатый */
//
//        case ((Some(prev), block, result), char) if prev == char && helperArray.head.getClass == emptyComBlo.getClass =>
//          helperArray.addOne(ru.neoflex.RleEncodingModern.CompressedBlock(helperArray.head.length + 1, prev)).remove(0);
//          (Some(char), block, result)
//
//        case ((Some(prev), block, result), char) if prev != char && helperArray.head.getClass == emptyComBlo.getClass =>
//          result += (helperArray.head)
//          block.collSymb.clear()
//          helperArray.clear()
//          (Some(char), block, result)
//
//      }
//    }
//    val tail = (block, prev) match {
//      case (_, Some(prev)) if helperArray.isEmpty => result += (ru.neoflex.RleEncodingModern.UncompressedBlock(1, List(prev)))
//      case (_, Some(prev)) if helperArray.head.getClass == emptyUncoBlo.getClass => result += ru.neoflex.RleEncodingModern.UncompressedBlock(helperArray.head.length + 1, helperArray.head.dataBuffer :+ prev)
//      case (_, Some(prev)) if helperArray.head.getClass == emptyComBlo.getClass => result += (ru.neoflex.RleEncodingModern.CompressedBlock(helperArray.head.length, prev))
//      case (_, None) => ListBuffer.empty[ru.neoflex.RleEncodingModern.Block]
//    }
//    result ++ tail
//
//    result
//  }
//}

/** Пример алгоритма из лекции */

//trait ru.neoflex.RleEncodingModern.Block {
//  def length: Int
//}
//case class ru.neoflex.RleEncodingModern.UncompressedBlock(length: Int, data: Seq[Char]) extends ru.neoflex.RleEncodingModern.Block
//
//case class ru.neoflex.RleEncodingModern.CompressedBlock(length: Int, data: Char) extends ru.neoflex.RleEncodingModern.Block
//
//class ru.neoflex.RleEncodingModern.RleEncoding {
//
//  def encode(str: String): Seq[ru.neoflex.RleEncodingModern.Block] = {
//    val (prev, block, result) = {
//      str.toCharArray.foldLeft((None: Option[Char], None: Option[ru.neoflex.RleEncodingModern.Block], Seq.empty[ru.neoflex.RleEncodingModern.Block])) {
//        case ((None, _, result), char) =>
//          (Some(char), None, result)
//
//        case ((Some(prev), None, result), char) if prev == char =>
//          (Some(char), Some(ru.neoflex.RleEncodingModern.CompressedBlock(1, prev)), result)
//
//        case ((Some(prev), None, result), char) =>
//          (Some(char), Some(ru.neoflex.RleEncodingModern.UncompressedBlock(1, Seq(prev))), result)
//
//        case ((Some(prev), Some(block@ru.neoflex.RleEncodingModern.CompressedBlock(_, _)), result), char) if prev == char =>
//          (Some(char), Some(ru.neoflex.RleEncodingModern.CompressedBlock(block.length + 1, block.data)), result)
//
//        case ((Some(_), Some(block@ru.neoflex.RleEncodingModern.CompressedBlock(_, _)), result), char) =>
//          (Some(char), None, result :+ ru.neoflex.RleEncodingModern.CompressedBlock(block.length + 1, block.data))
//
//        case ((Some(prev), Some(block@ru.neoflex.RleEncodingModern.UncompressedBlock(_, _)), result), char) if prev != char =>
//          (Some(char), Some(ru.neoflex.RleEncodingModern.UncompressedBlock(block.length + 1, block.data :+ prev)), result)
//
//        case ((Some(prev), Some(block@ru.neoflex.RleEncodingModern.UncompressedBlock(_, _)), result), char) =>
//          (Some(char), Some(ru.neoflex.RleEncodingModern.CompressedBlock(1, prev)), result :+ block)
//      }
//    }
//
//
//    val tail = (block, prev) match {
//      case (Some(block@ru.neoflex.RleEncodingModern.UncompressedBlock(_, _)), Some(prev)) => Seq(ru.neoflex.RleEncodingModern.UncompressedBlock(block.length + 1, block.data :+ prev))
//      case (Some(block@ru.neoflex.RleEncodingModern.CompressedBlock(_, data)), _) => Seq(ru.neoflex.RleEncodingModern.CompressedBlock(block.length + 1, data))
//      case (None, Some(prev)) => Seq(ru.neoflex.RleEncodingModern.UncompressedBlock(1, Seq(prev)))
//      case (None, None) => Seq.empty[ru.neoflex.RleEncodingModern.Block]
//    }
//
//    result ++ tail
//  }
//}
