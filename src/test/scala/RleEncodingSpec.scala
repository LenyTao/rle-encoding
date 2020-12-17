

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


class RleEncodingSpec extends AnyFlatSpec with should.Matchers {

  "RleEncoding" should "encode given string1" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("ABCDE").toString() should be(ListBuffer(UncompressedBlock(5, List('A', 'B', 'C', 'D', 'E'))).toString())
  }

  it should "encode given string2" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AABBCCDD").toString() should be(ListBuffer(CompressedBlock(2, 'A'), CompressedBlock(2, 'B'), CompressedBlock(2, 'C'), CompressedBlock(2, 'D')).toString())
  }

  it should "encode given string3" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("XAABBCCDD").toString() should be(ListBuffer(UncompressedBlock(1, List('X')), CompressedBlock(2, 'A'), CompressedBlock(2, 'B'), CompressedBlock(2, 'C'), CompressedBlock(2, 'D')).toString())
  }
  it should "encode given string4" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB").toString() should be(ListBuffer(CompressedBlock(4, 'A'), CompressedBlock(3, 'B'), CompressedBlock(2, 'C'), UncompressedBlock(3, List('X', 'Y', 'Z')), CompressedBlock(4, 'D'), CompressedBlock(3, 'E'), CompressedBlock(3, 'F'), CompressedBlock(6, 'A'), CompressedBlock(29, 'B')).toString())
  }
  it should "encode given string5" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX").toString() should be(ListBuffer(CompressedBlock(4, 'A'), CompressedBlock(3, 'B'), CompressedBlock(2, 'C'), UncompressedBlock(3, List('X', 'Y', 'Z')), CompressedBlock(4, 'D'), CompressedBlock(3, 'E'), CompressedBlock(3, 'F'), CompressedBlock(6, 'A'), CompressedBlock(29, 'B'), UncompressedBlock(1, List('X'))).toString())
  }

  it should "encode given string6" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("X").toString() should be(ListBuffer(UncompressedBlock(1, List('X'))).toString())
  }
  it should "encode given string7" in {
    val rleEncoding = new RleEncoding

    rleEncoding.encode("") should be(ListBuffer())
  }
}