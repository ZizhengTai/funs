package funs.collection.immutable

import scala.annotation.tailrec
import scala.collection.{AbstractSeq, GenSeq}
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import scala.collection.immutable.LinearSeq
import scala.collection.LinearSeqOptimized
import scala.collection.mutable.{ArrayBuilder, Builder}

import java.util.concurrent.atomic.AtomicBoolean

sealed abstract class VList[+A] extends AbstractSeq[A]
                                   with LinearSeq[A]
                                   //with Product
                                   with GenericTraversableTemplate[A, VList]
                                   with LinearSeqOptimized[A, VList[A]] {
                                   //with Serializable {
  override final def companion: GenericCompanion[VList] = VList

  def ::[B >: A](elem: B): VList[B]
}

private[immutable] final class VListBlock[+A](
  val base: Array[Any],
  var lastUsed: Int,
  val prev: VListBlock[A],
  val prevOffset: Int,
  val updating: AtomicBoolean
)

final class ::[+A] private[immutable] (block: VListBlock[A], offset: Int) extends VList[A] {
  override def isEmpty: Boolean = false

  override def head: A = block.base(offset).asInstanceOf[A]

  override def tail: VList[A] =
    if (offset > 0) new ::(block, offset - 1)
    else if (block.prev != null) new ::(block.prev, block.prevOffset)
    else VNil

  override def length: Int = {
    @tailrec
    def lengthRec(acc: Int, block: VListBlock[A], offset: Int): Int =
      if (block == null) acc
      else lengthRec(acc + offset + 1, block.prev, block.prevOffset)
    lengthRec(offset + 1, block.prev, block.prevOffset)
  }

  override def ::[B >: A](elem: B): VList[B] = {
    if (!block.updating.getAndSet(true) && offset == block.lastUsed && offset + 1 < block.base.length) {
      block.lastUsed += 1
      block.updating.set(false)
      block.base(offset + 1) = elem
      new ::(block, offset + 1)
    } else {
      block.updating.set(false)
      val newBlock = new VListBlock[A](
        base = new Array[Any](scala.math.round(block.base.length * VList.DefaultGrowthFactor)),
        lastUsed = 0,
        prev = block,
        prevOffset = offset,
        updating = new AtomicBoolean
      )
      newBlock.base(0) = elem
      new ::(newBlock, 0)
    }
  }

  override def stringPrefix: String = "VList"
}

case object VNil extends VList[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new UnsupportedOperationException("head of empty list")

  override def tail: VList[Nothing] = throw new UnsupportedOperationException("tail of empty list")

  override def length: Int = 0

  override def ::[B >: Nothing](elem: B): VList[B] = {
    val block = new VListBlock[B](
      base = new Array[Any](VList.DefaultInitialSize),
      lastUsed = 0,
      prev = null,
      prevOffset = 0,
      updating = new AtomicBoolean
    )
    block.base(0) = elem
    new ::(block, 0)
  }

  override def equals(that: Any): Boolean = that match {
    case that1: GenSeq[_] => that1.isEmpty
    case _ => false
  }
}

object VList extends SeqFactory[VList] {
  private[immutable] final val DefaultInitialSize: Int = 4
  private[immutable] final val DefaultGrowthFactor: Float = 2

  override def newBuilder[A]: Builder[A, VList[A]] =
    ArrayBuilder.make[Any]() mapResult { arr =>
      // Reverse the array in-place
      var i = 0
      var j = arr.length - 1
      while (i < j) {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
        i += 1
        j -= 1
      }

      new ::[A](
        block = new VListBlock[A](
          base = arr,
          lastUsed = arr.length - 1,
          prev = null,
          prevOffset = 0,
          updating = new AtomicBoolean
        ),
        offset = arr.length - 1
      )
    }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, VList[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  override final def empty[A]: VList[A] = VNil

  def apply[A](): VList[A] = VNil
}
