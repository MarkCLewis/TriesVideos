import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

trait BVec[+E] {
  def :+[E2 >: E : ClassTag](e: E2): BVec[E2]

  def apply(index: Int): E

  def updated[E2 >: E : ClassTag](index: Int, e: E2): BVec[E2]

  def size(): Int

  protected def isFull(): Boolean

  protected def newSibling[E2 >: E : ClassTag](e: E2): BVec[E2]
}

object BVec {
  private val BitsInAlphabet = 2

  def empty[E : ClassTag]: BVec[E] = Leaf[E](ArraySeq[E]())

  private case class Leaf[+E : ClassTag](data: ArraySeq[E]) extends BVec[E] {
    def :+[E2 >: E : ClassTag](e: E2): BVec[E2] = {
      if (data.length >= (1 << BitsInAlphabet)) {
        val sibling = Leaf(ArraySeq(e))
        Internal(1, ArraySeq(this, sibling))
      } else {
        Leaf(data :+ e)
      }
    }

    def apply(index: Int): E = data(index & (~0 >>> (32 - BitsInAlphabet)))

    def updated[E2 >: E : ClassTag](index: Int, e: E2): BVec[E2] = 
      Leaf(data.updated(index & (~0 >>> (32 - BitsInAlphabet)), e))

    def size(): Int = data.length

    protected def isFull(): Boolean = data.length >= (1 << BitsInAlphabet)

    protected def newSibling[E2 >: E : ClassTag](e: E2): BVec[E2] = Leaf(ArraySeq(e))
  }

  private case class Internal[+E : ClassTag](level: Int, children: ArraySeq[BVec[E]]) extends BVec[E] {
    def :+[E2 >: E : ClassTag](e: E2): BVec[E2] = {
      if (isFull()) {
        Internal(level + 1, ArraySeq(this, newSibling(e)))
      } else if (children.last.isFull()) {
        val newChildren = children :+ children.head.newSibling(e)
        Internal(level, newChildren)
      } else {
        val newChildren = children.updated(children.length - 1, children.last :+ e)
        Internal(level, newChildren)
      }
    }

    def apply(index: Int): E = 
      children((index >>> (level * BitsInAlphabet)) & (~0 >>> (32 - BitsInAlphabet))).apply(index)

    def updated[E2 >: E : ClassTag](index: Int, e: E2): BVec[E2] = {
      val childIndex = (index >>> (level * BitsInAlphabet)) & (~0 >>> (32 - BitsInAlphabet))
      Internal(level, 
        children.updated(childIndex, children(childIndex).updated(index, e))
      )
    }

    def size(): Int = (children.length - 1) * (1 << BitsInAlphabet * level) + children.last.size()

    protected def isFull(): Boolean = children.length >= (1 << BitsInAlphabet) && children.last.isFull()

    protected def newSibling[E2 >: E : ClassTag](e: E2): BVec[E2] = Internal(level, ArraySeq(children.head.newSibling(e)))
  }
}

