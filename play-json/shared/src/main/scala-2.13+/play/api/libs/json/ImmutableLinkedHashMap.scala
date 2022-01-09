package play.api.libs.json

import java.util.{ LinkedHashMap => JLinkedHashMap }
import scala.collection.AbstractIterator
import scala.collection.MapFactory
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Wraps a Java String-keyed map as a Scala immutable.Map.
 */
private[json] class ImmutableLinkedHashMap[A, +B](underlying: JLinkedHashMap[A, B]) extends Map[A, B] {

  override def get(key: A): Option[B] = Option(underlying.get(key))

  override def removed(key: A): Map[A, B] = {
    val c = shallowCopy()
    c.remove(key)
    new ImmutableLinkedHashMap(c)
  }

  override def updated[V1 >: B](key: A, value: V1): Map[A, V1] = {
    val c = shallowCopy[V1](size + 1)
    c.put(key, value)
    new ImmutableLinkedHashMap(c)
  }

  override def mapFactory: MapFactory[Map] = ImmutableLinkedHashMap

  override def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    private[this] val ui = underlying.entrySet().iterator()

    override def hasNext: Boolean = ui.hasNext

    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize

    override def next(): (A, B) = {
      val e = ui.next()
      (e.getKey, e.getValue)
    }
  }

  override def knownSize: Int = underlying.size()
  override def size: Int      = underlying.size()

  private def shallowCopy[V1 >: B](sizeHint: Int = size): JLinkedHashMap[A, V1] = {
    val c = new JLinkedHashMap[A, V1](sizeHint)
    for ((k, v) <- this) c.put(k, v)
    c
  }
}

private[json] object ImmutableLinkedHashMap extends MapFactory[Map] {
  private object EmptyMap extends ImmutableLinkedHashMap[Any, Nothing](new JLinkedHashMap(0))
  override def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]): Map[K, V] = {
    val i = it.iterator
    if (i.isEmpty) empty
    else {
      val lhm = i.foldLeft(new JLinkedHashMap[K, V]()) { (t, v) =>
        t.put(v._1, v._2)
        t
      }
      new ImmutableLinkedHashMap(lhm)
    }
  }

  override def newBuilder[K, V]: mutable.Builder[(K, V), Map[K, V]] = {
    ArrayBuffer
      .newBuilder[(K, V)]
      .mapResult { buf =>
        // buffering makes the size knowable resulting in a compact final hashmap
        // in practice, most objects are much smaller than LHM.DEFAULT_INITIAL_CAPACITY=16.
        val lhm = new JLinkedHashMap[K, V](buf.size)
        buf.foreach(t => lhm.put(t._1, t._2))
        new ImmutableLinkedHashMap(lhm)
      }
  }
}
