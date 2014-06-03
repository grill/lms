package scala.virtualization.lms
package epfl
package test12

import common._
import internal._
import java.io.PrintWriter
import scala.reflect.SourceContext

  

  class Entry[K,V](val key: K, var value: V, var next: Entry[K,V] = null) {

   def hasNext = next != null

   override def equals(obj:Any) = {
      obj.isInstanceOf[Entry[K,V]] &&
        obj.asInstanceOf[Entry[K,V]].key == key && key != null &&
        obj.asInstanceOf[Entry[K,V]].value == value && value != null
    }

    override def toString(): String = {
      key + "=" + value
    } 
  }

  trait EntryOps extends Base with Variables {

    object EntryO {
      def apply[K:Manifest,V:Manifest](key: Rep[K], value: Rep[V])(implicit pos: SourceContext) =
        entry_new[K,V](key, value)
    }

    class entryOpsCls[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) {
      def hasNext() = entryHasNext(x)
      def next() = entryNext(x)
      def setNext(n: Rep[Entry[K,V]]) = entrySetNext(x, n)
      def getKey() = entryGetKey(x)
      def getValue() = entryGetValue(x) // Usefull for debugging but consumes all elements
      def setValue(v: Rep[V]) = entrySetValue(x, v)
    }

    implicit def EntryToEntryOps[K:Manifest,V:Manifest](x: Entry[K,V]) = new entryOpsCls[K,V](unit(x))
    implicit def repEntryToEntryOps[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) = new entryOpsCls[K,V](x)
    implicit def varEntryToEntryOps[K:Manifest,V:Manifest](x: Var[Entry[K,V]]) = new entryOpsCls[K,V](readVar(x))

    def entry_new[K:Manifest,V:Manifest](key: Rep[K], value: Rep[V]): Rep[Entry[K,V]]
    def entryHasNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]): Rep[Boolean]
    def entryNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]): Rep[Entry[K,V]]
    def entrySetNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], n: Rep[Entry[K,V]]): Rep[Unit]
    def entryGetKey[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]): Rep[K]
    def entryGetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]): Rep[V]
    def entrySetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], v: Rep[V]): Rep[Unit]
  }

  trait EntryOpsExp extends EntryOps with BaseExp with Effects /*with VariablesExp*/ {
    case class EntryCreate[K:Manifest,V:Manifest](key: Rep[K], value: Rep[V]) extends Def[Entry[K,V]]
    case class EntryHasNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) extends Def[Boolean]
    case class EntryNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) extends Def[Entry[K,V]]
    case class EntrySetNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], n: Rep[Entry[K,V]]) extends Def[Unit]
    case class EntryGetKey[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) extends Def[K]
    case class EntryGetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) extends Def[V]
    case class EntrySetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], v: Rep[V]) extends Def[Unit]

    def entry_new[K:Manifest,V:Manifest](key: Rep[K], value: Rep[V]) = reflectMutable(EntryCreate(key,value))
    def entryHasNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) = reflectReadMutable(x)(EntryHasNext(x))
    def entryNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) = reflectReadMutable(x)(EntryNext(x))
    def entrySetNext[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], n: Rep[Entry[K,V]]) = reflectWriteMutable (reflectReadMutable(x)(EntryNext(x)), reflectReadMutable(x)(EntryHasNext(x))) (n) (EntrySetNext(x,n))
    def entryGetKey[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) = reflectReadMutable(x)(EntryGetKey(x))
    def entryGetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]]) = reflectReadMutable(x) (EntryGetValue(x))
    def entrySetValue[K:Manifest,V:Manifest](x: Rep[Entry[K,V]], v: Rep[V]) = reflectWriteMutable (reflectReadMutable(x)(EntryGetValue(x))) (v) (EntrySetValue(x,v))
  }

  trait ScalaGenEntry extends ScalaGenBase {
    val IR: EntryOpsExp
    import IR._
   
    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case EntryCreate(k, v) =>
        emitValDef(sym, "new Entry(" + quote(k) + ", " + quote(v) + ")")
      case EntryHasNext(x) =>
        emitValDef(sym, quote(x) + ".hasNext")
      case EntryNext(x) =>
        emitValDef(sym, quote(x) + ".next")
      case EntrySetNext(x, n) =>
        emitValDef(sym, quote(x) + ".next = " + quote(n))
      case EntryGetKey(x) =>
        emitValDef(sym, quote(x) + ".key")
      case EntryGetValue(x) =>
        emitValDef(sym, quote(x) + ".value")
      case EntrySetValue(x, v) =>
        emitValDef(sym, quote(x) + ".value = " + quote(v))

      case _ => super.emitNode(sym, rhs)
    }
  }

  trait OptionOps extends Base with Variables {

    object SomeO {
      def apply[V:Manifest](value: Rep[V])(implicit pos: SourceContext) =
        option_new[V](value)
    }

    object NoneO {
      def apply[V:Manifest](implicit pos: SourceContext) =
        option_new[V]()
    }

    class optionOpsCls[V:Manifest](x: Rep[Option[V]]) {
      def isEmpty() = optionIsEmpty(x)
      def get() = optiponGet(x)
    }

    implicit def repOptionToOptionOps[V:Manifest](x: Rep[Option[V]]) = new optionOpsCls[V](x)
    implicit def varOptionToOptionOps[V:Manifest](x: Var[Option[V]]) = new optionOpsCls[V](readVar(x))

    def option_new[V:Manifest](): Rep[Option[V]]
    def option_new[V:Manifest](value: Rep[V]): Rep[Option[V]]
    def optionIsEmpty[V:Manifest](x: Rep[Option[V]]): Rep[Boolean]
    def optiponGet[V:Manifest](x: Rep[Option[V]]): Rep[V]
  }

 trait OptionOpsExp extends OptionOps with BaseExp with Effects /*with VariablesExp*/ {
    case class OptionSome[V:Manifest](value: Rep[V]) extends Def[Option[V]]
    case class OptionNone[V:Manifest] extends Def[Option[V]]
    case class OptionIsEmpty[V:Manifest](x: Rep[Option[V]]) extends Def[Boolean]
    case class OptionGet[V:Manifest](x: Rep[Option[V]]) extends Def[V]

    def option_new[V:Manifest]() = OptionNone[V]
    def option_new[V:Manifest](value: Rep[V]) = OptionSome(value)
    def optionIsEmpty[V:Manifest](x: Rep[Option[V]]) = OptionIsEmpty(x)
    def optiponGet[V:Manifest](x: Rep[Option[V]]) = OptionGet(x)
  }

  trait ScalaGenOption extends ScalaGenBase {
    val IR: OptionOpsExp
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case OptionSome(v) =>
        emitValDef(sym, "Some(" + quote(v) + ")")
      case OptionNone() =>
        emitValDef(sym, "None")
      case OptionIsEmpty(x) =>
        emitValDef(sym, quote(x) + ".isEmpty")
      case OptionGet(x) =>
        emitValDef(sym, quote(x) + ".get")

      case _ => super.emitNode(sym, rhs)
    }
  }

  trait HashMapArrOps extends Base with Variables with TupleOps {

    object HashMap {
      def apply[K:Manifest,V:Manifest](n: Rep[Int], specializedKey: String = "", specializedValue:String = "")(implicit pos: SourceContext) =
        hashmap_new[K,V](n, specializedKey, specializedValue)
    }

    //type Entry[K,V] = List[(K,V)]

    implicit def HashMapToRepHashMapOps[K:Manifest,V:Manifest](m: HashMap[K,V]) = new hashmapOpsCls[K,V](unit(m))
    implicit def repHashMapToHashMapOps[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) = new hashmapOpsCls[K,V](m)
    implicit def varrepHashMapToHashMapOps[K:Manifest,V:Manifest](m: Var[HashMap[K,V]]) = new hashmapOpsCls[K,V](readVar(m))

    class hashmapOpsCls[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]]) {
      def apply(k: Rep[K])(implicit pos: SourceContext) = hashmap_apply(m, k)
      def update(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = hashmap_update(m,k,v)
      def contains(k: Rep[K])(implicit pos: SourceContext) = hashmap_contains(m, k)
      def size(implicit pos: SourceContext) = hashmap_size(m)
      //added for testing nested mutability with simple data types
      def setSize(i: Rep[Int])(implicit pos: SourceContext) = hashmap_setSize(m, i)
      def foreach(block: Rep[Entry[K,V]] => Rep[Unit])(implicit pos: SourceContext) = hashmap_foreach(m, block)
      def clear()(implicit pos: SourceContext) = hashmap_clear(m)
      def -=(v: Rep[K])(implicit pos:SourceContext) = hashmap_-=(m,v)

      //added for testing nested mutability with complex datatypes
      def table(implicit pos: SourceContext) = hashmap_table(m)
      def setTable(newTable: Rep[Array[Entry[K, V]]])(implicit pos: SourceContext) = hashmap_setTable(m, newTable)
    }

    def hashmap_new[K:Manifest,V:Manifest](n: Rep[Int], specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,V]]
    def hashmap_apply[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Option[V]]
    def hashmap_update[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit]
    def hashmap_contains[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
    def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int]
    //added for testing
    def hashmap_setSize[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
    //def hashmap_map[K:Manifest,V:Manifest, A:Manifest, B:Manifest](m: Rep[HashMap[K,V]], f: Rep[Entry[K,V]]=>Rep[Entry[A,B]]): Rep[Array[Entry[A,B]]]
    def hashmap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], block: Rep[Entry[K,V]] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
    def hashmap_clear[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit]
    def hashmap_-=[K: Manifest, V: Manifest](m: Rep[HashMap[K,V]], v: Rep[K])(implicit pos: SourceContext): Rep[Unit]

    //added for testing
    def hashmap_table[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Array[Entry[K, V]]]
    def hashmap_setTable[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], newTable: Rep[Array[Entry[K, V]]])(implicit pos: SourceContext): Rep[Unit]

  }



  class HashMap[K,V] (capacity: Int = 16) {
    val MAXIMUM_CAPACITY = 1 << 30
    val DEFAULT_LOAD_FACTOR = 0.75f
    var table: Array[Entry[K,V]] = new Array[Entry[K,V]] (capacity)
    var loadFactor: Float = DEFAULT_LOAD_FACTOR
    var threshold = (capacity * DEFAULT_LOAD_FACTOR).toInt
    var size = 0
  }
  
  trait HashMapArrOpsExp extends HashMapArrOps with ArrayOpsExp with EffectExp with TupleOpsExp with EntryOpsExp
  with HashCodeOpsExp with BooleanOpsExp with PrimitiveOpsExp with ListOpsExp with FunctionsExp with VariablesExp
  with NumericOpsExp with EqualExp with WhileExp with OrderingOpsExp with IfThenElseExp
   with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
   with Blocks with MiscOpsExp with OptionOpsExp
   with VariablesNested
  {
  case class NewHashMap[K, V](mK: Manifest[K], mV: Manifest[V], size: Exp[Int]) extends Def[HashMap[K, V]]
  //mutable
  case class HashMapGetSize[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapSetSize[K, V](x: Exp[HashMap[K, V]], newSz: Exp[Int]) extends Def[Unit]
  case class HashMapGetLoadFactor[K, V](x: Exp[HashMap[K, V]]) extends Def[Float]
  case class HashMapMAXIMUM_CAPACITY[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapGetThreshold[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapSetThreshold[K: Manifest, V](x: Exp[HashMap[K, V]], newThreshold: Exp[Int]) extends Def[Unit]
  //nested mutable
  case class HashMapGetTable[K, V](x: Exp[HashMap[K, V]]) extends Def[Array[Entry[K, V]]]
  case class HashMapSetTable[K, V](x: Exp[HashMap[K, V]], newTable: Exp[Array[Entry[K, V]]]) extends Def[Unit]

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case HashMapSetTable(m,t) => syms(t)
    case HashMapGetTable(m) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case HashMapSetTable(m,t) => Nil
    case HashMapGetTable(m) => syms(m)
    case _ => super.extractSyms(e)
  }

    def hashmap_new[K:Manifest,V:Manifest](n: Exp[Int], specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,V]] = {
      //NewHashMap(manifest[K], manifest[V])
      val m = reflectMutable(NewHashMap(manifest[K], manifest[V], n))
      //array_obj_new[Entry[K,V]](n)

      reflectReadMutable(m)(HashMapGetTable(m))
      hashmap_array_apply(reflectReadMutable(m)(HashMapGetTable(m)), unit(0))
      reflectReadMutable(m)(HashMapGetSize(m))
      reflectReadMutable(m)(HashMapGetLoadFactor(m))
      reflectReadMutable(m)(HashMapMAXIMUM_CAPACITY(m))
      reflectReadMutable(m)(HashMapGetThreshold(m))

      m
    }

    def hashmap_apply[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Option[V]] = {
      //tuple2_get2(ArrayApply(m, k))
      val m: Rep[Array[Entry[K,V]]] = hashmap_table(x)//reflectReadMutable (x) ( HashMapGetTable(x) )

      val h1 = int_tolong(__hashCode(k))
      val h2 = (h1 >>> unit(20)) ^ (h1 >>> unit(12)) ^ h1
      val h3 = h2 ^ (h2 >>> unit(7)) ^ (h2 >>> unit(4))
      val idx = int_binaryand(long_toint(h3), m.length - unit(1))
      val el = hashmap_array_apply(m, idx)
      val n = var_new(el)
        //reflectReadMutable (m) ( ArrayApply(m, idx) )
        //array_apply(m, idx)
      //)

      if(readVar(n) == unit(null)) {
        NoneO[V]
        //unit(0).asInstanceOf[Rep[V]]
        //unit(null).asInstanceOf[Rep[V]]
      } else {
        while(boolean_and(n.hasNext(), notequals(n.getKey(), k) )) {
         var_assign(n, n.next())
        }

        if(n.getKey() == k) {
          SomeO(n.getValue())
        } else {
          NoneO[V]
          //unit(0).asInstanceOf[Rep[V]]
          //unit(null).asInstanceOf[Rep[V]]
        }
      }
    }

    def hashmap_update[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit] = {
      hashmap_add(x, k, v)
      hashmap_resize(x)
    }

    def hashmap_add[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Unit] = {
      val m = hashmap_table(x)//reflectReadMutable (x) { HashMapGetTable(x) }

      val h1 = int_tolong(__hashCode(k))
      val h2 = (h1 >>> unit(20)) ^ (h1 >>> unit(12)) ^ h1
      val h3 = h2 ^ (h2 >>> unit(7)) ^ (h2 >>> unit(4))
      val idx = int_binaryand(long_toint(h3), m.length - unit(1))
      val el = hashmap_array_apply(m, idx)
        //reflectReadMutable (m) ( ArrayApply(m, idx) )
        //array_apply(m, idx)
      val n = var_new(el)
      
      val size = hashmap_size(x) //map.size   //reflectNested

      if(readVar(n) == unit(null)) {

        //reflectWrite(el) ( ArrayUpdate(m, idx, entry_new(k,v)) )
        //val new_entry = 
        hashmap_array_update(m, idx, entry_new(k,v))
        //reflectWriteMutable(reflectReadMutable (m) ( ArrayApply(m, idx) )) (new_entry) ( ArrayUpdate(m, idx, new_entry) )
        //array_update(m, idx, entry_new(k,v))

        // reflectWrite(m) ==> backTracking      --> update calculate dependencies
        //--> possible update reflectWrite(ArrayApply(x,i))
 
        hashmap_setSize(x, numeric_plus(size, unit(1)))  //reflectWrite(HashMapGetSize(x))  --> overloaded reflectWrite,
      } else {
        while(n.hasNext() && n.getKey() != k) {
          var_assign(n, n.next())
        }

        if(n.getKey() == k) {
          n.setValue(v)
        } else {
          n.setNext(entry_new(k,v))
          hashmap_setSize(x, numeric_plus(size, unit(1)))
        }
      }

    }

    def hashmap_resize[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit] = {
      val m = hashmap_table(x)//reflectReadMutable (x) { HashMapGetTable(x) }
      val threshold = hashmap_threshold(x)
      val max_capacity = hashmap_maximumCapacity(x)
      val size = hashmap_size(x) 

      //var_assign(size, size + 1)
      if (ordering_gteq(size, threshold)) {
        val oldCapacity = m.length
        if(oldCapacity == max_capacity) {
          hashmap_setThreshold(x, Int.MaxValue)
          //var_assign(threshold, unit(Int.MaxValue))
        } else {
          val newCapacity = unit(2) * oldCapacity
          val newTable = array_obj_new[Entry[K, V]](newCapacity)

          //transfer table
          val j: Var[Int] = var_new(unit(0))

          while(j < m.length) {
            val e: Var[Entry[K, V]] = var_new(hashmap_array_apply(m,j))
            if(e != unit(null)) {
              //array_update(m, j, unit(null))
              hashmap_array_update(m, j, unit(null))

              while(e != unit(null)) {
                val ht1 = int_tolong(__hashCode(e.getKey()))
                val ht2 = (ht1 >>> unit(20)) ^ (ht1 >>> unit(12)) ^ ht1
                val ht3 = ht2 ^ (ht2 >>> unit(7)) ^ (ht2 >>> unit(4))
                val z = int_binaryand(long_toint(ht3), newCapacity - unit(1))

                val next = e.next
                e.setNext(hashmap_array_apply(newTable, z))
                //newTable.update(z, e)
                hashmap_array_update(newTable, z, readVar(e))
                
                var_assign(e, next)
              }
            
            }
            var_assign(j, readVar(j) + unit(1))
            //var_plusequals(j, unit(1))
          }

          hashmap_setTable(x, newTable)
          val loadFactor: Rep[Float] = hashmap_loadFactor(x)
          hashmap_setThreshold(x, (numeric_times(loadFactor, newCapacity.asInstanceOf[Rep[Float]]))
            .AsInstanceOf[Int])
          //var_assign(threshold, newCapacity*HashMapGetLoadFactor(x))
        }
      }
    }

    def hashmap_contains[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Boolean] = { 
      val m: Rep[Array[Entry[K,V]]] = HashMapGetTable(x)
      val h1 = int_tolong(__hashCode(k))
      val h2 = (h1 >>> unit(20)) ^ (h1 >>> unit(12)) ^ h1
      val h3 = h2 ^ (h2 >>> unit(7)) ^ (h2 >>> unit(4))
      val n = var_new(array_apply(m, int_binaryand(long_toint(h3), m.length - unit(1))))

      if(readVar(n) == unit(null)) {
        unit(false)
      } else {
        while(n.hasNext() && n.getKey() != k) {
          var_assign(n, n.next())
        }

        n.getKey() == k
      }
    }

    def hashmap_array_update[K:Manifest,V:Manifest](m: Rep[Array[Entry[K,V]]], idx: Rep[Int], new_entry: Rep[Entry[K,V]])(implicit pos: SourceContext): Rep[Unit] = {
        reflectWriteMutable (reflectReadMutable (m) ( ArrayApply(m, idx) )) (new_entry) ( ArrayUpdate(m, idx, new_entry) )
    }

    def hashmap_array_apply[K:Manifest,V:Manifest](m: Rep[Array[Entry[K,V]]], idx: Rep[Int])(implicit pos: SourceContext): Rep[Entry[K,V]] = {
        reflectReadMutable (m) ( ArrayApply(m, idx) )
    }

    def hashmap_loadFactor[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Float] = {
      //reflectReadMutable (m) ( HashMapGetSize(m) )
      reflectReadMutable (m) ( HashMapGetLoadFactor(m) )
      //HashMapGetSize(m)
    }

    def hashmap_threshold[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int] = {
      //reflectReadMutable (m) ( HashMapGetSize(m) )
      reflectReadMutable (m) ( HashMapGetThreshold(m) )
      //HashMapGetSize(m)
    }
    
    def hashmap_maximumCapacity[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int] = {
      //reflectReadMutable (m) ( HashMapGetSize(m) )
      reflectReadMutable (m) ( HashMapMAXIMUM_CAPACITY(m) )
      //HashMapGetSize(m)
    }

    def hashmap_setThreshold[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = {
      reflectWriteMutable (reflectReadMutable (m) ( HashMapGetThreshold(m) )) (i) ( HashMapSetThreshold(m, i) )
    }

    def hashmap_table[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Array[Entry[K, V]]] = {
      reflectReadMutable (m) ( HashMapGetTable(m) )
    }

    def hashmap_setTable[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], newTable: Rep[Array[Entry[K, V]]])(implicit pos: SourceContext): Rep[Unit] = {
      //reflectMutableWrite multiple cases:
      //clean and add -> replace references contained (equals sym)
      //add -> references contained (contains sym)
      //clone -> create new representative (clone sym)
      //extract??? => Read and Write at the same time
      reflectWriteMutable( reflectReadMutable(m) (HashMapGetTable(m)) ) (newTable) (HashMapSetTable(m, newTable))
    }

    def hashmap_size[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Int] = {
      //reflectReadMutable (m) ( HashMapGetSize(m) )
      reflectReadMutable (m) ( HashMapGetSize(m) )
      //HashMapGetSize(m)
    }

    def hashmap_setSize[K:Manifest,V:Manifest](m: Rep[HashMap[K,V]], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = {
      reflectWriteMutable(reflectReadMutable (m) ( HashMapGetSize(m) ))(i)(HashMapSetSize(m, i))
      //reflectWrite( reflectReadMutable(m) (HashMapGetSize(m)) ) (HashMapSetSize(m, i))
      //reflectWrite(HashMapGetSize(m)) (HashMapSetSize(m, i))
    }

    /*def hashmap_map[K:Manifest,V:Manifest, A:Manifest, B:Manifest](m: Rep[HashMap[K,V]], f: Rep[Entry[K,V]]=>Rep[Entry[A,B]]): Rep[Array[Entry[A,B]]] = {
      var i = unit(0)
      var n: Rep[Entry[K,V]] = unit(null)
      val nmap = hashmap_new[A,B](m.length)

      hashmap_foreach(m, {e => val n = f(e); hashmap_update(nmap, e.key, e.value)})
      nmap
    }*/

    def hashmap_foreach[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]], f: Rep[Entry[K,V]] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit] = {
      val m: Rep[Array[Entry[K,V]]] = hashmap_table(x)
      val i = var_new(unit(0))
      val n = var_new(unit(null).AsInstanceOf[Entry[K,V]])

      while(i < m.length) {
        val el = hashmap_array_apply(m,i)
        if(el != unit(null)) {
          var_assign(n, el)
          f(el)
          while(n.hasNext()) {
            var_assign(n, n.next())
            f(readVar(n))
          }
        }

        i = i + unit(1)
      }
    }

    def hashmap_clear[K:Manifest,V:Manifest](x: Rep[HashMap[K,V]])(implicit pos: SourceContext): Rep[Unit] = {
      val m = hashmap_table(x)
      val i = var_new(unit(0))

      while(i < m.length) {
        hashmap_array_update(m, i, unit(null))
        var_assign(i, readVar(i) + unit(1))
      }
    }

    def hashmap_-=[K: Manifest, V: Manifest](x: Rep[HashMap[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Unit] = {
      val m = hashmap_table(x)
      val h1 = int_tolong(__hashCode(k))
      val h2 = (h1 >>> unit(20)) ^ (h1 >>> unit(12)) ^ h1
      val h3 = h2 ^ (h2 >>> unit(7)) ^ (h2 >>> unit(4))
      val idx = int_binaryand(long_toint(h3), m.length - unit(1))

      val p = var_new(unit(null).AsInstanceOf[Entry[K,V]])
      val n = var_new(hashmap_array_apply(m, idx))

      if(readVar(n) != unit(null)) {
        while(readVar(n).hasNext() && readVar(n).getKey() != k) {
          var_assign(p, readVar(n))
          var_assign(n, readVar(n).next())
        }

        if(readVar(p) == unit(null))
          hashmap_array_update(m, idx, unit(null))
        else if(readVar(n).getKey() == k) {
          p.setNext(readVar(n).next())
        }
      }
    }

    
  }

trait ScalaGenHashMap extends ScalaGenBase with ScalaGenMiscOps {
  val IR: HashMapArrOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewHashMap(mK, mV, n) => emitValDef(sym, "new HashMap[" + mK + "," + mV + "](" + quote(n) + ")")
    case HashMapGetSize(x) => emitValDef(sym, "" + quote(x) + ".size")
    case HashMapGetTable(x) => emitValDef(sym, "" + quote(x) + ".table")
    //case HashMapGetTableIndex(x, index) => emitValDef(sym, "" + quote(x) + ".table(" + quote(index) + ")")
    case HashMapGetThreshold(x) => emitValDef(sym, "" + quote(x) + ".threshold")
    case HashMapGetLoadFactor(x) => emitValDef(sym, "" + quote(x) + ".loadFactor")
    case HashMapMAXIMUM_CAPACITY(x) => emitValDef(sym, "" + quote(x) + ".MAXIMUM_CAPACITY")
    case HashMapSetSize(x, newSz) => emitAssignment(sym, "" + quote(x) + ".size", quote(newSz))
    case HashMapSetTable(x, newTable) => emitAssignment(sym, "" + quote(x) + ".table", quote(newTable))
    //case HashMapSetTableIndex(x, tbl, index, value) => emitAssignment("" + quote(x) + ".table(" + quote(index) + ")", quote(value))
    case HashMapSetThreshold(x, newThreshold) => emitAssignment(sym, "" + quote(x) + ".threshold", quote(newThreshold))
    case _ => super.emitNode(sym, rhs)
  }
}
