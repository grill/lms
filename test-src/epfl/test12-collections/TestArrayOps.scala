package scala.virtualization.lms
package epfl
package test12

import common._
import internal._
import java.io.PrintWriter
import scala.reflect.SourceContext

  trait VariablesNested extends VariablesExpOpt {
    override  def var_new[T:Manifest](init: Exp[T])(implicit pos: SourceContext): Var[T] = {
      //reflectEffect(NewVar(init)).asInstanceOf[Var[T]]
      Variable(reflectMutable(NewVar(init)))
    }

    override  def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
      reflectWriteMutable(lhs.e)(rhs)(Assign(lhs, rhs))
      Const()
    }

    override  def var_plusequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
      reflectWriteMutable(lhs.e)(rhs)(VarPlusEquals(lhs, rhs))
      Const()
    }

    override  def var_minusequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
      reflectWriteMutable(lhs.e)(rhs)(VarMinusEquals(lhs, rhs))
      Const()
    }
  
    override  def var_timesequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
      reflectWriteMutable(lhs.e)(rhs)(VarTimesEquals(lhs, rhs))
      Const()
    }
  
    override  def var_divideequals[T:Manifest](lhs: Var[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Unit] = {
      reflectWriteMutable(lhs.e)(rhs)(VarDivideEquals(lhs, rhs))
      Const()
    }

    override def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : Exp[T] = reflectReadMutable(v.e) { ReadVar(v) }
  }


trait ScalaGenVariablesNested extends ScalaGenVariables {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(a) => emitValDef(sym, quote(a))
    case NewVar(init) => {
      if (sym.tp != manifest[Variable[Nothing]]) {
        val obj = sym.asInstanceOf[Sym[Variable[Any]]]
            emitVarDef(obj, quote(init))
      }
    }
    case ReadVar(null) => {} // emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], "null")
    case Assign(a, b) => {
        val lhsIsNull = a match {
            case Def(Reflect(NewVar(y: Exp[_]),_,_)) => 
                if (y.tp == manifest[Nothing]) true
                else false
            case y@_ => false
        }
        val obj = a.asInstanceOf[Sym[Variable[Any]]]
        if (lhsIsNull) {
            emitVarDef(obj, quote(b))
        }
        else emitAssignment(sym, quote(a), quote(b))
    }
    //case Assign(a, b) => emitAssignment(quote(a), quote(b))
    case VarPlusEquals(a, b) => stream.println(quote(a) + " += " + quote(b))
    case VarMinusEquals(a, b) => stream.println(quote(a) + " -= " + quote(b))
    case VarTimesEquals(a, b) => stream.println(quote(a) + " *= " + quote(b))
    case VarDivideEquals(a, b) => stream.println(quote(a) + " /= " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

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

  class HashMap[K,V] (capacity: Int = 16) {
    val MAXIMUM_CAPACITY = 1 << 30
    val DEFAULT_LOAD_FACTOR = 0.75f
    var table: Array[Entry[K,V]] = new Array[Entry[K,V]] (capacity)
    var loadFactor: Float = DEFAULT_LOAD_FACTOR
    var threshold = (capacity * DEFAULT_LOAD_FACTOR).toInt
    var size = 0
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

  trait HashMapArrOpsExp extends HashMapArrOps with ArrayOpsExp with EffectExp with TupleOpsExp with EntryOpsExp
  with HashCodeOpsExp with BooleanOpsExp with PrimitiveOpsExp with ListOpsExp with FunctionsExp with VariablesExp
  with NumericOpsExp with EqualExp with WhileExp with OrderingOpsExp with IfThenElseExp
   with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
   with Blocks with MiscOpsExp with OptionOpsExp
   with VariablesNested
  {
  case class NewHashMap[K, V](mK: Manifest[K], mV: Manifest[V], size: Exp[Int]) extends Def[HashMap[K, V]]
  case class HashMapGetSize[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapGetTable[K, V](x: Exp[HashMap[K, V]]) extends Def[Array[Entry[K, V]]]
  case class HashMapGetThreshold[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapGetLoadFactor[K, V](x: Exp[HashMap[K, V]]) extends Def[Float]
  case class HashMapMAXIMUM_CAPACITY[K, V](x: Exp[HashMap[K, V]]) extends Def[Int]
  case class HashMapSetSize[K, V](x: Exp[HashMap[K, V]], newSz: Exp[Int]) extends Def[Unit]
  case class HashMapSetTable[K, V](x: Exp[HashMap[K, V]], newTable: Exp[Array[Entry[K, V]]]) extends Def[Unit]
  case class HashMapSetThreshold[K: Manifest, V](x: Exp[HashMap[K, V]], newThreshold: Exp[Int]) extends Def[Unit]

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

    def hashmap_new[K:Manifest,V:Manifest](n: Exp[Int], specializedKey: String = "", specializedValue: String = "")(implicit pos: SourceContext) : Rep[HashMap[K,V]] =
      //NewHashMap(manifest[K], manifest[V])
      reflectMutable(NewHashMap(manifest[K], manifest[V], n))
      //array_obj_new[Entry[K,V]](n)

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

      val threshold = hashmap_threshold(x)
      val max_capacity = hashmap_maximumCapacity(x)

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

              while(e.hasNext()) {
                val ht1 = int_tolong(__hashCode(e.getKey()))
                val ht2 = (ht1 >>> unit(20)) ^ (ht1 >>> unit(12)) ^ ht1
                val ht3 = ht2 ^ (ht2 >>> unit(7)) ^ (ht2 >>> unit(4))
                val z = int_binaryand(long_toint(h3), newCapacity - unit(1))

                val next = e.next
                e.setNext(hashmap_array_apply(newTable, z))
                //newTable.update(z, e)
                hashmap_array_update(newTable, z, readVar(e))
                
                var_assign(e, next)
              }

                val ht1 = int_tolong(__hashCode(e.getKey()))
                val ht2 = (ht1 >>> unit(20)) ^ (ht1 >>> unit(12)) ^ ht1
                val ht3 = ht2 ^ (ht2 >>> unit(7)) ^ (ht2 >>> unit(4))
                val z = int_binaryand(long_toint(h3), newCapacity - unit(1))

                val next = e.next
                e.setNext(hashmap_array_apply(newTable, z))
                //newTable.update(z, e)
                hashmap_array_update(newTable, z, readVar(e))
            
            }
            var_assign(j, readVar(j) + unit(1))
            //var_plusequals(j, unit(1))
          }

          hashmap_setTable(x, newTable)
          val loadFactor: Rep[Float] = hashmap_loadFactor(x)
          hashmap_setThreshold(x, (new CastingOpsCls(numeric_times(loadFactor, newCapacity.asInstanceOf[Rep[Float]])))
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
      reflectReadMutable (m) ( HashMapSetThreshold(m, i) )
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
      reflectWrite(m)(HashMapSetSize(m, i))
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


class TestArrayOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test12-"


  it("testNestedMutability") {
    withOutFile(prefix+"hash-map-nested-mutability") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val m = hashmap_new[Int, Int](unit(200))
          val t1 = NewArray[Entry[Int,Int]](unit(10))//Array(unit(1), unit(2), unit(3))
          //val t2 = m.table

          //val rN1 = reflectReadMutable (m.table) ( ArrayApply(m.table, unit(0)) )
          //reflectWrite(rN1)( ArrayUpdate(t1, unit(0), entry_new(unit(0),unit(1))) )

          //println(reflectReadMutable (m.table) ( ArrayApply(m.table, unit(0)) ))

          m.setTable(t1)
          println(reflectReadMutable (m.table) ( ArrayApply(m.table, unit(0)) ))

          //TODO: find out why DCE doesn't work here --> should have no side effects
          val rN2 = reflectReadMutable (t1) ( ArrayApply(t1, unit(0)) )
          val entry = entry_new(unit(0),unit(1))
          reflectWriteMutable(rN2)(entry)( ArrayUpdate(t1, unit(0), entry) )

          println(reflectReadMutable (m.table) ( ArrayApply(m.table, unit(0)) ))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapNestedMutability", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

 /* it("testIntArrayCreation") {
    withOutFile(prefix+"array-seq-creation") {
      val prog = new ArrayOps with MiscOps with ArrayOpsExp with MiscOpsExp{
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit(1), unit(2), unit(3))
          println(a(unit(0)))
        }

        def g(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit('a'), unit('b'), unit('c'))
          println(a(unit(0)))
        }
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps{ val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntArrayCreation", new PrintWriter(System.out))
      codegen.emitSource1(prog.g, "CharArrayCreation", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"array-seq-creation")
  }*/

    it("testGetAndSetSize") {
    withOutFile(prefix+"hash-map-get-and-set-size") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          println(a.size)
          a.setSize(unit(3))
          println(a.size)
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndSetSize", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testGetAndUpdate") {
    withOutFile(prefix+"hash-map-get-and-update") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          println(a(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdate", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testGetAndUpdateOpt") {
    withOutFile(prefix+"hash-map-get-and-update-opt") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          a.update(unit(2), unit(3))

          val v = (a(unit(1)).AsInstanceOf[Int]) + unit(1)
          a.update(unit(1), v)
          println(a(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdateOpt", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testGetAndUpdateOptVar") {
    withOutFile(prefix+"hash-map-get-and-update-opt-var") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp with VariablesNested {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(1))
          val n = var_new(unit(1))
          var_assign(n, unit(2))
          
          a.update(unit(2), readVar(n))

          a.update(unit(3), a(readVar(n)).get() + unit(1))

          a.foreach( {x => println(x)} )
          a.clear()
          //why is there a new table used each time??
          a.foreach( {x => println(x)} )
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdateOptVar", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testAssignmentProblem1") {
    withOutFile(prefix+"hash-map-assignment-problem1") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp
        with VariablesNested {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))

          val n = var_new(a)
          n.update(unit(1), unit(3))
          println(n(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap /*with ScalaGenVariablesNested */{ val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapAssignmentProblem1", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testAssignmentProblem2") {
    withOutFile(prefix+"hash-map-assignment-problem2") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp
        with VariablesNested {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))

          val n = var_new(a)
          n.update(unit(1), unit(3))

          println(a(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap /*with ScalaGenVariablesNested */ { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapAssignmentProblem2", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testAssignmentProblem3") {
    withOutFile(prefix+"hash-map-assignment-problem3") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExpOpt 
        with VariablesNested {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))

          println(a.size)

          val n = var_new(a)
          //n.setSize(unit(2))

          val n2 = var_new(readVar(n))
          n2.setSize(unit(3))

          println(a.size)
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap /*with ScalaGenVariablesNested */ { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapAssignmentProblem3", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  /*it("testNewArray") {
    withOutFile(prefix+"hash-map-get-and-update") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          println(a(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdate", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testUpdate") {
    withOutFile(prefix+"hash-map-update") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Array[Int]](unit(200))
          val c = array_obj_new[Int](unit(5))
          c.update(unit(0), unit(1))
          c.update(unit(1), unit(2))
          a.update(unit(1), c)
          //mutable can not be added to HashMap
          val b = a(unit(1)) //variables are not removed
          b.update(unit(0), unit(10))
          println(array_apply(a(unit(1)), unit(0)))

        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapUpdate", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testContains") {
    withOutFile(prefix+"hash-map-contains") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          println(a.contains(i))
          println(a.contains(unit(0)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapContains", new PrintWriter(System.out))
    }
  }

  it("testSize") {
    withOutFile(prefix+"hash-map-size") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          a.update(unit(1), unit(2))
          a.update(unit(2), unit(3))
          println(a.size)
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapSize", new PrintWriter(System.out))
    }
  }

  it("testForEach") {
    withOutFile(prefix+"hash-map-for-each") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          a.update(unit(1), unit(2))
          a.update(unit(2), unit(3))
          a.foreach({e => println(e)})
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapForEach", new PrintWriter(System.out))
    }
  }

  it("testDelete") {
    withOutFile(prefix+"hash-map-delete") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          a.update(unit(2), unit(3))
          println(a.size)
          a -= unit(2)
          println(a.size)
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapDelete", new PrintWriter(System.out))
    }

  }*/

}

/*
 object Test {
  def main(args: Array[String]) = {
    val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(200))
          a.update(unit(1), unit(2))
          println(a(unit(1)))
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps with ScalaCodeGenPkg with ScalaGenEntry with ScalaGenHashCodeOps { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdate", new PrintWriter(System.out))
  }
 }*/

 /*
 Ideas:

 * 
 * 

 */