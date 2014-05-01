package scala.virtualization.lms
package common

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext
import scala.collection.mutable.{HashMap,Set}

trait ArrayOps extends Variables {

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToArrayOps[T:Manifest](x: Var[Array[T]]) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Manifest](a: Array[T]) = new ArrayOpsCls(unit(a))

  // substitution for "new Array[T](...)"
  // TODO: look into overriding __new for arrays
  object NewArray {
    def apply[T:Manifest](n: Rep[Int], specializedType: Rep[String] = unit("")) = array_obj_new(n, specializedType)    
  }

  object Array {
    def apply[T:Manifest](xs: T*) = array_obj_fromseq(xs)
  }

  class ArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = array_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = array_update(a,n,y)
    def length(implicit pos: SourceContext) = array_length(a)
    def foreach(block: Rep[T] => Rep[Unit])(implicit pos: SourceContext) = array_foreach(a, block)
    //def filter(f: Rep[T] => Rep[Boolean]) = array_filter(a, f)
	  // def groupBy[B: Manifest](f: Rep[T] => Rep[B]) = array_group_by(a,f)
    def sort(implicit pos: SourceContext) = array_sort(a)
    // def map[B:Manifest](f: Rep[T] => Rep[B]) = array_map(a,f)
    def toSeq = array_toseq(a)
	  def sum = array_sum(a)
    def zip[B: Manifest](a2: Rep[Array[B]]) = array_zip(a,a2)
    def corresponds[B: Manifest](a2: Rep[Array[B]]) = array_corresponds(a,a2)
    def mkString(del: Rep[String] = unit("")) = array_mkString(a,del)
    //this operation can be used, whenever an array is accepted as
    //a function parameter and initially it's assumed to be immutable
    def mutable = array_mutable(a)
  }    

  def array_obj_new[T:Manifest](n: Rep[Int], specializedType: Rep[String] = unit("")): Rep[Array[T]]
  def array_obj_fromseq[T:Manifest](xs: Seq[T]): Rep[Array[T]]
  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def array_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
  def array_length[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext) : Rep[Int]
  def array_foreach[T:Manifest](x: Rep[Array[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def array_filter[T : Manifest](l: Rep[Array[T]], f: Rep[T] => Rep[Boolean])(implicit pos: SourceContext): Rep[Array[T]]
  def array_group_by[T : Manifest, B: Manifest](l: Rep[Array[T]], f: Rep[T] => Rep[B])(implicit pos: SourceContext): Rep[HashMap[B, Array[T]]]
  def array_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
  def array_unsafe_copy[T:Manifest](src: Rep[Array[T]], srcPos: Rep[Int], dest: Rep[Array[T]], destPos: Rep[Int], len: Rep[Int])(implicit pos: SourceContext): Rep[Unit]
  def array_sort[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext): Rep[Array[T]]
  def array_map[A:Manifest,B:Manifest](a: Rep[Array[A]], f: Rep[A] => Rep[B]): Rep[Array[B]]
  def array_toseq[A:Manifest](a: Rep[Array[A]]): Rep[Seq[A]]
  def array_sum[A:Manifest](a: Rep[Array[A]]): Rep[A]
  def array_zip[A:Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]): Rep[Array[(A,B)]]
  def array_mkString[A: Manifest](a: Rep[Array[A]], del: Rep[String] = unit("")): Rep[String]
  // limited support for corresponds (tests equality)
  def array_corresponds[A: Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]): Rep[Boolean]
  def array_mutable[A: Manifest](a: Rep[Array[A]]): Rep[Array[A]]
}

trait ArrayOpsExp extends ArrayOps with EffectExp with VariablesExp {
  case class ArrayNew[T:Manifest](n: Exp[Int], specializedType: Rep[String] = unit("")) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayFromSeq[T:Manifest](xs: Seq[T]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int]) extends Def[T]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], n: Exp[Int], y: Exp[T]) extends Def[Unit]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int] {
    val m = manifest[T]
  }
  case class ArrayForeach[T](a: Exp[Array[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class ArrayFilter[T : Manifest](l: Exp[Array[T]], x: Sym[T], block: Block[Boolean]) extends Def[Array[T]]
  case class ArrayGroupBy[T: Manifest, B: Manifest](l: Exp[Array[T]], x: Sym[T], block: Block[B]) extends Def[HashMap[B, Array[T]]]
  case class ArrayCopy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int]) extends Def[Unit] {
    val m = manifest[T]
  }
  case class ArraySort[T:Manifest](x: Exp[Array[T]]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayMap[A:Manifest,B:Manifest](a: Exp[Array[A]], x: Sym[A], block: Block[B]) extends Def[Array[B]] {
    val array = NewArray[B](a.length)
  }
  case class ArrayToSeq[A:Manifest](x: Exp[Array[A]]) extends Def[Seq[A]]
  case class ArraySum[A:Manifest](x: Exp[Array[A]]) extends Def[A]
  case class ArrayZip[A:Manifest, B: Manifest](x: Exp[Array[A]], x2: Exp[Array[B]]) extends Def[Array[(A,B)]]
  case class ArrayMkString[A:Manifest](a: Exp[Array[A]], b: Exp[String] = unit("")) extends Def[String]
  case class ArrayCorresponds[A:Manifest, B: Manifest](x: Exp[Array[A]], x2: Exp[Array[B]]) extends Def[Boolean]
  case class ArrayMutable[A:Manifest](a: Exp[Array[A]]) extends Def[Array[A]]


  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a, i, v) => syms(v)
    case ArrayApply(a, i) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a, i, v) => Nil
    case ArrayApply(a, i) => syms(a)
    case _ => super.extractSyms(e)
  }
  
  def array_obj_new[T:Manifest](n: Exp[Int], specializedType: Rep[String] = unit("")) = reflectMutable(ArrayNew(n, specializedType))
  def array_obj_fromseq[T:Manifest](xs: Seq[T]) = /*reflectMutable(*/ ArrayFromSeq(xs) /*)*/
  def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = ArrayApply(x, n)
  def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit pos: SourceContext) = reflectWrite(x)(ArrayUpdate(x,n,y))
  def array_unsafe_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = ArrayUpdate(x,n,y)
  def array_length[T:Manifest](a: Exp[Array[T]])(implicit pos: SourceContext) : Rep[Int] = ArrayLength(a)
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }
  def array_filter[T : Manifest](l: Exp[Array[T]], f: Exp[T] => Exp[Boolean])(implicit pos: SourceContext) = {
    val a = fresh[T]
    val b = reifyEffects(f(a))
    reflectEffect(ArrayFilter(l, a, b), summarizeEffects(b).star)
  }
  def array_group_by[T : Manifest, B: Manifest](l: Exp[Array[T]], f: Exp[T] => Exp[B])(implicit pos: SourceContext) = {
    val a = fresh[T]
    val b = reifyEffects(f(a))
    reflectEffect(ArrayGroupBy(l, a, b), summarizeEffects(b).star)
  }

  def array_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = reflectWrite(dest)(ArrayCopy(src,srcPos,dest,destPos,len))
  def array_unsafe_copy[T:Manifest](src: Exp[Array[T]], srcPos: Exp[Int], dest: Exp[Array[T]], destPos: Exp[Int], len: Exp[Int])(implicit pos: SourceContext) = ArrayCopy(src,srcPos,dest,destPos,len)
  def array_sort[T:Manifest](x: Exp[Array[T]])(implicit pos: SourceContext) = ArraySort(x)
  def array_map[A:Manifest,B:Manifest](a: Exp[Array[A]], f: Exp[A] => Exp[B]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(ArrayMap(a, x, b), summarizeEffects(b))
  }
  def array_toseq[A:Manifest](a: Exp[Array[A]]) = ArrayToSeq(a)
  def array_sum[A:Manifest](a: Exp[Array[A]]) = ArraySum(a)
  def array_zip[A:Manifest, B: Manifest](a: Exp[Array[A]], a2: Exp[Array[B]]) = ArrayZip(a,a2)
  def array_mkString[A: Manifest](a: Rep[Array[A]], del: Rep[String] = unit("")) = ArrayMkString(a, del)
  def array_corresponds[A: Manifest, B: Manifest](a: Rep[Array[A]], a2: Rep[Array[B]]) = ArrayCorresponds(a,a2)
  def array_mutable[A: Manifest](a: Rep[Array[A]]) = reflectMutable(ArrayMutable(a))
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case ArrayLength(x) => array_length(f(x))
    case e@ArraySort(x) => array_sort(f(x))(e.m,pos)
    case e@ArrayCopy(a,ap,d,dp,l) => toAtom(ArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m))(mtype(manifest[A]),pos)
    case Reflect(e@ArrayNew(n, sType), u, es) => reflectMirrored(Reflect(ArrayNew(f(n), sType)(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@ArrayLength(x), u, es) => reflectMirrored(Reflect(ArrayLength(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(ArrayApply(l,r), u, es) => reflectMirrored(Reflect(ArrayApply(f(l),f(r))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArraySort(x), u, es) => reflectMirrored(Reflect(ArraySort(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(ArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(ArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayMutable(a), u, es) => reflectMirrored(Reflect(ArrayMutable(f(a)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case ArrayMap(a, x, body) => syms(a):::syms(body)
    case ArrayFilter(a, x, body) => syms(a):::syms(body)
    case ArrayGroupBy(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case ArrayMap(a, x, body) => x :: effectSyms(body)
    case ArrayFilter(a, x, body) => x :: effectSyms(body)
    case ArrayGroupBy(a, x, body) => x::effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayFilter(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayGroupBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait ArrayOpsExpOpt extends ArrayOpsExp {


  override def array_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assigment at index n, just return the last value assigned
      val vs = x.asInstanceOf[Sym[Array[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(0)) // FIXME: bounds check!
        case Def(Reflect(ArrayUpdate(`x`, `n`, rhs: Exp[T]), _, _)) => Some(rhs)
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.array_apply(x,n))
    } else {
      super.array_apply(x,n)
    }
  }

  override def array_update[T:Manifest](x: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit pos: SourceContext) = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assigment at index n with the same value, just do nothing
      val vs = x.asInstanceOf[Sym[Array[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(())) // FIXME: bounds check!
        case Def(Reflect(ArrayUpdate(`x`, `n`, `y`), _, _)) => Some(Const(()))
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.array_update(x,n,y))
    } else {
      super.array_update(x,n,y)
    }
  }



}




trait BaseGenArrayOps extends GenericNestedCodegen {
  val IR: ArrayOpsExp
  import IR._

}

trait ScalaGenArrayOps extends BaseGenArrayOps with ScalaGenBase {
  val IR: ArrayOpsExp
  import IR._

  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n, sType) => {
        val arrType = if (quote(sType) != "\"\"") quote(sType).replaceAll("\"","") else remap(a.m)
        emitValDef(sym, src"new Array[$arrType]($n)")
    }
    case e@ArrayFromSeq(xs) => {
      emitData(sym, xs)
      emitValDef(sym,
        if(xs.size > ARRAY_LITERAL_MAX_SIZE) {
          /* def append(i: Int) = {
            val start = i*ARRAY_LITERAL_MAX_SIZE
            val end = Math.min((i+1)*ARRAY_LITERAL_MAX_SIZE, xs.size)
            val size = end - start
            "def x" + sym.id + "_" + i + "=Array(" + (start until end).map{xs(_)} + ")\nArray.copy(x" + sym.id + "_" + i + ",0,buf," + start + "," + size + ")\n"
          }
          val numBlocks = Math.ceil(xs.size / ARRAY_LITERAL_MAX_SIZE).intValue
          "{val buf=new Array[" + remap(e.mt) + "](" + xs.size + ")\n" + ((0 until numBlocks).map(append)).mkString("\n") + "buf}" */
          "{import scala.io.Source;(Source.fromFile(\"" + symDataPath(sym) + "\").getLines.map{Integer.parseInt(_)}).toArray}"
        }
        else {
          src"Array($xs)"
        }
      )
    }
    case ArrayApply(x,n) => emitValDef(sym, src"$x($n)")
    case ArrayUpdate(x,n,y) => emitAssignment(sym, src"$x($n)", quote(y))
    case ArrayLength(x) => emitValDef(sym, src"$x.length")
    case ArrayForeach(a,x,block) => 
      emitValDef(sym, src"$a.foreach{")    
      gen"""$x => 
           |${nestedBlock(block)}
           |$block
           |}"""
    case ArrayCopy(src,srcPos,dest,destPos,len) => emitValDef(sym, src"System.arraycopy($src,$srcPos,$dest,$destPos,$len)")
    case a@ArraySort(x) => 
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
        gen"""{
             |val d = new Array[${a.m}]($x.length)
             |System.arraycopy($x, 0, d, 0, $x.length)
             |scala.util.Sorting.quickSort(d)
             |d
             |}"""
      }
      emitValDef(sym, strWriter.toString)
    case n@ArrayMap(a,x,blk) => 
      val strWriter = new java.io.StringWriter
      val localStream = new PrintWriter(strWriter);
      withStream(localStream) {
      //stream.println("/* workaround for refinedManifest problem */")
        gen"""{
             |val out = ${n.array}
             |val in = $a
             |var i = 0
             |while (i < in.length) {
             |val $x = in(i)
             |${nestedBlock(blk)}
             |out(i) = $blk
             |i += 1
             |}
             |out
             |}"""
      }
      emitValDef(sym, strWriter.toString)
    
      // stream.println("val " + quote(sym) + " = " + quote(a) + ".map{")
      // stream.println(quote(x) + " => ")
      // emitBlock(blk)
      // stream.println(quote(getBlockResult(blk)))
      // stream.println("}")  
	case ArrayFilter(a,x,blk) =>
		stream.println("// FILTER")
		emitValDef(sym, quote(a) + ".filter(" + quote(x) + "=> {")
		emitBlock(blk)
		emitBlockResult(blk)
		stream.println("})") 
		stream.println("// END OF FILTER")
	case ArrayGroupBy(a,x,blk) =>
		stream.println("// GROUPBY")
		emitValDef(sym, quote(a) + ".groupBy(" + quote(x) + "=> {")
		emitBlock(blk)
		emitBlockResult(blk)
		stream.println("})") 
		stream.println("// END OF GROUPBY")
	case ArraySum(a) => 
		stream.println("// SUM")
		emitValDef(sym, quote(a) + ".sum")
		stream.println("// END OF SUM")
    case ArrayToSeq(a) => emitValDef(sym, src"$a.toSeq")
    case ArrayZip(a,a2) => emitValDef(sym, src"$a zip $a2") 
    case ArrayMkString(a, del) => 
      if (del != unit(""))
        emitValDef(sym, src"$a.mkString($del)")
      else
        emitValDef(sym, src"$a.mkString")	
    case ArrayCorresponds(a,a2) => emitValDef(sym, src"$a.corresponds($a2){_==_}") 
    case ArrayMutable(x) => emitValDef(sym, src"$x //array made mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenArrayOps extends BaseGenArrayOps with CLikeGenBase {
  val IR: ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case ArrayLength(x) => emitValDef(sym, src"$x.length")
        case ArrayApply(x,n) => emitValDef(sym, src"$x.apply($n)")
        case ArrayUpdate(x,n,y) => stream.println(src"$x.update($n,$y);")
        case ArrayMutable(x) => emitValDef(sym, src"$x //array made mutable")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenArrayOps extends CudaGenBase with CLikeGenArrayOps
trait OpenCLGenArrayOps extends OpenCLGenBase with CLikeGenArrayOps
trait CGenArrayOps extends CGenBase with CLikeGenArrayOps {
	val IR: ArrayOpsExp
  	import IR._

  	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    	rhs match {
    		case a@ArrayNew(n, sType) => {
        		val arrType = if (quote(sType) != "\"\"") remapInternal(quote(sType).replaceAll("\"","")) else remap(a.m)
		        stream.println(arrType + "* " + quote(sym) + " = " + getMemoryAllocString(quote(n), arrType))
				// Simulate length
				stream.println("int " + quote(sym) + "Size = " + quote(n) + ";")
			}
			case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
        	case ArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
    		case ArrayMkString(a, del) => stream.println("TODO: IMPLEMENT ARRAY MKSTRING")
        	case _ => super.emitNode(sym, rhs)
		}
	}
}
