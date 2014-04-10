package scala.virtualization.lms
package common

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import java.util.Date
import java.text.SimpleDateFormat
import scala.virtualization.lms.internal.GenerationFailedException

/**
 * Lifter Classes for SimpleDateFormat
 */
trait SimpleDateFormatOps extends Base {

  class SimpleDateFormatOpsCls(x: Rep[SimpleDateFormat]) {
    def format(d: Rep[Date]): Rep[String] = sdfFormat(x, d)
    def parse(s: Rep[String]): Rep[Date] = sdfParse(x, s)
  }

  implicit def date2SimpleDateFormatOpsCls(x: Rep[SimpleDateFormat]): SimpleDateFormatOpsCls = new SimpleDateFormatOpsCls(x)
  def newSimpleDateFormat(format: Rep[String]): Rep[SimpleDateFormat]
  def sdfFormat(x: Rep[SimpleDateFormat], d: Rep[Date]): Rep[String]
  def sdfParse(x: Rep[SimpleDateFormat], s: Rep[String]): Rep[Date]
}

trait SimpleDateFormatExp extends SimpleDateFormatOps with BaseExp {

  case class SdfFormat(x: Exp[SimpleDateFormat], d: Exp[Date]) extends Def[String]
  case class SdfParse(x: Exp[SimpleDateFormat], s: Exp[String]) extends Def[Date]
  case class NewSimpleDateFormat(format: Exp[String]) extends Def[SimpleDateFormat]

  def newSimpleDateFormat(format: Exp[String]): Exp[SimpleDateFormat] = NewSimpleDateFormat(format)
  def sdfFormat(x: Exp[SimpleDateFormat], d: Exp[Date]) = SdfFormat(x, d)
  def sdfParse(x: Exp[SimpleDateFormat], s: Exp[String]) = SdfParse(x, s)
 
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewSimpleDateFormat(format) => newSimpleDateFormat(f(format))
    case SdfFormat(x, d) => sdfFormat(f(x), f(d))
    case SdfParse(x, s) => sdfParse(f(x), f(s))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait SimpleDateFormatExpOpt extends SimpleDateFormatExp {
}

trait ScalaGenSimpleDateFormat extends ScalaGenBase {
  val IR: SimpleDateFormatExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewSimpleDateFormat(fmt) => fmt match {
      case Const(theFormat) => staticFields += ("SimpleDateFormatOps."+sym.id -> "val %s = new java.text.SimpleDateFormat(\"%s\")".format("sdf"+sym.id, theFormat))
      case _ => emitValDef(sym, "new java.text.SimpleDateFormat(%s)".format(quote(fmt)))
    }
    case SdfFormat(x, d) => emitValDef(sym, "%s.format(%s)".format(quoteSimpleDateFormat(x),quote(d)));
    case SdfParse(x, s) => emitValDef(sym, "%s.parse(%s)".format(quoteSimpleDateFormat(x),quote(s)));
    case _ => super.emitNode(sym, rhs)
  }

  def quoteSimpleDateFormat(x: Exp[SimpleDateFormat]) = Def.unapply(x) match {
    case Some(NewSimpleDateFormat(Const(theFormat))) => 
      val sym = x.asInstanceOf[Sym[_]]
      staticFields += ("SimpleDateFormatOps."+sym.id -> "val %s = new java.text.SimpleDateFormat(\"%s\")".format("sdf"+sym.id, theFormat))
      "sdf"+sym.id
    case _ => quote(x)
  }
}

trait CLikeGenSimpleDateFormat extends CLikeGenBase {
  val IR: SimpleDateFormatExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewSimpleDateFormat(fmt) => throw new GenerationFailedException("CLikeGenSimpleDateFormat: SimpleDateFormatOps is not supported")
    case SdfFormat(x, d) => throw new GenerationFailedException("CLikeGenSimpleDateFormat: SimpleDateFormatOps is not supported")
    case SdfParse(x, s) => throw new GenerationFailedException("CLikeGenSimpleDateFormat: SimpleDateFormatOps is not supported")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenSimpleDateFormat extends CudaGenBase with CLikeGenSimpleDateFormat
trait OpenCLGenSimpleDateFormat extends OpenCLGenBase with CLikeGenSimpleDateFormat
trait CGenSimpleDateFormat extends CGenBase with CLikeGenSimpleDateFormat