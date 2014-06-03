package scala.virtualization.lms
package internal

import java.io.{File, FileWriter, PrintWriter}
import scala.virtualization.lms.util.ReflectionUtil
import scala.reflect.SourceContext

/**
 * ScalaConciseCodegen is just an extension to ScalaCodegen
 * which inlines expressions that are possible to inline,
 * instead of creating a new val-def for each of them, leading
 * to a more compact and concise code.
 *
 * @author Mohammad Dashti (mohammad.dashti@epfl.ch)
 */
trait CConciseCodegen extends CNestedCodegen { self =>
  val IR: ExtendedExpressions with Effects with LoweringTransform
  import IR._

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    val extra = if ((Config.sourceinfo < 2) || sym.pos.isEmpty) "" else {
      val context = sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }
    sym match {
      case s@Sym(n) => isVoidType(s.tp) match {
        case true => stream.println("" + rhs + extra)
        case false => if(s.possibleToInline || s.noReference) {
            stream.print("("+rhs+")")
          } else {
            stream.println("val " + quote(sym) + " = " + rhs + extra)
          }
      }
      case _ => stream.println("val " + quote(sym) + " = " + rhs + extra)
    }
  }

  override def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
//    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + rhs)
    stream.println("var " + quote(sym) + ":" + remap(sym.tp) + " = " + rhs)
  }

  override def emitAssignment(sym: Sym[Any], lhs: String, rhs: String): Unit = {
    // if(isVoidType(sym.tp)) {
      stream.println(lhs + " = " + rhs)
    // } else {
    //   emitValDef(sym, lhs + " = " + rhs)
    // }
  }

  override def emitForwardDef[A:Manifest](args: List[Manifest[_]], functionName: String, out: PrintWriter) = {
    out.println(remap(manifest[A])+" "+functionName+"("+args.map(a => remap(a)).mkString(", ")+");")
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => if(!sym.possibleToInline && sym.refCount > 0 /*for eliminating read-only effect-ful statements*/) emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  override def quote(x: Exp[Any], forcePrintSymbol: Boolean) : String = {
    def printSym(s: Sym[Any]): String = {
      if(s.possibleToInline || s.noReference) {
        Def.unapply(s) match {
          case Some(d: Def[Any]) => {
            val strWriter: java.io.StringWriter = new java.io.StringWriter;
            val stream = new PrintWriter(strWriter);
            withStream(stream) {
              emitNode(s, d)
            }
            strWriter.toString
          }
          case None => "x"+s.id
        }
      } else {
        "x"+s.id
      }
    }
    x match {
      case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
      case Const(c: Char) => "'"+c+"'"
      case Const(f: Float) => "%1.10f".format(f) + "f"
      case Const(l: Long) => l.toString + "L"
      case Const(null) => "null"
      case Const(z) => z.toString
      case s@Sym(n) => if (forcePrintSymbol) {
        printSym(s)
      } else {
        isVoidType(s.tp) match {
          case true => "(" + /*"x" + n +*/ ")"
          case false => printSym(s)
        }
      }
	  case null => "null"
      case _ => throw new RuntimeException("could not quote " + x)
    }
  }

  override def performTransformations[A:Manifest](body: Block[A]): Block[A] = {
    val transformedBody = super.performTransformations[A](body)
    val fixer = new SymMetaDataFixerTransform{ val IR: self.IR.type = self.IR }
    fixer.traverseBlock(transformedBody.asInstanceOf[fixer.Block[A]])
    transformedBody
  }

}
