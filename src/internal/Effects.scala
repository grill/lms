package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import util.GraphUtil
import scala.collection.mutable
import scala.annotation.unchecked.uncheckedVariance

/**
 * Blocks are nodes which contain and represent inner nodes (statements)
 */

trait Blocks extends Expressions {

  case class Block[+T](val res: Exp[T]) { def tp: Manifest[T @uncheckedVariance] = res.tp } // variance ...

  def blocks(e: Any): List[Block[Any]] = e match {
    case b: Block[Any] => List(b)
    case p: Product => p.productIterator.toList.flatMap(blocks(_))
    case _ => Nil
  }

}

/*
 * This trait introduces new kinds of nodes to store information about effects in the graph
 */

trait Effects extends Expressions with Blocks with Utils {

  // TODO: transform over Summary currently lives in common/Base.scala. move it here?
  // --- context

  type State = List[Exp[Any]] // TODO: maybe use TP instead to save lookup

  //global variable containing symbols which are currently in scope
  var context: State = _

  // --- class defs

  /**
    * Captures side-effectful statements in the context
    * Parameters:
    *   x       ... node representing statement
    *   summary ... information about effects of statement
    *   deps    ... dependencies of statement (list of symbols which have to be before x in the schedule)
    */
  case class Reflect[+A](x:Def[A], summary: Summary, deps: List[Exp[Any]]) extends Def[A]
  /*
   * opens a context (a block)
   * Parameters:
   *   x       ... symbol representing inner statements
   *   summary ... information about effects of the inner statements
   *   effects ... list of symbols of effectful statements
   */
  case class Reify[A](x: Exp[A], summary: Summary, effects: List[Exp[Any]]) extends Def[A]

  // --- summary of a reflect node

  case class Summary(
    val maySimple: Boolean,
    val mstSimple: Boolean,

    //if effect is global (affects all statements in scope)
    val mayGlobal: Boolean,
    val mstGlobal: Boolean,

    //if a mutable object was initialized
    val resAlloc: Boolean,
    
    //list of tuples containing a list of parent symbols (-> nested datastructure) and a list
    //of statements representing 
    val readStmnts: List[(List[Sym[Any]], List[Def[Any]])],

    //symbol representing a object reference (-> aliasing)
    val aliasRep: List[Sym[Any]],

    //list of symbols from which are read
    val mayRead: List[Sym[Any]],
    val mstRead: List[Sym[Any]],

    //list of symbols to which is written to
    val mayWrite: List[Sym[Any]],
    val mstWrite: List[Sym[Any]]
  )

  //often used summaries
  def Pure() = new Summary(false,false,false,false,false,Nil, Nil, Nil,Nil,Nil,Nil)
  def Simple() = new Summary(true,true,false,false,false,Nil, Nil, Nil,Nil,Nil,Nil)
  def Global() = new Summary(false,false,true,true,false,Nil, Nil, Nil,Nil,Nil,Nil)
  def Alloc() = new Summary(false,false,false,false,true,Nil, Nil, Nil,Nil,Nil,Nil)

  //summary constructors introduced for data structures with nested objects
  def ReadMutable(v: List[Sym[Any]], a: List[Def[Any]], r: List[Sym[Any]]) = new Summary(false,false,false,false,false,List((v.distinct,a.distinct)),r.distinct,v.distinct,v.distinct,Nil,Nil)
  def WriteMutable(v: List[Sym[Any]], r: List[Sym[Any]]) = new Summary(false,false,false,false,false,Nil,r.distinct,Nil,Nil,v.distinct,v.distinct)

  //summary constructors for simple data structures containing only nested primitives
  def Read(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,Nil,Nil,v.distinct,v.distinct,Nil,Nil)
  def Write(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,Nil,Nil,Nil,Nil,v.distinct,v.distinct)

  //check if summary is a read mutable
  def isReadMutable(u: Summary): Boolean = !u.readStmnts.isEmpty
  def mayRead(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayRead contains _)
  def mayWrite(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayWrite contains _)
  def maySimple(u: Summary): Boolean = u.mayGlobal || u.maySimple

  //check if the summary is from a statement which initialized an object
  def mustMutable(u: Summary): Boolean = u.resAlloc
  
  //check if the summary is from a statement which is pure
  def mustPure(u: Summary): Boolean = u == Pure().copy(readStmnts=u.readStmnts, aliasRep=u.aliasRep)

  def mustOnlyAlloc(u: Summary): Boolean = u == Alloc() // only has a resource allocation
  def mustOnlyRead(u: Summary): Boolean = u == Pure().copy(mayRead=u.mayRead, mstRead=u.mstRead, readStmnts=u.readStmnts, aliasRep=u.aliasRep) // only reads allowed

  //check if the summary is from a statement which is idempotent (used for CSE)
  def mustIdempotent(u: Summary): Boolean = mustOnlyRead(u) // currently only reads are treated as idempotent

  // --- methods for combining summaries

  def infix_orElse(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple && v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal && v.mstGlobal,
    //TODO: think about nested mutability here

    false, //u.resAlloc && v.resAlloc, <--- if/then/else will not be mutable!
    (u.readStmnts ++ v.readStmnts).distinct,//TODO: checkThis
    //(u.acctype ++ v.acctype).distinct,//TODO: checkThis
    (u.aliasRep ++ v.aliasRep).distinct,//TODO: checkThis
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead intersect v.mstRead),
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite intersect v.mstWrite)
  )

  def infix_andAlso(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    u.resAlloc || v.resAlloc,
    (u.readStmnts ++ v.readStmnts).distinct,//TODO: checkThis
    //(u.acctype ++ v.acctype).distinct,//TODO: checkThis
    (u.aliasRep ++ v.aliasRep).distinct,//TODO: checkThis
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )

  def infix_andThen(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    v.resAlloc,
    (u.readStmnts ++ v.readStmnts).distinct,//TODO: checkThis
    //(u.acctype ++ v.acctype).distinct,//TODO: checkThis
    (u.aliasRep ++ v.aliasRep).distinct,//TODO: checkThis
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )

  def infix_star(u: Summary) = Pure() orElse u // any number of repetitions, including 0


  def summarizeEffects(e: Block[Any]) = e match {
    case Block(Def(Reify(_,u,_))) => u
    //case Def(Reflect(_,u,_)) => u
    case _ => Pure()
  }



  // --- reflect helpers

  override def syms(e: Any): List[Sym[Any]] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.syms(e)
  }

  override def rsyms[T](e: Any)(f: Any => List[T]): List[T] = e match { // stack overflow ...
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.rsyms(e)(f)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.symsFreq(e)
  }

  override def effectSyms(x: Any): List[Sym[Any]] = x match {
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Sym[Any]]]
    case _ => super.effectSyms(x)
  }

  def readSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    case Reify(x, u, es) =>
      // in general: the result of a block is not read but passed through.
      // FIXME this piece of logic is not clear. is it a special case for unit??
      // it looks like this was introduced to prevent the Reify to be reflected
      // if x is a mutable object defined within the block.
      // TODO the globalMutableSyms part was added later (June 2012) -- make sure it does the right thing
      if ((es contains x) || (globalMutableSyms contains x)) Nil
      else readSyms(x)
    case s: Sym[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(readSyms(_))
    case _ => Nil
  }

  /*
    decisions to be made:
    1) does alias imply read? or are they separate?
    2) use a data structure to track transitive aliasing or recompute always?
  */


  /*

  the methods below define the sharing relation between the
  result of an operation and its arguments.

  how do i use them? what do i need to return?

  assume an operation foo:

  y = Foo(x)

  x should be returned in the following cases:

  x in aliasSyms(y)      if y = x      // if then else
  x in containSyms(y)    if *y = x     // array update
  x in extractSyms(y)    if y = *x     // array apply
  x in copySyms(y)       if *y = *x    // array clone

  y = x is to be understood as "y may be equal to x"
  *y = x as "dereferencing y (at some index) may return x"
  etc.

  */

  def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => aliasSyms(x)
    case Reify(x, u, es) => syms(x)
    case s: Sym[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(aliasSyms(_))
    case _ => Nil
  }

  def containSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => containSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(containSyms(_))
    case _ => Nil
  }

  def extractSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => extractSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(extractSyms(_))
    case _ => Nil
  }

  def copySyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => copySyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(copySyms(_))
    case _ => Nil
  }


  def isPrimitiveType[T](m: Manifest[T]) = m.toString match {
    case "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Boolean" | "Unit" => true
    case _ => false
  }

/*
  def allTransitiveAliases(start: Any): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] = {
      val st1 = st filterNot (s => isPrimitiveType(s.tp))
      globalDefs.filter(st1 contains _.sym)
    }
    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(aliasSyms(start)), t => deps(aliasSyms(t.rhs))).flatten.reverse
  }
*/

  def noPrim(sm: List[Sym[Any]]): List[Sym[Any]] = sm.filterNot(s=>isPrimitiveType(s.tp))

  /*
   TODO: switch back to graph based formulation -- this will not work for circular deps
  */

  val shallowAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]
  val deepAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]
  val allAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]

  def utilLoadStm[T](s: Sym[T]) = if (!isPrimitiveType(s.tp)) /*globalDefs.filter{e => e.lhs contains s}*/ findDefinition(s).toList else Nil
  def utilLoadStms(s: List[Sym[Any]]) = s.flatMap(utilLoadStm)
  def utilLoadSym[T](s: Sym[T]) = utilLoadStm(s).map(_.rhs)

  def shallowAliases(start: Any): List[Sym[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => a::shallowAliasCache.getOrElseUpdate(a, shallowAliases(utilLoadSym(a))) }
    val extract = noPrim(extractSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    //println("shallowAliases("+start+") = "+alias+" ++ "+extract)
    (alias ++ extract).distinct
  }

  def deepAliases(start: Any): List[Sym[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val copy = noPrim(copySyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val contain = noPrim(containSyms(start)) flatMap { a => a::allAliasCache.getOrElseUpdate(a, allAliases(utilLoadSym(a))) }
    //println("aliasSyms("+start+") = "+aliasSyms(start) + "/" + noPrim(aliasSyms(start)))
    //println("copySyms("+start+") = "+copySyms(start) + "/" + noPrim(copySyms(start)))
    //println("containSyms("+start+") = "+containSyms(start) + "/" + noPrim(containSyms(start)))
    //println("deepAliases("+start+") = "+alias+" ++ "+copy+" ++ "+contain)
    (alias ++ copy ++ contain).distinct
  }


  def allAliases(start: Any): List[Sym[Any]] = {
    val r = (shallowAliases(start) ++ deepAliases(start)).distinct
    //printdbg("all aliases of " + start + ": " + r.mkString(", "))
    r
  }

  //def allTransitiveAliases(start: Any): List[Stm] = utilLoadStms(allAliases(start))
  //def transitiveAliases(start: List[Sym[Any]]): List[Stm] = start.flatMap(utilLoadSymTP)

  // TODO possible optimization: a mutable object never aliases another mutable object, so its inputs need not be followed

  def mutableTransitiveAliases(s: Any) = {
    val aliases = allAliases(s)
    val bareMutableSyms = aliases filter { o => globalMutableSyms.contains(o) }
    val definedMutableSyms = utilLoadStms(aliases) collect { case TP(s2, Reflect(_, u, _)) /*if mustMutable(u) || isReadMutable(u) */=> s2 }
    bareMutableSyms ++ definedMutableSyms
  }


  def getActuallyReadSyms[A](d: Def[A]) = {
    val bound = boundSyms(d)
    val r = readSyms(d).map{case Def(Reify(x,_,_)) => x case x => x} filterNot (bound contains _)
    //if (d.isInstanceOf[Reify[Any]] && r.nonEmpty) {
    //  println("** actually read: "+readSyms(d)+"\\"+bound+"="+r)
    //  println("** transitive shallow: " + shallowAliases(r))
    //  println("** transitive deep: " + deepAliases(r))
    //}
    r
  }

  def readMutableData[A](d: Def[A]) = {
    val bound = boundSyms(d)
    mutableTransitiveAliases(getActuallyReadSyms(d)) filterNot (bound contains _)
  }

  // --- reflect

  // REMARK: making toAtom context-dependent is quite a departure from the
  // earlier design. there are a number of implications especially for mirroring.

  /*
    wrapping reads in a reflect can also have an unfortunate effect on rewritings.
    consider
      val a = ...       // mutable
      val b = a.foo     // usually Foo(a) but now Reflect(Foo(a))
      val c = b.costly  // costly(Foo(a)) would simplify to Cheap(a),
                        // but this ends up as Reflect(Costly(Reflect(Foo(a)))) instead of Reflect(Cheap(a))

    of course this is unsafe in general but there might be cases that are definitely save.
  */

  protected override implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
  /*
    are we depending on a variable or mutable object? then we need to be serialized -> effect

    the call chain goes like this:

      toAtom
      reflectEffect(Pure())      // figure out dependencies on mutable objects
      reflectEffectInternal(u)   // extended summary Pure() -> u
        super.toAtom             // if summary is still pure
        createReflectDefinition  // if summary is not pure
  */

    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    if (Config.verbosity == 2 && manifest[T] == manifest[Any])
      printlog("warning: possible missing mtype call - toAtom with Def of type Any " + d)

    // AKS NOTE: this was removed on 6/27/12, but it is still a problem in OptiML apps without it,
    // so I'm putting it back until we can get it resolved properly.
    d match {
      case Reify(x,_,_) =>
        // aks: this became a problem after adding global mutable vars to the read deps list. what is the proper way of handling this?
        // specifically, if we return the reified version of a mutable bound var, we get a Reflect(Reify(..)) error, e.g. mutable Sum
        // printlog("ignoring read of Reify(): " + d)
        super.toAtom(d)
      case _ => reflectEffect(d, Pure())
    }
    // reflectEffect(d, Pure())
  }

  def reflectMirrored[A:Manifest](zd: Reflect[A]): Exp[A] = {
    checkContext()
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    if (manifest[A] == manifest[Any]) printlog("warning: possible missing mtype call - reflectMirrored with Def of type Any: " + zd)
    context.filter { case Def(d) if d == zd => true case _ => false }.reverse match {
      //case z::_ => z.asInstanceOf[Exp[A]]  -- unsafe: we don't have a tight context, so we might pick one from a flattened subcontext
      case _ => createReflectDefinition(fresh[A], zd)
    }
  }

  def checkIllegalSharing(z: Exp[Any], mutableAliases: List[Sym[Any]]) {
    if (mutableAliases.nonEmpty && Config.verbosity >= 1) {
      val zd = z match { case Def(zd) => zd }
      printerr("error: illegal sharing of mutable objects " + mutableAliases.mkString(", "))
      printerr("at " + z + "=" + zd)
      printsrc("in " + quotePos(z))
    }
  }

  def isWritableSym[A](w: Sym[A]): Boolean = {
    findDefinition(w) match {
      case Some(TP(_, Reflect(_, u, _))) if mustMutable(u) => true // ok
      case o => globalMutableSyms.contains(w)
    }
  }


  var globalMutableSyms: List[Sym[Any]] = Nil

  def reflectMutableSym[A](s: Sym[A]): Sym[A] = {
    assert(findDefinition(s).isEmpty)
    globalMutableSyms = globalMutableSyms :+ s
    s
  }

  def reflectMutable[A:Manifest](d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val z = reflectEffect(d, Alloc())

    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  
  /**
   * new effect system
   **/

  class MutableStatement[A:Manifest]
  case class MutableAtom[A:Manifest](val a: Exp[A]) extends MutableStatement[A]
  case class MutableDef[A:Manifest](val s: Sym[A], val d: Reflect[A]) extends MutableStatement[A]

  def reflectMutableWithPars[A:Manifest](write0: (Exp[Any]=>Exp[Any])*)(read0: Exp[Any]*)(d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    //println("Init: " + read0 + "Def: " + d)

    //get all alias reps from written exprs --> find unique alias
    val repsR = (read0.toList.flatMap { case x: Sym[Any] => Some(x)
                                        case _ => None
    }).flatMap { x => findDefinition(x) match {
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(y, u, _))) => /*println("haa: " + y + " uu: " + u);*/ u.aliasRep
      case _ => None
    }}

    val z = reflectEffectMutableInternal(d, Alloc() andAlso WriteMutable(Nil, repsR), Nil) match {
      case MutableAtom(a) => a
      case MutableDef(zn, Reflect(x,u,w)) =>
        val writes = write0.toList.map { x => x(zn) }.toList.asInstanceOf[List[Sym[Any]]]
        //val writes = Nil
        createReflectDefinition(zn, Reflect(x, u.copy(mayWrite=writes, mstWrite=writes), w))
    }

    /*println("ReflectInitMutable: " + (findDefinition(z.asInstanceOf[Sym[Any]]) match {
      //TODO: either write to get or alloc ??
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(_, u, deps))) => "" + u.aliasRep + " deps: " + deps
      case _ => ""
    }))*/

    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def equalReadOperation(currentNode: List[Def[Any]], otherNode: List[Def[Any]]) =
      !currentNode.map(_.getClass).intersect(otherNode.map(_.getClass)).isEmpty

  def reflectReadMutable[A:Manifest](parent0: Exp[Any]*)(d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    //println("reflectReadMutable: " + parent0 + " Def: " + d)
    val parent = parent0.toList.asInstanceOf[List[Sym[Any]]]


    //returns all aliases of the parent reference
    val repsOther = parent flatMap { findDefinition(_) match {
        case Some(TP(_, Reflect(_, u, _))) => u.aliasRep
        case _ => List()
    }}

    //TODO: optimize look up last read and use representative
    //TODO: if not there look at the aliases of parents and try to find a representative there
    //Pseudo code: look through all parent references and their alias representatives
    //and see if there have been any equal reads
    //if yes -> use symbol as representative
    //if no -> return List()

    val pp = List(((parent ++ repsOther).toSet.toList, List(d)))

    val prevWrites = (context filter ({ case e@Def(Reflect(x, u, _)) =>
          //1. finde nodes which have the same parent and the same read operation
          u.readStmnts.exists({
            uRF => pp.exists( rF =>
              !rF._1.intersect(uRF._1).isEmpty &&
               equalReadOperation(rF._2, uRF._2)
          //2. find all symbols of statements writing to at least one of the prev. yielded symbols
          )})   }) flatMap ({ e =>
            context filter { case Def(Reflect(_, u, _)) => u.mayWrite.contains(e) }
          })).asInstanceOf[List[Sym[Any]]]

    val repsW  = if(prevWrites.isEmpty) {
      //current symbol is representative
      //or check via parent aliases if anyone else is the representative
      
      //List()
      repsOther
    } else {
      findDefinition(prevWrites.last) match {
        case Some(TP(_, Reflect(_, u, _))) => u.aliasRep
        case _ => List()
        //TODO: what if multiple node can read the same data    
        //TODO: move CSE with dep info from effect tracking here
      }
    }

    //println("ReadMutable: RW: " + repsW + " prevW: " + prevWrites + " repsOther: " + repsOther)
    val z = reflectEffectMutableInternal(d, ReadMutable((parent++repsOther).toSet.toList, List(d), repsW), prevWrites) match {
      case MutableAtom(a) => a
      case MutableDef(zn, zd) => createReflectDefinition(zn, zd)
    }

   /*println("ReflectReadMutable: " + (findDefinition(z.asInstanceOf[Sym[Any]]) match {
      //TODO: either write to get or alloc ??
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(_, u, deps))) => "" + u.aliasRep + " deps: " + deps
      case _ => ""
    }))*/

    val mutableAliases = mutableTransitiveAliases(d) filterNot (parent contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectWriteMutable[A:Manifest](write0: Exp[Any]*)(read0: Exp[Any]*)(d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    //println("Write: " + write0 + "Def: " + d)
    val write = write0.toList.asInstanceOf[List[Sym[Any]]]

    //get all previous alias reps --> contains
    val repsW = write.flatMap { x => findDefinition(x) match {
      //TODO: either write to get or alloc ??
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(_, u, _))) => u.aliasRep
      case _ => None
    }}

    //TODO: remove the old ones when reassign happens

    //get all alias reps from written exprs --> find unique alias
    val repsR = (read0.toList.flatMap { case x: Sym[Any] => Some(x)
                                        case _ => None
    }).flatMap { x => findDefinition(x) match {
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(_, u, _))) => u.aliasRep
      case _ => None
    }}

    //TODO: impl. loop problem detection:
    //if( inLoop && repsW.contains( mutable which was defined before loop ) && repsR.contains( mutable which was defined before loop ) )

    //println("WriteMutable: RW: " + repsW + " RR: " + repsR)
    val z = reflectEffectMutableInternal(d, WriteMutable(write, repsR ++ repsW), Nil) match {
      case MutableAtom(a) => a
      case MutableDef(zn, zd) => createReflectDefinition(zn, zd)
    }



    /*println("AliasRep: " + (findDefinition(z.asInstanceOf[Sym[Any]]) match {
      //TODO: either write to get or alloc ??
      //case Some(TP(_, Reflect(_, u, _))) if (mustOnlyAlloc(u)) => List(x)
      case Some(TP(_, Reflect(_, u, deps))) => "" + u.aliasRep + " deps: " + deps
      case _ => ""
    }))*/

    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectEffectMutableInternal[A:Manifest](x: Def[A], s: Summary, prevWrites: List[Sym[Any]])(implicit pos: SourceContext): MutableStatement[A] = {
    val mutableInputs = readMutableData(x)
    val u = s andAlso Read(mutableInputs) // will call super.toAtom if mutableInput.isEmpty

    //first statement in scope
    if (context == null) {
      context = Nil
      if (mustPure(u))
        MutableAtom(super.toAtom(x))
      else {
         //statement has effects
         val z = fresh[A]
         val zd = if(mustOnlyAlloc(u) || (mustIdempotent(u) && u.aliasRep.isEmpty))
            Reflect(x,u.copy(aliasRep=List(z)),null)
        else
            Reflect(x,u,null)
        MutableDef(z, zd)
        //createReflectDefinition(z, zd)
      }

    } else if (mustPure(u)) {
      MutableAtom(super.toAtom(x))
    } else {
      checkContext()
      // NOTE: reflecting mutable stuff *during mirroring* doesn't work right now.

      //calculate dependencies
      val deps = if (u.mayGlobal) context else {
        val read = u.mayRead
        val write = u.mayWrite

        def canonic(xs: List[Exp[Any]]) = xs // TODO
        def canonicLinear(xs: List[Exp[Any]]) = xs.takeRight(1)

        // the mayPrune flag is for test8-speculative4: with pruning on, the 'previous iteration'
        // dummy is moved out of the loop. this is not per se a problem -- need to look some more into it.

        val readDeps = if (read.isEmpty) Nil else context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
        val softWriteDeps = if (write.isEmpty) Nil else context filter { case e@Def(Reflect(_, u, _)) => mayRead(u, write) }
        val writeDeps = if (write.isEmpty) Nil else context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || write.contains(e) }
        val simpleDeps = if (!u.maySimple) Nil else context filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
        val globalDeps = context filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }
        // TODO: write-on-read deps should be weak
        // TODO: optimize!!
        val allDeps = canonic(readDeps ++ softWriteDeps ++ writeDeps ++ prevWrites ++ canonicLinear(simpleDeps) ++ canonicLinear(globalDeps))
        context filter (allDeps contains _)
      }

      var zd = Reflect(x,u,deps)

      //println("ref: " + mustIdempotent(u) + " dep: " + deps + " x: " + x)

      //CSE
      if (mustIdempotent(u)) {  
        context find { case Def(Reflect(x1, u1, d1)) => x1 == x && deps == d1} map { x => MutableAtom(x.asInstanceOf[Exp[A]]) } getOrElse {
          val z = fresh[A]
          val zd = if(mustOnlyAlloc(u) || u.aliasRep.isEmpty)
            Reflect(x,u.copy(aliasRep=List(z)),deps)
          else
            Reflect(x,u,deps)

          if (!x.toString.startsWith("ReadVar")) { // supress output for ReadVar
            printlog("promoting to effect: " + z + "=" + zd)
            for (w <- u.mayRead)
              printlog("depends on  " + w)
            }
            MutableDef(z,zd)
            //createReflectDefinition(z, zd)
          }

      //no CSE, creation of new nodes
      } else {
        val z = fresh[A](List(pos))
        zd = if(mustOnlyAlloc(u) || (mustIdempotent(u) && u.aliasRep.isEmpty))
            Reflect(x,u.copy(aliasRep=List(z)),deps)
        else
            Reflect(x,u,deps)
        // make sure all writes go to allocs
        if (Config.verbosity >= 1) {
          for (w <- u.mayWrite if !isWritableSym(w)) {
            printerr("error: write to non-mutable " + w + " -> " + findDefinition(w))
            printerr("at " + z + "=" + zd)
            printsrc("in " + quotePos(z))
          }
        }
        MutableDef(z, zd)
        //createReflectDefinition(z, zd)
      }
    }
  }

  /**
   * old effect system
   **/


  //effect tracking on data structures with underlying immutables
  def reflectWrite[A:Manifest](write0: Exp[Any]*)(d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Sym[Any]]] // should check...


    val z = reflectEffect(d, Write(write))

    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectEffect[A:Manifest](x: Def[A])(implicit pos: SourceContext): Exp[A] = reflectEffect(x, Simple()) // simple effect (serialized with respect to other simples)

  def reflectEffect[A:Manifest](d: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] = {
    // are we depending on a variable? then we need to be serialized -> effect
    val mutableInputs = readMutableData(d)
    reflectEffectInternal(d, u andAlso Read(mutableInputs)) // will call super.toAtom if mutableInput.isEmpty
  }

  def reflectEffectInternal[A:Manifest](x: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] = {
    /*
     * We want to handle the case where a variable is first initialized to null,
     * and later initialized to its actual value (also see variables.scala).
     * This is the case with local state in the DBMS system of DATA lab. There,
     * when the var is initialized to null, the context is null, since it is
     * _outside_ any compiled method (we do not lift everything in this
     * system!). Initial solution was to override the case where context ==
     * null, however this breaks test4-fac4 (reflectEffect when not needed).
     */
    if (context == null) {
      context = Nil
	    if (mustPure(u))
         super.toAtom(x)
	    else {
       	val z = fresh[A]
        val zd = if(mustOnlyAlloc(u) || (mustIdempotent(u) && u.aliasRep.isEmpty))
            Reflect(x,u.copy(aliasRep=List(z)),null)
        else
            Reflect(x,u,null)
        createReflectDefinition(z, zd)
	    }
    } else if (mustPure(u))
      super.toAtom(x)
    else {
      checkContext()
      // NOTE: reflecting mutable stuff *during mirroring* doesn't work right now.

      //effect tracking
      val deps = calculateDependencies(u)
      var zd = Reflect(x,u,deps)

      //CSE
      if (mustIdempotent(u)) {  

              context find { case Def(x) => x == zd} map { _.asInstanceOf[Exp[A]] } getOrElse {
    //        findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse TODO: turn around and look at context first??
              val z = fresh[A]
              zd = if(mustOnlyAlloc(u) || (mustIdempotent(u) && u.aliasRep.isEmpty))
                  Reflect(x,u.copy(aliasRep=List(z)),deps)
              else
                  Reflect(x,u,deps)

              if (!x.toString.startsWith("ReadVar")) { // supress output for ReadVar
                printlog("promoting to effect: " + z + "=" + zd)
                for (w <- u.mayRead)
                  printlog("depends on  " + w)
              }
              createReflectDefinition(z, zd)
            }
      } else {
        val z = fresh[A](List(pos))
        zd = if(mustOnlyAlloc(u) || (mustIdempotent(u) && u.aliasRep.isEmpty))
            Reflect(x,u.copy(aliasRep=List(z)),deps)
        else
            Reflect(x,u,deps)
        // make sure all writes go to allocs
        if (Config.verbosity >= 1) {
	        for (w <- u.mayWrite if !isWritableSym(w)) {
    	      printerr("error: write to non-mutable " + w + " -> " + findDefinition(w))
        	  printerr("at " + z + "=" + zd)
	          printsrc("in " + quotePos(z))
    	    }
		    }
        // prevent sharing between mutable objects / disallow mutable escape for non read-only operations
        // make sure no mutable object becomes part of mutable result (in case of allocation)
        // or is written to another mutable object (in case of write)
        /*
          val a = mzeros(100)
          val b = zeros(100)
          val c = if (..) {
            a.update
            b
          } else {
            a
          }

          PROBLEM: the whole if expr has summary mayWrite=List(a), mstWrite=Nil and allAliases=List(a,b)

          what is the right thing?
          - mutableAliases \ mstWrite <-- first try, but maybe to restrictive?
          - mutableAliases \ mayWrite <-- too permissive?
          - something else?

        */
        createReflectDefinition(z, zd)
      }
    }
  }

  def calculateDependencies(u: Summary): State = {
    checkContext();
    calculateDependencies(context, u, true)
  }

  def calculateDependencies(scope: State, u: Summary, mayPrune: Boolean): State = {
    if (u.mayGlobal) scope else {
      val read = u.mayRead
      val write = u.mayWrite

      // TODO: in order to reduce the number of deps (need to traverse all those!)
      // we should only store those that are not transitively implied.
      // For simple effects, take the last one (implemented).
      // For mutations, take the last write to a particular mutable sym (TODO).

      def canonic(xs: List[Exp[Any]]) = xs // TODO
      def canonicLinear(xs: List[Exp[Any]]) = if (mayPrune) xs.takeRight(1) else xs

      // the mayPrune flag is for test8-speculative4: with pruning on, the 'previous iteration'
      // dummy is moved out of the loop. this is not per se a problem -- need to look some more into it.

      val readDeps = if (read.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
      val softWriteDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayRead(u, write) }
      val writeDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || write.contains(e) }
      val simpleDeps = if (!u.maySimple) Nil else scope filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
      val globalDeps = scope filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }

      // TODO: write-on-read deps should be weak
      // TODO: optimize!!
      val allDeps = canonic(readDeps ++ softWriteDeps ++ writeDeps ++ canonicLinear(simpleDeps) ++ canonicLinear(globalDeps))
      scope filter (allDeps contains _)
    }
  }

  def createReflectDefinition[A](s: Sym[A], x: Reflect[A]): Sym[A] = {
    x match {
      case Reflect(Reify(_,_,_),_,_) =>
        printerr("error: reflecting a reify node.")
        printerr("at " + s + "=" + x)
        printsrc("in " + quotePos(s))
      case _ => //ok
    }
    createDefinition(s, x)
    context :+= s
    s
  }

  def checkContext() {
    if (context == null)
      sys.error("uninitialized effect context: effectful statements may only be used within a reifyEffects { .. } block")
  }


  // --- reify

  def summarizeAll(es: List[Exp[Any]]): Summary = {
    // compute an *external* summary for a seq of nodes
    // don't report any reads/writes on data allocated within the block
    var u = Pure()
    var ux = u
    var allocs: List[Exp[Any]] = Nil
    def clean(xs: List[Sym[Any]]) = xs.filterNot(allocs contains _)
    for (s@Def(Reflect(_, u2, _)) <- es) {
      if (mustMutable(u2)) allocs ::= s
      u = u andThen (u2.copy(mayRead = clean(u2.mayRead), mstRead = clean(u2.mstRead),
              mayWrite = clean(u2.mayWrite), mstWrite = clean(u2.mstWrite)))
      ux = ux andThen u2
    }
    //if (ux != u) printdbg("** effect summary reduced from "+ux+" to" + u)
    u
  }

  def pruneContext(ctx: List[Exp[Any]]): List[Exp[Any]] = ctx // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }

  // reify the effects of an isolated block.
  // no assumptions about the current context remain valid.
  def reifyEffects[A:Manifest](block: => Exp[A]): Block[A] = {
    val save = context
    context = Nil

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)

    val deps = context
    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) Block(result) else Block(Reify(result, summary, pruneContext(deps))) // calls toAtom...
  }

  // reify the effects of a block that is executed 'here' (if it is executed at all).
  // all assumptions about the current context carry over unchanged.
  def reifyEffectsHere[A:Manifest](block: => Exp[A]): Block[A] = {
    val save = context
    if (save eq null)
      context = Nil

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)

    if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
      printerr("error: 'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)

    val deps = if (save eq null) context else context.drop(save.length)

    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) Block(result) else Block(Reify(result, summary, pruneContext(deps))) // calls toAtom...
  }

  // --- bookkeping

  override def reset = {
    shallowAliasCache.clear()
    deepAliasCache.clear()
    allAliasCache.clear()
    globalMutableSyms = Nil
    context = null
    super.reset
  }

}
