package scala.virtualization.lms
package epfl
package test12

import common._
import internal._
import java.io.PrintWriter
import scala.reflect.SourceContext

class TestArrayOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test12-"

 /**
  * Traits needed for function code generation
  */

  trait MyCodeGen extends ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaCodeGenPkg with ScalaGenHashMap with ScalaConciseCodegen {
    override val IR: ScalaOpsPkgExp with HashMapArrOpsExp with OptionOpsExp with EntryOpsExp with ExtendedExpressions with Effects with LoweringTransform
    import IR._

    override def remap(s:String) = s match {
      case "scala.virtualization.lms.epfl.test12.Entry" => "Entry"
      case "scala.virtualization.lms.epfl.test12.HashMap" => "HashMap"
      case x => super.remap(x)
    }
  }

  trait MyOpsExp extends HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp with DSLBase
        with LiftPrimitives with LiftString with LiftVariables
        with ExtendedExpressions with Effects with LoweringTransform

  trait Impl extends MyOpsExp { self =>
    val codegen = new ScalaGenArrayOps with ScalaGenMiscOps with ScalaConciseCodegen
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: self.type = self }
  

    def emitAll(): Unit = {
      assert(codegen ne null) //careful about initialization order
      val stream = new PrintWriter(System.out)
      rec.foreach { case (k,x) =>
        stream.println("/* FILE: " + x.name + ".c */")
        //codegen.emitSource1(x.f, x.name, stream)(mtype(x.mA), mtype(x.mB))
        x match {
          case TopLevel1  (name, mA1, mB, f) => codegen.emitSource1(f, name, stream)(mtype(mA1), mtype(mB))
          case TopLevel2  (name, mA1, mA2, mB, f) => codegen.emitSource2(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mB))
          case TopLevel3  (name, mA1, mA2, mA3, mB, f) => codegen.emitSource3(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mB))
          case TopLevel4  (name, mA1, mA2, mA3, mA4, mB, f) => codegen.emitSource4(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mB))
          case TopLevel5  (name, mA1, mA2, mA3, mA4, mA5, mB, f) => codegen.emitSource5(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mB))
          case TopLevel6  (name, mA1, mA2, mA3, mA4, mA5, mA6, mB, f) => codegen.emitSource6(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mA6), mtype(mB))
          case TopLevel7  (name, mA1, mA2, mA3, mA4, mA5, mA6, mA7, mB, f) => codegen.emitSource7(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mA6), mtype(mA7), mtype(mB))
          case TopLevel8  (name, mA1, mA2, mA3, mA4, mA5, mA6, mA7, mA8, mB, f) => codegen.emitSource8(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mA6), mtype(mA7), mtype(mA8), mtype(mB))
          case TopLevel9  (name, mA1, mA2, mA3, mA4, mA5, mA6, mA7, mA8, mA9, mB, f) => codegen.emitSource9(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mA6), mtype(mA7), mtype(mA8), mtype(mA9), mtype(mB))
          case TopLevel10 (name, mA1, mA2, mA3, mA4, mA5, mA6, mA7, mA8, mA9, mA10, mB, f) => codegen.emitSource10(f, name, stream)(mtype(mA1), mtype(mA2), mtype(mA3), mtype(mA4), mtype(mA5), mtype(mA6), mtype(mA7), mtype(mA8), mtype(mA9), mtype(mA10), mtype(mB))
        }
      }
    }
    emitAll()
  }


 /**
  * Tests for hash map implementation 
  */

  it("testFunMap") {
    withOutFile(prefix+"hash-map-fun-map") {
      trait Prog extends MyOpsExp {
        toplevel1("main") { a: Rep[HashMap[Int,Int]] =>
          println(a.size)
          a.setSize(3)
          println(a.size)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"hash-map-fun-map")
  }

  it("testGetAndSetSize") {
    withOutFile(prefix+"hash-map-get-and-set-size") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          println(a.size)
          a.setSize(3)
          println(a.size)
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndSetSize", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-get-and-set-size")
  }

  it("testComplexNested") {
    withOutFile(prefix+"hash-map-complex-nested") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, HashMap[Int,Int]](1)
          val a1 = HashMap[Int,Int](1)
          val a2 = HashMap[Int,Int](1)

          a.update(1, a1)
          a.update(2, a2)

          a1.update(1, 1)
          a(1).get().update(1, 2)

          a.foreach({x => println(x); x.getValue().foreach({println(_)}) })
          a.foreach({x => a.update(x.getKey(), unit(null).AsInstanceOf[HashMap[Int, Int]])  })
          a.foreach({x => println(x)})
          a.foreach({x => a -= x.getKey()})
          a.update(3, a1)
          a.foreach({x => println(x); x.getValue().foreach({println(_)}) })
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapComplexNested", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-complex-nested")
  }

  it("testComplex") {
    withOutFile(prefix+"hash-map-complex") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](unit(1))
          a.update(1, 2)
          a.update(2, 3)

          val v = a(1).get() + 1
          a.update(1, v)

          a.foreach({x => println(x)})
          a.foreach({x => a.update(x.getKey(), 0)})
          a.foreach({x => println(x)})
          a.foreach({x => a -= x.getKey()})
          a.update(3, 4)
          a.foreach({x => println(x)})
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapComplex", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-complex")
  }

  it("testGetAndUpdate") {
    withOutFile(prefix+"hash-map-get-and-update") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          println(a(1))
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdate", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-get-and-update")
  }

  it("testGetAndUpdateOpt") {
    withOutFile(prefix+"hash-map-get-and-update-opt") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          a.update(2, 3)

          val v = a(1).get() + 1
          a.update(1, v)
          println(a(1))
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdateOpt", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-get-and-update-opt")
  }

  it("testGetAndUpdateOptVar") {
    withOutFile(prefix+"hash-map-get-and-update-opt-var") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](1)
          val n = var_new(unit(1))
          n = 2
          
          a.update(2, readVar(n))

          a.update(3, a(readVar(n)).get() + 1)

          a.foreach( {x => println(x)} )
          a -= 2

          a.foreach( {x => println(x)} )
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapGetAndUpdateOptVar", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-get-and-update-opt-var")
  }

  it("testNestedObjectTrackingVar") {
    withOutFile(prefix+"hash-map-nested-object-tracking-var") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)

          val n = var_new(a)
          n.update(1, 3)
          println(n(1))
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapNestedObjectTrackingVar", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-nested-object-tracking-var")
  }

  it("testNestedReAssignment") {
    withOutFile(prefix+"hash-map-nested-reassignment") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)

          val n = var_new(a)
          n.update(1, 3)

          println(a(1))
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapNestedReAssignment", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-nested-reassignment")
  }

  it("testNestedPrimitiveReAssignment") {
    withOutFile(prefix+"hash-map-nested-primitive-reassignment") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)

          println(a.size)

          val n = var_new(a)
          val n2 = var_new(readVar(n))
          n2.setSize(3)

          println(a.size)
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapNestedPrimitiveReAssignment", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-nested-primitive-reassignment")
  }

  it("testUpdate") {
    withOutFile(prefix+"hash-map-update") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Array[Int]](200)
          val c = NewArray[Int](5)
          c.update(0, 1)
          c.update(1, 2)
          a.update(1, c)
          //mutable can not be added to HashMap
          val b = a(1).get //variables are not removed
          b.update(0, 10)
          println(a(1).get()(0))

        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapUpdate", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-update")
  }

  it("testContains") {
    withOutFile(prefix+"hash-map-contains") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          println(a.contains(1))
          println(a.contains(0))
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapContains", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-contains")
  }

  it("testSize") {
    withOutFile(prefix+"hash-map-size") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          a.update(1, 2)
          a.update(2, 3)
          println(a.size)
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapSize", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-size")
  }

  it("testForEach") {
    withOutFile(prefix+"hash-map-for-each") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          a.update(1, 2)
          a.update(2, 3)
          a.foreach({e => println(e)})
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapForEach", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-for-each")
  }

  it("testDelete") {
    withOutFile(prefix+"hash-map-delete") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = HashMap[Int, Int](200)
          a.update(1, 2)
          a.update(2, 3)
          println(a.size)
          a -= 2
          println(a.size)
        }
        f(1)
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapDelete", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"hash-map-delete")
  }

 /**
  * Tests for array implementation (old)
  */
 it("testIntArraySeqCreation") {
    withOutFile(prefix+"array-seq-creation") {
      val prog = new MyOpsExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit(1), unit(2), unit(3))
          println(a(unit(0)))
        }

        def g(i : Rep[Int]): Rep[Unit] = {
          val a = Array(unit('a'), unit('b'), unit('c'))
          println(a(unit(0)))
        }
      }

      val codegen = new MyCodeGen { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntArrayCreation", new PrintWriter(System.out))
      codegen.emitSource1(prog.g, "CharArrayCreation", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"array-seq-creation")
  }

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