package scala.virtualization.lms
package epfl
package test12

import common._
import internal._
import java.io.PrintWriter
import scala.reflect.SourceContext

class TestArrayOps extends FileDiffSuite {

  val prefix = "test-out/epfl/test12-"

  //old
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

  it("testComplexNested") {
    withOutFile(prefix+"hash-map-complex-nested") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, HashMap[Int,Int]](unit(1))
          val a1 = hashmap_new[Int,Int](unit(1))
          val a2 = hashmap_new[Int,Int](unit(1))

          a.update(unit(1), a1)
          a.update(unit(2), a2)

          a1.update(unit(1), unit(1))
          a(unit(1)).get().update(unit(1), unit(2))

          a.foreach({x => println(x); x.getValue().foreach({println(_)}) })
          a.foreach({x => a.update(x.getKey(), unit(null).AsInstanceOf[HashMap[Int, Int]])  })
          a.foreach({x => println(x)})
          a.foreach({x => a -= x.getKey()})
          a.update(unit(3), a1)
          a.foreach({x => println(x); x.getValue().foreach({println(_)}) })
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapComplexNested", new PrintWriter(System.out))
    }
    //assertFileEqualsCheck(prefix+"hash-map-creation")
  }

  it("testComplex") {
    withOutFile(prefix+"hash-map-complex") {
      val prog = new HashMapArrOps with MiscOps with HashMapArrOpsExp
        with MiscOpsExp with ScalaOpsPkgExp {
        def f(i : Rep[Int]): Rep[Unit] = {
          val a = hashmap_new[Int, Int](unit(1))
          a.update(unit(1), unit(2))
          a.update(unit(2), unit(3))

          val v = a(unit(1)).get() + unit(1)
          a.update(unit(1), v)

          a.foreach({x => println(x)})
          a.foreach({x => a.update(x.getKey(), unit(0))})
          a.foreach({x => println(x)})
          a.foreach({x => a -= x.getKey()})
          a.update(unit(3), unit(4))
          a.foreach({x => println(x)})
        }
        f(unit(1))
      }

      val codegen = new ScalaGenArrayOps with ScalaGenMiscOps
      with ScalaGenEntry with ScalaCodeGenPkg with ScalaGenHashCodeOps with ScalaGenOption
      with ScalaGenHashMap { val IR: prog.type = prog }
      codegen.emitSource1(prog.f, "IntHashMapComplex", new PrintWriter(System.out))
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

          val v = a(unit(1)).get() + unit(1)
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
          a -= unit(2)

          //a.clear()
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