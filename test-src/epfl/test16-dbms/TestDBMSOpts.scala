package scala.virtualization.lms
package epfl
package test16

import common._
import internal._
import java.io.PrintWriter

import scala.reflect.SourceContext


class TestDBMSOpts extends FileDiffSuite {
  val prefix = "test-out/epfl/test16-"

  it("testDBMSOpt2") {
    withOutFile(prefix + "DBMSOpt2") {
      val prog = new ScalaOpsPkgExp with LiftVariables with LiftNumeric with LiftString with ScalaCompile { self =>
	val codegen = new ScalaCodeGenPkg { val IR: self.type = self; }
	def test() = {
  	  var x = NewArray[Array[Int]](3)
	  for ( i <- 0 to 2 )
	    x(i) = NewArray[Int](3)
	  for (i <- 0 to 2 ) {
	    var y = x(i)
	    y(i) = 2
	    println(y.mkString(","))
	    y = NewArray[Int](7)
	    y(6-i) = 1
	    println(y.mkString(","))
	    x(i) = y
	  }
	  x
       }
       codegen.emitSource0(test, "lala", new PrintWriter(System.out))
       val x = compile0(test)
       val arr = x()
       System.out.println(arr.length)
       for ( i <- 0 to arr.length - 1)
	 System.out.println(arr(i).mkString(","))
     }
    }
  }

  it("testDBMSOpt1") {
    withOutFile(prefix+"DBMSOpt1") {
      val prog = new ScalaOpsPkgExp with LiftVariables with ScalaCompile { self =>
	val codegen = new ScalaCodeGenPkg { val IR: self.type = self; }
        class DBMSSkeleton {
    	  var dummyInt: Var[Int] = null;
	  def setDummyInt(flag: Rep[Boolean], y: Rep[Int]) = {
      	    dummyInt = 0;
	    if (flag) {
	      dummyInt = y;
              y
	    } else dummyInt
          }
        }
        def test = {
	  val x = new DBMSSkeleton
	  val f = compile2(x.setDummyInt)
	  System.out.println(f(true, -1))
        }
	def test2 = {
	  val x = new DBMSSkeleton
	  val f = compile2(x.setDummyInt)
	  System.out.println(f(false, 9))
	}
      }
      prog.test
      prog.test2
    }
  }
}
