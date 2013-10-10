package scala.virtualization.lms
package epfl
package test16

import common._
import internal._
import java.io.PrintWriter

import scala.reflect.SourceContext


class TestDBMSOpts extends FileDiffSuite {
  val prefix = "test-out/epfl/test16-" 

  def testDBMSOpt1 = {
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
