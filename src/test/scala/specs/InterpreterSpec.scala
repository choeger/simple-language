/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest._
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.impl._
import de.tuberlin.uebb.sl2.modules._
import java.lang.ClassLoader

import scala.collection.JavaConversions._
import scala.io.Source

trait InterpreterSpec	
  extends FunSpec
  with Inside
  with Matchers
  with SLPrograms {

  this: Syntax with ModuleCompiler with ExprCompiler 
  with Configs with Parser with ByteCodeInterpreter =>

  /**
   * Generate a '''fresh''' class name for the evaluation process
   */
  def freshClassName : ClassName

  implicit class slString(val s: String) {

    val className = freshClassName

    def compiled() = {
      val str = s.stripMargin
      CompilationResult(className, compileModule(className, str))
    }

    def evaluated() : Either[Error, EvaluationResult] = {
      val str = s.stripMargin
      for (classes <- compileExpr(className, "eval", str).right ; 
           res <- evalToObject(className, "eval", classes).right
         ) yield {
        EvaluationResult(res)
      }
    }
  }
    
  sealed case class CompilationResult(val className : ClassName, 
                                      res : Either[Error, List[JVMClass]]) {
    def evaluated(fn : String) : Either[Error, EvaluationResult] = {
      for (classes <- res.right ;
           obj <- evalToObject(className, fn, classes).right)
        yield {
          EvaluationResult(obj)
        }
    }
  }

  sealed case class EvaluationResult(res : Any)

  def result_correctly_in(other : Either[Error, EvaluationResult]) : Matcher[Either[Error, EvaluationResult]] = {
    (equal(other)) and (be ('right))
  }
 
  describe("An SL Compiler") {
    
    describe("Handling literals") {
      it("Should compile the 'True' literal correctly ") {
        ("""PUBLIC FUN test: Bool
         |DEF test = True""".compiled.evaluated("test")) should be (Right(EvaluationResult(true)))
      }

      it("Should compile the 'False' literal correctly") {
        ("""PUBLIC FUN test: Bool
         |DEF test = False""".compiled.evaluated("test")) should be (Right(EvaluationResult(false)))
      }

      it("Should compile integer literals correctly") {
        ("""PUBLIC FUN test: Int
      	 |DEF test = 42""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
      }

      it("Should compile character literals correctly") {
        ("""PUBLIC FUN test: Char
      	 |DEF test = 'c'""".compiled.evaluated("test")) should result_correctly_in("'c'".evaluated)
      }
      
      it("Should compile string literals correctly") {
        ("""PUBLIC FUN test: String
         |DEF test = "42"""".compiled.evaluated("test")) should be(Right(EvaluationResult("42")))
      }

      it("Should compile real literals correctly") {
        """PUBLIC FUN test: Real
        |DEF test = 42.0""".compiled.evaluated("test") should be(Right(EvaluationResult(42.0)))
      }
    }
    
    describe("handling the prelude") {
        it("Should compile addition correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = 40 + 2""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
        }

        it("Should compile string concatenation correctly") {
          ("""PUBLIC FUN test: String
           |DEF test = "a" ++ "b"""".compiled.evaluated("test")) should result_correctly_in(""""ab"""".evaluated)
        }

        it("Should compile multiplication correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = 40 * 2""".compiled.evaluated("test")) should result_correctly_in("80".evaluated)
        }

        it("Should compile division correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = 40 / 2""".compiled.evaluated("test")) should result_correctly_in("20".evaluated)
        }

        it("Should round integer division") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = 5 / 2""".compiled.evaluated("test")) should result_correctly_in("2".evaluated)
        }

        it("Should compile subtraction correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = 44 - 2""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
        }
      }
      
      describe("handling conditionals") {
        it("Should compile if-then-else correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = IF True THEN 42 ELSE 23""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
        }

        it("Should allow for complex condititons") {
          "IF 39 + 2 < 42 THEN 42 ELSE 39".evaluated should be (Right(EvaluationResult(42)))
        }

        it("Should allow for complex bodies") {
          "IF False THEN 13 ELSE ((13 + 7) * 2) + 2".evaluated should be (Right(EvaluationResult(42)))
        }
      }

      describe("handling lambda-expressions") {

        it("Should compile the identity lambda expression correctly") {
          ("""PUBLIC FUN test: Int
      	   |DEF test = (\ x . x) 42""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
        }

        it("Should compile lambdas with multiple arguments correctly") {
          ("""PUBLIC FUN test: Int
           |DEF test = (\x y. x) 1 2""".compiled.evaluated("test")) should result_correctly_in("1".evaluated)
        }

        it("Should compile lambdas with many arguments correctly") {
          ("""PUBLIC FUN test: Int
           |DEF test = (\a b c d e. a * e) 1 2 3 4 5""".compiled.evaluated("test")) should result_correctly_in("5".evaluated)
        }
      }
  }

  describe("Compiling constants") {
    it("Evaluate the declarations in order of dependency") {
      (constants+"\n"+
       """PUBLIC FUN test: a
         |DEF test = c3""").compiled.evaluated("test") should result_correctly_in("2".evaluated)
    }

    it("Should hide constants by LETs") {
      (constants+"\n"+
       """PUBLIC FUN test: a
         |DEF test = l3""").compiled.evaluated("test") should result_correctly_in("3".evaluated)
    }

    it("Should correctly evaluate shadowed right hand sides") {
      (constants+"\n"+
       """PUBLIC FUN test: a
         |DEF test = l1""").compiled.evaluated("test") should result_correctly_in("1".evaluated)
    }

    it("Should correctly evaluate shadowed calculating right hand sides") {
      (constants+"\n"+
       """PUBLIC FUN test: a
         |DEF test = l2""").compiled.evaluated("test") should result_correctly_in("2".evaluated)
    }

    it("Should correctly capture closure variables") {
      (constants+"\n"+
       """PUBLIC FUN test: a
         |DEF test = l4""").compiled.evaluated("test") should result_correctly_in("8".evaluated)
    }
  }

  
  describe("Compiling (recursive) function definitions") {

    it("Should compile the identity function correctly") {
      ("""DEF id2 x = x
      	 |PUBLIC FUN test: Int
      	 |DEF test = id2 42""".compiled.evaluated("test")) should result_correctly_in("42".evaluated)
    }

    it("Should compile the factorial function correctly") {
      ("""DEF fac n = IF n == 1 THEN 1 ELSE n * fac (n - 1)
         |PUBLIC FUN test: Int
         |DEF test = fac 12""".compiled.evaluated("test")) should result_correctly_in("479001600".evaluated)
    }

    it("Should compile mutually recursive DEFs") {
      """DEF even n = IF n == 0 THEN True ELSE odd (n - 1)
        |DEF odd n = IF n == 1 THEN True ELSE even (n - 1)
        |DEF test = even 100
      """.compiled.evaluated("test") should result_correctly_in("True".evaluated)
    }
    
  }

  /*  
    it("Should compile the head and tail function correctly") {
      ("""IMPORT "std/list" AS L
         |DEF head (L.Cons x xs) = x
         |DEF tail (L.Cons x xs) = xs
         |PUBLIC FUN test: Int
         |DEF test = head (tail (L.Cons 2 (L.Cons 1 L.Nil)))""".compiled.evaluated("test")) should result_correctly_in("1".evaluated)
    }

    it("Should compile the tree data structure correctly") {
      ("""DATA Tree a = Leaf a | Node (Tree a) (Tree a)
         |DEF sum (Leaf n) = n
         |DEF sum (Node x y) = sum x + sum y
         |PUBLIC FUN test: Int
         |DEF test = sum (Node (Leaf 13) (Node (Leaf 2)(Leaf 3)))""".compiled.evaluated("test")) should result_correctly_in("18".evaluated)
    }

    it("Should compile case") {
      ("""DATA Tree a = Leaf a | Node (Tree a) (Tree a)
         |DEF sum x = CASE x
         |            OF Leaf a THEN a
         |            OF Node x y THEN sum x + sum y
         |PUBLIC FUN test: a
         |DEF test = sum (Node (Leaf 13) (Node (Leaf 2)(Leaf 3)))""".compiled.evaluated("test")) should result_correctly_in("18".evaluated)
    }


    it("Should compile mutually recursive even and odd function") {
      ("""DEF even n = IF n == 0 THEN True ELSE odd (n - 1)  
         |DEF odd n = IF n == 1 THEN True ELSE even (n - 1)
         |PUBLIC FUN test: Bool
         |DEF test = even 22""".compiled.evaluated("test")) should result_correctly_in("true".evaluated)
    }

    it("Should compile mutually recursive even and odd in let expression") {
      ("""PUBLIC FUN test: a
         |DEF test = LET even = \ n . IF n == 0 THEN True ELSE odd (n - 1)  
         |    odd = \ n . IF n == 1 THEN True ELSE even (n - 1)  
         |    IN even 22 """.compiled.evaluated("test")) should result_correctly_in("true".evaluated)
    }

    it("Should evaluate LETs in dependency-order") {
      """PUBLIC FUN test: a
        |DEF test = LET x=y y=5 IN x""".compiled.evaluated("test") should result_correctly_in("5".evaluated)
    }

    it("Should compile nested lets") {
      ("""PUBLIC FUN test: x
         |DEF test = LET a = LET c = 3 IN c + c IN a""".compiled.evaluated("test")) should result_correctly_in("6".evaluated)
    }


    it("Should successfully compile functions with many parameters") {
      ("""DEF add x y z = x + y + z
         |PUBLIC FUN test: a
      	 |DEF test = add 1 2 3""".compiled.evaluated("test")) should result_correctly_in("6".evaluated)
    }
  }




  describe("Compiling functions using pattern matching") {
    it("Should compile late matches") {
      (lateMatch+"\n"+
       """PUBLIC FUN test: x
         |DEF test = f (L.Cons 1 L.Nil) 3 L.Nil""").compiled.evaluated("test") should result_correctly_in("3".evaluated)
    }

    it("Should compile nested matches") {
      """IMPORT "std/list" AS L
        |DEF f (L.Cons (L.Cons a b) (L.Cons d L.Nil)) = a
        |PUBLIC FUN test: x
        |DEF test = f (L.Cons (L.Cons 4 (L.Cons 5 L.Nil)) (L.Cons (L.Cons 8 L.Nil) L.Nil))""".compiled.evaluated("test") should result_correctly_in("4".evaluated)
    }

    it("Should compile mixed patterns") {
      (mixedPatterns+"\n"+
       """PUBLIC FUN test: x
         |DEF test = f Green True""").compiled.evaluated("test") should result_correctly_in("2".evaluated)
    }

    it("Should compile function with overlapping patterns") {
      (overlappingPatterns+"\n"+
       """PUBLIC FUN test: x
         |DEF test = f 1 2 3""").compiled.evaluated("test") should result_correctly_in("1".evaluated)
    }
  }
  

  describe("Compiling list concatenation") {
    it("Should work on the empty lists") {
      ("""IMPORT "std/list" AS L"""+"\n"+concat+"\n"+
       """PUBLIC FUN test: a
         |DEF test = L.Nil +++ L.Nil""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        		 |PUBLIC FUN test: b
				 |DEF test = L.Nil""".compiled.evaluated("test"))
    }

    it("Should work on the singleton list") {
      ("""IMPORT "std/list" AS L"""+"\n"+concat+"\n"+
       """PUBLIC FUN test: a
         |DEF test = (L.Cons 1 L.Nil) +++ L.Nil""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        		 |PUBLIC FUN test: b
				 |DEF test = L.Cons 1 L.Nil""".compiled.evaluated("test"))
    }

    it("Should be symmetric on the empty list") {
      ("""IMPORT "std/list" AS L"""+"\n"+concat+"\n"+
       """PUBLIC FUN test: a
         |DEF test = L.Nil +++ (L.Cons 1 L.Nil)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        		 |PUBLIC FUN test: b
        		 |DEF test = L.Cons 1 L.Nil""".compiled.evaluated("test"))
    }

    it("Should append to the end of the list") {
      ("""IMPORT "std/list" AS L"""+"\n"+concat+"\n"+
       """PUBLIC FUN test: a
         |DEF test = (L.Cons 2 L.Nil) +++ (L.Cons 1 L.Nil)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        		 |PUBLIC FUN test: b
        		 |DEF test = L.Cons 2 (L.Cons 1 L.Nil)""".compiled.evaluated("test"))
    }

    it("Should work on slightly larger lists") {
      ("""IMPORT "std/list" AS L"""+"\n"+concat+"\n"+
       """PUBLIC FUN test: a
         |DEF test = (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil)))) +++ (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil))))""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        		 |PUBLIC FUN test: b
        		 |DEF test = (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil))))))))""".compiled.evaluated("test"))
    }
  }


  describe("Compiling a list generator") {
    it("Should work for the recursion anchor") {
      (range+"\n"+
       """PUBLIC FUN test: a
         |DEF test = range 0""").compiled.evaluated("test") should (
         be ('right) and
         be ("""IMPORT "std/list" AS L
               |PUBLIC FUN test: 
               |DEF test = L.Cons 0 L.Nil""".compiled.evaluated("test"))
        )
    }

    it("Should work for a smaller list") {
      (range+"\n"+
       """PUBLIC FUN test: a
         |DEF test = range 4""").compiled.evaluated("test") should (
         be ('right) and
         be ("""IMPORT "std/list" AS L
        	   |PUBLIC FUN test: b
               |DEF test = L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 (L.Cons 0 L.Nil))))""".compiled.evaluated("test")))
    }
  }

  
  describe("Compiling filter") {
    it("Should work in the empty list") {
      ("""IMPORT "std/list" AS L"""+"\n"+filter+"\n"+
       """PUBLIC FUN test: a
         |DEF test = filter L.Nil (\ x . False)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: b
                 |DEF test = L.Nil""".compiled.evaluated("test"))
    }
    
    it("Should work on equality") {
      ("""IMPORT "std/list" AS L"""+"\n"+filter+"\n"+
       """PUBLIC FUN test: a
         |DEF test = filter (L.Cons 1 (L.Cons 2 (L.Cons 0 L.Nil))) (\ x . x == 0) """).compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: b
                 |DEF test = L.Cons 0 L.Nil""".compiled.evaluated("test"))
    }

    it("Should work on greater-than") {
      ("""IMPORT "std/list" AS L"""+"\n"+filter+"\n"+
       """PUBLIC FUN test: a
         |DEF test = filter (L.Cons 1 (L.Cons 0 (L.Cons 2 L.Nil))) (\ x . x > 0)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: b
                 |DEF test = L.Cons 1 (L.Cons 2 L.Nil)""".compiled.evaluated("test"))
    }
  }


  describe("Compiling reverse") {
    it("Should work on the empty list") {
      ("""IMPORT "std/list" AS L"""+"\n"+reverse+"\n"+
       """PUBLIC FUN test: a
         |DEF test = reverse L.Nil""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Nil""".compiled.evaluated("test"))
    }
    
    it("Should work on the singleton list") {
      ("""IMPORT "std/list" AS L"""+"\n"+reverse+"\n"+
       """PUBLIC FUN test: a
         |DEF test = reverse (L.Cons 1 L.Nil)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 1 L.Nil""".compiled.evaluated("test"))
    }

    it("Should work on a 4-element list") {
      ("""IMPORT "std/list" AS L"""+"\n"+reverse+"\n"+
       """PUBLIC FUN test: a
         |DEF test = reverse (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil))))""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 1 (L.Cons 2 (L.Cons 3 (L.Cons 4 L.Nil)))""".compiled.evaluated("test"))
    }

    it("Should work be it's own inverse") {
      ("""IMPORT "std/list" AS L"""+"\n"+reverse+"\n"+
       """PUBLIC FUN test: a
         |DEF test = reverse (reverse (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil)))))""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil)))""".compiled.evaluated("test"))
    }
  }


  describe("Compiling quicksort") {
    it("Should work in the empty list") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort L.Nil""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Nil""".compiled.evaluated("test"))
    }

    it("Should work on the singleton list") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort (L.Cons 1 L.Nil)""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 1 L.Nil""".compiled.evaluated("test"))
    }

    it("Should work on an ordered list") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort (L.Cons 1 (L.Cons 2 (L.Cons 3 (L.Cons 4 L.Nil))))""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 1 (L.Cons 2 (L.Cons 3 (L.Cons 4 L.Nil)))""".compiled.evaluated("test"))
    }
    
    it("Should work on an reverse-ordered list") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort (L.Cons 4 (L.Cons 3 (L.Cons 2 (L.Cons 1 L.Nil))))""").compiled.evaluated("test") should
         result_correctly_in("""IMPORT "std/list" AS L
        	     |PUBLIC FUN test: a
                 |DEF test=L.Cons 1 (L.Cons 2 (L.Cons 3 (L.Cons 4 L.Nil)))""".compiled.evaluated("test"))
    }

    it("Should work on larger input") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort (reverse (range 100))""").compiled.evaluated("test") should
         result_correctly_in((range+"""
                |PUBLIC FUN test: a
                |DEF test=range 100""").compiled.evaluated("test"))
    }

    it("Should work on even larger input") {
      (sort+"\n"+
       """PUBLIC FUN test: a
         |DEF test = quicksort (reverse (range 400))""").compiled.evaluated("test") should
         result_correctly_in((range+"""
                |PUBLIC FUN test: a
                |DEF test=range 400""").compiled.evaluated("test"))
    }
  }


  describe("Compiling SL programs") {
    it("Should compile a function with `Bool' arguments") {
      ("""DEF f True  x = 0
         |DEF f False x = 1
         |PUBLIC FUN test: a
         |DEF test = f True 10""".compiled.evaluated("test")) should result_correctly_in("0".evaluated)
    }

    it("Should compile a case expressions on a built-in type") {
      (caseWithBuiltIn+"\n"+
       """PUBLIC FUN test: a
         |DEF test = f False""").compiled.evaluated("test") should result_correctly_in("2".evaluated)
    }

    it("Should compile a case expressions on a user-defined type") {
      (caseWithCustomType+"\n"+
       """PUBLIC FUN test: a
         |DEF test = f True 251""").compiled.evaluated("test") should result_correctly_in("251".evaluated)
    }

    it("Should compile nested conditionals") {
      (nestedConditional+"\n"+
       """PUBLIC FUN test: a
         |DEF test = f 100 200 300""").compiled.evaluated("test") should result_correctly_in("0".evaluated)
    }

    it("Should compile a function with a reserved JavaScript name as argument name") {
      ("""DEF f return = return + 1
         |PUBLIC FUN test: a
         |DEF test = f 1""".compiled.evaluated("test")) should result_correctly_in("2".evaluated)
    }

    it("Should compile pattern matching in lambda abstractions") {
      (lambdaPatterns+"\n"+
       """PUBLIC FUN test: r
         |DEF test = f""").compiled.evaluated("test") should result_correctly_in("-531".evaluated)
    }

    it("Should compile functions with shadowed local definitions") {
      (shadowedLocalDef+"\n"+
       """PUBLIC FUN test: r
         |DEF test = f""").compiled.evaluated("test") should result_correctly_in("0".evaluated)
    }

    it("Should compile functions with nested local definitions") {
      (nestedLet+"\n"+
       """PUBLIC FUN test: z
         |DEF test = result""").compiled.evaluated("test") should result_correctly_in("12345".evaluated)
    }

    it("Should compile functions with shadowed pattern variables") {
      (shadowedPatternVar+"\n"+
       """PUBLIC FUN test: z
         |DEF test = f (L.Cons 1 (L.Cons 2 L.Nil)) 10""").compiled.evaluated("test") should result_correctly_in("11".evaluated)
    }

    it("Should compile functions where pattern variables shadow top-level names") {
      (shadowedTopLevelNames+"\n"+
       """PUBLIC FUN test: z
         |DEF test = f (-5) (-10) 0""").compiled.evaluated("test") should result_correctly_in("-15".evaluated)
    }

    it("Should compile ulam function") {
      (ulam+"\n"+
       """PUBLIC FUN test: z
         |DEF test = ulam 11""").compiled.evaluated("test") should result_correctly_in("1".evaluated)
    }

    it("Should compile partial application") {
      (partialApplication+"\n"+
       """PUBLIC FUN test: z
         |DEF test = call (-10) (g 10)""").compiled.evaluated("test") should result_correctly_in("0".evaluated)
    }

    it("Should compile functions with shadowed local names") {
      (shadowedVars+"\n"+
       """PUBLIC FUN test: f
         |DEF test = f""").compiled.evaluated("test") should result_correctly_in("-590".evaluated)
    }
  }*/

}
