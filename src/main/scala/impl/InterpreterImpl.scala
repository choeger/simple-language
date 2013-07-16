/*
 * Copyright (c) 2013, Technische Universität Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.sl2.impl

import scala.io.Source

import org.kiama.util.REPL

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.modules.Syntax.Var

/**
  * An interpreter for SL.
  */
object InterpreterImpl extends Interpreter with FrontEnd with Lexic with Syntax
		       with CombinatorParser with EnrichedLambdaCalculus with GraphImpl[Var]
		       with LetRecSplitter with Type with NameSupply with Context
		       with Substitution with Unification with TypeCheckerImpl
		       with DTCheckerImpl with FDCheckerImpl with ProgramCheckerImpl
		       with Errors with REPL {


  /**
    * Evaluate an SL program which has been transformed into the enriched lambda calculus.
    */
  def eval(env: => Environment, expr: ELC): Either[Error, Value] = expr match {
    /* Values of the built-in types form the atomic expressions of the language */
    case int: EInt   => Right(int)
    case char: EChar => Right(char)
    case str: EStr   => Right(str)

    /* Look up the evaluated expression bound to the variable in the given environment */
    case v: EVar => env.get(v).toRight(GenericError("Could not evaluate variable " + quote(v.toString)))

    /* Constructors are atomic values of the language, too */
    case con: ECon => Right(con)

    /* Built-in binary operators */
    case EApp(EApp(EVar(f, _), e1, _), e2, _) if (predefinedOps contains f) => {
      for ( x <- eval(env, e1).right ;
	    y <- eval(env, e2).right ;
	    res <- evalBinOp(f, x, y).right )
      yield res
    }

      /* Built-in conversion functions */
    case EApp(EVar(f, _), e, _) if (conversionFuns contains f) => {
      for ( x <- eval(env, e).right ;
	    res <- evalConversion(f, x).right )
      yield res
    }

    case _ => {
      println("Expr: " + expr)
      Left(NotYetImplemented)
    }
  }


  /**
    * Evaluate SL's built-in arithmetic and comparison binary operators.
    */
  def evalBinOp(f: Var, arg1: Value, arg2: Value): Either[Error, Value] = (f, arg1, arg2) match {
    /* Arithmetic functions */
    case ("+", IntVal(x), IntVal(y)) => Right(x + y)
    case ("-", IntVal(x), IntVal(y)) => Right(x - y)
    case ("*", IntVal(x), IntVal(y)) => Right(x * y)
    case ("/", IntVal(x), IntVal(y)) => Right(x / y)

    /* Comparison functions */
    case ("<", IntVal(x), IntVal(y))  => Right(x < y)
    case ("<=", IntVal(x), IntVal(y)) => Right(x <= y)
    case ("==", IntVal(x), IntVal(y)) => Right(x == y)
    case ("/=", IntVal(x), IntVal(y)) => Right(!(x == y))
    case (">=", IntVal(x), IntVal(y)) => Right(x >= y)
    case (">", IntVal(x), IntVal(y))  => Right(x > y)

    /* String concatenation */
    case ("+s", StringVal(s), StringVal(t)) => Right(s + t)

    case _ => Left(GenericError("Eval Error"))
  }


  /**
    * Evaluate SL's built-in conversion functions.
    */
  def evalConversion(f: Var, arg: Value): Either[Error, Value] = (f, arg) match {
    case ("int2Str", IntVal(int))    => Right(int.toString)
    case ("char2Str", CharVal(char)) => Right(char.toString)
    case ("ord", CharVal(char))      => Right(char.toInt)
    case ("chr", IntVal(int))        => Right(int.toChar)

    case _ => Left(GenericError("Eval Error"))
  }



  /*
   * Define a read-eval-print-loop (REPL) using Kiama's REPL trait.
   */
  var module: Option[String] = None

  /* Load the module given as a command line parameter */
  override def setup (args: Array[String]): Boolean = {
    emitter.emitln ("Enter SL expressions for evaluation.")

    if (! args.isEmpty) {
      val source = scala.io.Source.fromFile(args(0))
      module = Some(source.mkString)
      source.close()
    }

    true
  }

  /* Parse, check, and evaluate the user's input */
  def processline (line: String): Unit = {
    val prelude = Source.fromURL(getClass.getResource("/Prelude.sl")).getLines.mkString("\n")

    /*
     * We treat the user's input as the module's `main' function. This means, that the
     * given input has to meet al the requirements of the body of a `main' function.
     */
    val input = List(prelude, module.getOrElse("") + "\nDEF main = " + line)

    val result = for ( program <- run(input).right ;
		       evaluatedProg <- eval(Map.empty, program).right )
		 yield evaluatedProg
    
    result.fold((err: Error) => println(err.message), println(_))
  }
}
