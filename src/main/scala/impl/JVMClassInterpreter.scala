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

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.runtime

import scala.collection.mutable 

trait JVMClassInterpreter extends ByteCodeInterpreter with Syntax with Errors with IMSyntax with JVMEncoder {   

  /**
   * A custom class loader to simplify byte code loading
   */
  object dynamicClassLoader extends ClassLoader {
    private val classCache = mutable.HashMap[String, Class[_]]()
    
    def define(classes : List[JVMClass]) = {
      for (klazz <- classes) {
        val kl = super.defineClass(klazz.name.toString, klazz.code, 0, klazz.code.size);
        classCache.put(klazz.name.toString, kl)
      }
    }

    def load(klazz : ClassName) = {
      if (classCache.contains(klazz.toString))
        classCache(klazz.toString)
      else
        super.findClass(klazz.toString)
    }

    def clearCache() {
      classCache.clear
    }
  }

  def decode(o : Object) : Either[Error, String] = o match {
    case i:Integer => Right(i.toString)
    case f:runtime.Closure => Right("Closure: " + o.getClass)
    case _ => Left(GenericError("Decoding of '%s' failed".format(o)))
  }

  def eval(klazz : ClassName, classes : List[JVMClass]) : Either[Error, String] = {
    for (o <- evalToObject(klazz, classes).right ; dec <- decode(o).right)
      yield dec
  }

  /**
   * Load bytecode as JVM class and execute its "eval" method.
   */
  def evalToObject(klazz : ClassName, classes : List[JVMClass]) : Either[Error, Object] = {
    try {
      dynamicClassLoader.clearCache
      dynamicClassLoader.define(classes)
      val created = dynamicClassLoader.load(klazz)
      val method = created.getMethod("eval")
      Right(method.invoke(null))
    } catch {
      case t:Throwable => {
        Left(GenericError(t.getMessage))
      }
    }
  }

}
