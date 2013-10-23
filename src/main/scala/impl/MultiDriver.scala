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
 * */

package de.tuberlin.uebb.sl2.impl

import scala.collection.mutable.ListBuffer
import scala.text.Document
import scala.text.DocText
import de.tuberlin.uebb.sl2.modules._
import scala.io.Source
import scalax.io.JavaConverters._
import scalax.io._
import scalax.file._

import java.net.URL;
import scalaz._
import Scalaz._

/**
 * A driver that is able to compile more than one source file
 * at a time, using topological sorting
 */
trait MultiDriver extends Driver {
  self: Parser
  	with Syntax
  	with ProgramChecker
  	with Errors
  	with SignatureSerializer
  	with Configs
  	with ModuleResolver
  	with ModuleNormalizer
  	with ModuleLinearization =>


  /**
    *  Run a transitive module check
    *  As a side effect, this method creates .signature files.
    */
  override def run(config: Config): Either[Error, String] = {    
     for (
      // load all (indirectly) required modules
      modules <-  errorMap(config.sources, {src => 
        createModuleFromSourceFile(src, config)}).right ;

      // load dependencies
      dependencyList <- errorMap(modules, { mod => loadModuleDependencies(mod, config) }).right ;
      dependencies = Map() ++ dependencies ;

      // sort topologically
      sortedModules <- topoSort(dependencies).right ;
      _ <- ensureDirExists(config.destination).right ;

      // compile in topological order
      ordered <- sortedModules;

      results <- errorMap(ordered.toSeq.toList, handleSource(_ :Module, config)).right)
     yield results
  }
  
  def createModuleFromSourceFile(fileName: String, config: Config):Either[Error,Module] = {
    if(fileName endsWith ".sl") {
      findModuleFromSource(fileName.substring(0, fileName.length-3), config)
    } else {
      Left(GenericError("Cannot compile: SL source file "+quote(fileName)+" does not end with suffix .sl"))
    }
  }
  
  def findModules(importedBy: Module, names: List[String], config:Config):Either[Error,List[Module]] = {
    errorMap(names, findModule(importedBy, _: String, config))
  }
  
  /**
   * Create an appropriate module object, based on available files:
   * 
   * <ol>
   * <li>If there is no signature file on the classpath for the imported
   *     file or the source file was given explicitly, create the module
   *     to be compiled from its source file. Return an error, if the
   *     source file does not exist.</li>
   * <li>If there is a signature file and a source file, and the source
   * 	 file is younger, create a module to be compiled from its source
   *     file.</li>
   * <li>Otherwise create a module that is not to be compiled.</li>    
   */
  def findModule(importedBy: Module, name: String, config: Config):Either[Error,Module] = {
    try {
	    val module = moduleFromName(name, config)
	    if(!module.signature.canRead) {
	    	// no signature file exists
	    	if(module.source.canRead) {
	    	    Right(module.copy(compile = true))
	    	} else {
	    		Left(FilesNotFoundError("Module "+name+" imported by "+importedBy.name+" not found: ",
	    				module.source.path, module.signature.path))
	    	}
	    } else if(module.source.canRead() &&
	              (module.source.lastModified() > module.signature.lastModified())) {
	    	// a signature file exists, as well as a source file
	        Right(module.copy(compile = true))
	    } else {
	    	// a signature, but no source file exists: load from signature
	    	Right(module)
	    }
    } catch {
      case ioe: IOException => Left(GenericError(ioe.getMessage()))
    }
  }
  
  /**
   * Create a module to be compiled from its source file.
   */
  def findModuleFromSource(name: String, config: Config): Either[Error,Module] = {
    val module = moduleFromName(name, config)
	if(module.source.canRead) {
	    Right(module.copy(compile = true))
	} else {
		Left(FileNotFoundError(module.source.path))
	}
  }

  /**
   * Resolves the direct and transient dependencies of the given module,
   * if it is not yet a key in the given dependencies map and if it is to
   * be compiled.
   */
  def loadModuleDependencies(module: Module, config: Config,
      dependencies: Map[Module,Set[Module]]):
      Either[Error, Map[Module,Set[Module]]] = {
    if((dependencies contains module) || !module.compile) {
      Right(dependencies)
    } else {
      val deps = getDirectDependencies(module, config)
      if(deps.isLeft) {
        Left(deps.left.get)
      } else {
        val depsR = deps.right.get.filter(_.compile) // ignore dependencies that need no compilation
        (Right(dependencies + (module -> depsR)).
            asInstanceOf[Either[Error, Map[Module,Set[Module]]]] /: depsR) {
          (currDeps, mod) => currDeps.right.flatMap(loadModuleDependencies(mod, config, _))
        }
      }
    }
  }
  
  /**
   * finds out the depencies of a given compilation unit
   */
  def getDirectDependencies(module: Module, config: Config): Either[Error, Set[Module]] = {
    // load input file
    val code = module.source.contents

    // parse the syntax
    fileName = module.source.path
    val ast = parseAst(code)
    
    // resolve dependencies
    for (
      mod <- ast.right;
      imports <- resolveDependencies(mod, config).right;
      x <- findModules(module, imports.toList, config).right
    ) yield x.toSet
  }
  
  def handleSource(module: Module, inputConfig: Config) = {
    // load input file
    val name = module.name

    //TODO: move main marking to Module?
    val isMain = inputConfig.sources.contains(name+".sl")
    val destination = inputConfig.destination
    val config = inputConfig.copy(mainName = module.source.filename, 
      mainParent = module.source.parent, destination = destination)
    val code = module.source.contents
    
    for (
      moq <- qualify(name, code, config).right;      
      // output to fs
      res <- outputToFiles(moq, name, config).right
    ) yield res
  }
  
  def qualify(name: String, code: String, config: Config): Either[Error, Program] = {
    val ast = parseAst(code)

    val checkResults = for (
      mo <- ast.right;
      // check and load dependencies
      imports <- inferDependencies(mo, config).right;
      // type check the program
      _ <- checkProgram(mo, normalizeModules(imports)).right)
      yield imports
      
    for (
      mo <- ast.right;
      // check and load dependencies
      imports <- checkResults.right
    ) yield qualifyUnqualifiedModules(mo.asInstanceOf[Program], imports)
  }
  
  def ensureDirExists(dir: Path) : Either[Error, Path] = {
    // Create directory, if necessary
    try {
      Right(dir.createDirectory(failIfExists = false))
    } catch {
      case e:Throwable => Left(GenericError(e.getMessage))
    }
  }
  
  def outputToFiles(program: Program, name: String, config: Config): Either[Error, Unit] = {    
    // write compiled external signature file
    val signatureFile = config.destination / (name + ".sl.signature")
    signatureFile.write(serialize(program))
    
    Right(())
  }

  def mergeAst(a: Program, b: Program): Either[Error, Program] = {
    for (
      sigs <- mergeMap(a.signatures, b.signatures).right;
      funs <- mergeMap(a.functionDefs, b.functionDefs).right;
      funsEx <- mergeMap(a.functionDefsExtern, b.functionDefsExtern).right
    ) yield {
      val defs = a.dataDefs ++ b.dataDefs
      Program(List(), sigs, funs, funsEx, defs)
    }
  }

  def mergeMap[A, B](a: Map[A, B], b: Map[A, B]): Either[Error, Map[A, B]] = {
    val intersect = a.keySet & b.keySet
    if (intersect.isEmpty)
      Right(a ++ b)
    else
      Left(DuplicateError("Duplicated definition: " + intersect.mkString(", "), "", Nil))
  }
}

