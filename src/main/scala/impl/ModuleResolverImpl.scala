package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._
import scalax.file._

import java.net.URLClassLoader

trait ModuleResolverImpl extends ModuleResolver {
  this: Syntax
  with Errors
  with Configs
  with SignatureSerializer =>

  /**
   * Iterate the class loader hierarchy until an URLClassLoader is found
   */
  def getURLClassLoader(cl : ClassLoader) : Option[URLClassLoader] = {
    if (cl == null) None
    else 
      if (cl.isInstanceOf[URLClassLoader])
	Some(cl.asInstanceOf[URLClassLoader])
      else
	getURLClassLoader(cl.getParent)
  }

  /**
   * Search for the standard library in the (accessible) class path
   */
  lazy val fixedStandardLibPath = {
    val loader = getURLClassLoader(this.getClass.getClassLoader)
    val urls  = (loader map (l => l.getURLs.toList)).getOrElse(Nil)

    val package_dirs = for (url <- urls ; 
			    op = Path(url.toURI) ; 
			    p <- op ; 
			    if (p / "lib" ).isDirectory) 
		       yield p / "lib"
    
    package_dirs.headOption.getOrElse(Path())
  }    


  def standardLibName = "std"
  
  def standardLibPath = fixedStandardLibPath

  def inferDependencies(program: AST, config: Config) : Either[Error, List[ResolvedImport]] = program match {
    case Program(imports, _, _, _, attribute) =>
      checkImports(imports) match {
        case Left(err) => return Left(err)
        case _ =>
      }
      
      val preludeImp = if (config.mainName != "prelude.sl")
         UnqualifiedImport(standardLibName+"/prelude") :: imports 
      else
         imports
      errorMap(preludeImp, resolveImport(config))
    case _ => throw new RuntimeException("")
  }
  
  def checkImports(imports : List[Import]) : Either[Error, Unit] = {
    for (
      _ <- checkPathSyntax(imports).right;
      _ <- checkUniquePath(imports).right;
      _ <- checkUniqueIde(imports).right
    ) yield ()
  }
  
  private def checkPathSyntax(imports : List[Import]) : Either[Error, Unit] = {
    if (imports.isEmpty)
      return Right()
    
    /** Checks the syntax of the path
     *
     * The underscore indicates terminal symbols.
     * Within '[' ']' only terminal symbols are enumerated.
     *
     * The syntax is defined as follows:
     *
     *   path   ::= _/?(dir _/) module
     *   dir    ::= char+
     *   module ::= char+
     *   char   ::= [:lower:] | [:digit:] | [-_]
     */
    def validatePath(imp : Import) : Either[Error, Unit] = {
      if (imp.path.equals("prelude") || imp.path.equals(standardLibName+"/prelude"))
    	return Left(ImportError("Prelude must not be imported explicitly.", imp.attribute))
      else if (imp.path.matches("/?([a-z0-9-_]+/)*[a-z0-9-_]+"))
        return Right()
      else
        return Left(InvalidPathError(imp.path, imp.attribute))
    }
    
    imports.map(validatePath).reduce(collectErrors)
  }
  
  private def checkUniquePath(imports : List[Import]) : Either[Error, Unit] = {
    if (imports.isEmpty)
      return Right()
    
    def checkUniqueness(imp : Import) : Either[Error, Unit] = {
      if (imports.count(_.path == imp.path) != 1)
        return Left(DuplicatePathError(imp.path, imp.attribute))
      else 
        return Right()
    }
    
    imports.map(checkUniqueness).reduce(collectErrors)
  }
  
  private def checkUniqueIde(imports : List[Import]) : Either[Error, Unit] = {
    val qualified = imports.filter(_.isInstanceOf[QualifiedImport]).map(_.asInstanceOf[QualifiedImport])

    if (qualified.isEmpty)
      return Right()
    
    def checkUniqueness(imp : QualifiedImport) : Either[Error, Unit] = {
      if (qualified.count(_.name == imp.name) == 1)
        return Right()
      else
        return Left(DuplicateModuleError(imp.name, imp.attribute))
    }
    
    if(!qualified.isEmpty)
    	qualified.map(checkUniqueness).reduce(collectErrors)
    else
    	Right(Unit) // avoids UnsupportedOperationException: empty.reduce
  }
  
  /**
   * Returns the names of modules imported in the given AST. This does not include
   * the implicitly imported prelude and imported externs, because both do not
   * need to be compiled.
   */
  def resolveDependencies(program: AST, config: Config) : Either[Error, Set[String]] = program match {
    case Program(imports, _, _, _, attribute) =>
      for(
        _ <- checkImports(imports).right;
        // ignore extern imports
        resolvedImports <- errorMap(imports.filter( x =>
        	(x.isInstanceOf[UnqualifiedImport]) ||
        	(x.isInstanceOf[QualifiedImport])), collectImport(config)).right) 
    	yield resolvedImports.toSet
  }

  def collectImport(config: Config)(imp: Import): Either[Error, String] = { Right(imp.path) }
  
  def resolveImport(config: Config)(imp: Import): Either[Error, ResolvedImport] = imp match {
    case ui @ UnqualifiedImport(path, attr) => 
      val stdPrefix = standardLibName + "/" // TODO: Do this less rigidly
      if (stdPrefix != path.substring(0, stdPrefix.length))
        return Left(GenericError("unqualified import " + path + " doesn't start with " + stdPrefix))
      for (
        file <- findImportResource(imp.path.substring(stdPrefix.length) + ".sl.signature", config, attr).right;
        signature <- importSignature(file).right
      ) yield ResolvedUnqualifiedImport(path, file, signature, ui)
    case qi @ QualifiedImport(path, name, attr) =>
      for (
        file <- findImport(config, imp.path + ".sl.signature", attr).right;
        jsFile <- findImport(config, imp.path + ".sl.js", attr).right;
        signature <- importSignature(file).right
      ) yield ResolvedQualifiedImport(name, path, file, signature, qi)
  }

  def findImportResource(path: String, config: Config, attr: Attribute): Either[Error, Path] = {
    val file:Path = standardLibPath / path
    if(!file.canRead) {
    	Left(ImportError("Could not find resource " + quote(file.path), attr))
    } else {
      val files = file::(config.classpath / path):: // the last two paths are necessary to compile the std. libraries
      (config.destination / path) :: Nil
      files.find(_.canRead).toRight(
	ImportError("Could not find resource " + quote(path)+ " at " + files.map(_.path).mkString("\n\t\t\t\tor "), attr))
    }
  }
  
  def findImport(config: Config, path: String, attr: Attribute): Either[Error, Path] = {
    val stdPrefix = standardLibName + "/" // TODO: Do this less rigidly
    if (stdPrefix == path.substring(0, stdPrefix.length)) {
      findImportResource(path.substring(stdPrefix.length), config, attr)
    } else {
      val files = (config.classpath / path)::(config.destination / path)::
                  (config.mainParent / path)::(Path(".") / path)::Nil
      files.find(_.canRead).toRight(
        ImportError("Could not find " + quote(path) + " at " + files.map(_.path).mkString("\n\t\t\t\tor "), attr))
    }
  }

  def importSignature(file: Path): Either[Error, Program] = {
    val json = file.string    
    for(signature <- deserialize(json, FileLocation(file.path, null, null)).right      
    ) yield signature.asInstanceOf[Program]
  }
}
