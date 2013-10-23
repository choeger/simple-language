package de.tuberlin.uebb.sl2.modules

import scalax.file.Path;

/**
 * A ModuleResolver is able to find and load the modules a program
 * references.
 */
trait ModuleResolver {
  this: Syntax
  with Errors
  with Configs =>

  sealed abstract class ResolvedImport(
      val path:String,
      val file:Path,
      val ast:Import)
  
  sealed abstract class ResolvedModuleImport(
      val name:String,
      override val path:String,
      override val file:Path,
      val signature:Program,
      override val ast:Import) extends ResolvedImport(path, file, ast)
      
  case class ResolvedUnqualifiedImport(
      override val path: String,
      override val file: Path,
      override val signature: Program,
      override val ast: UnqualifiedImport) extends ResolvedModuleImport(
          "$$"+path.replace('/', '$'), path, file, signature, ast)
  
  case class ResolvedQualifiedImport(
      override val name: ModuleVar,
      override val path: String,
      override val file: Path,
      override val signature: Program,
      override val ast: QualifiedImport)
    extends ResolvedModuleImport(name, path, file, signature, ast)
    
  case class ResolvedExternImport(
      override val path: String,
      override val file: Path,
      override val ast: ExternImport)
    extends ResolvedImport(path, file, ast)
    
  def inferDependencies(program: AST, config: Config): Either[Error, List[ResolvedImport]]
  def resolveDependencies(program: AST, config: Config): Either[Error, Set[String]]
  
  def checkImports(imports : List[Import]) : Either[Error, Unit]

  def findImportResource(path: String, config: Config, attr: Attribute): Either[Error, Path]

  def standardLibName: String

  def standardLibPath: Path

  def getLibResource(path: String) : Path = standardLibPath / path
}
