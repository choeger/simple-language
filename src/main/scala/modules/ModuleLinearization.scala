package de.tuberlin.uebb.sl2.modules

import de.tuberlin.uebb.sl2.modules._

import scalax.file._

/**
 * Sort the nodes in a given set of edges topologically.
 */
trait ModuleLinearization
	extends Object
	with Configs
	with Errors
	with ModuleResolver
	with Syntax {
  
  sealed case class Module(
    val name: String = "",
    val source: Path = null,
    val signature: Path = null,
    val js: Path = null,
    val compile: Boolean = false) {
    
    override def equals(obj: Any) = {
      (obj != null &&
        obj.isInstanceOf[Module] &&
        obj.asInstanceOf[Module].name == this.name)
    }

    override def hashCode() = { name.hashCode }

    override def toString() = {
      "(Module " + quote(name) + " (compile=" + compile + "))\n"
    }
  }

  /**
   * creates a module compilation unit object from a file name
   * (can either be from /std)
   */
  def moduleFromName(name: String, config: Config): Either[Error, Module] = {
    if (name.startsWith(standardLibName+"/")) {
      // load std/ library from resources directory
      val nameEnd = name.replace(standardLibName+"/", "")
      val stdSource = getLibResource(nameEnd + ".sl")
      if (stdSource == null)
        Left(GenericError("Could not find source of standard library: "
          + quote((standardLibPath / name).toString)))
      else {
        val p = Path(stdSource)
        Right(Module(nameEnd, p, p.sibling(nameEnd + "sl.signature"), p.sibling(nameEnd + ".sl.js")))
      }
    } else {
      // load ordinary files relative to source- and classpath
      Right(Module(name, config.sourcepath / (name + ".sl"),
        config.sourcepath / (name + ".sl.signature"), config.sourcepath / (name + ".sl.js")))
    }
  }

  /**
   * Sorts the modules in the map from modules to sets of their respective
   * required modules topologically and returns the sorted sequence of modules.
   */
  def topoSort(predecessors: scala.collection.Map[Module, Set[Module]]): Either[Error, Iterable[Module]] = {
    topoSort(predecessors, Seq())
  }

  /**
   * Sorts the modules in the map from modules to sets of required modules topologically.
   *
   * Recursively sort topologically: add the modules to the set of done modules
   * that have no predecessors, remove them from the map of predecessors and
   * recurse until no modules are left. Return an error, if modules are left.
   */
  def topoSort(predecessors: scala.collection.Map[Module, Set[Module]],
    done: Iterable[Module]): Either[Error, Iterable[Module]] = {
    val (hasNoPredecessors, hasPredecessors) = predecessors.partition { _._2.isEmpty }
    if (hasNoPredecessors.isEmpty) {
      if (hasPredecessors.isEmpty) {
        Right(done)
      } else {
        Left(CircularDependencyError("Circular dependency between modules " +
          (for (key <- hasPredecessors.keys) yield key.source.path).mkString(", ")))
      }
    } else {
      val found = hasNoPredecessors.map { _._1 }
      topoSort(hasPredecessors.mapValues { _ -- found }, done ++ found)
    }
  }
}
