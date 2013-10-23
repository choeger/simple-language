package de.tuberlin.uebb.sl2.modules

trait SignatureSerializer {
  this : Syntax with Errors =>

  def serialize(in : AST) : String
  
  def deserialize(in : String, location : Location = NoLocation) : Either[Error, AST]

}
