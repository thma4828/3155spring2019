package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = e match{
      case Const(c) => List(PushI(c))
      case Plus(a ,b) => {
        val l1 = compileToStackMachineCode(a)
        val l2 = compileToStackMachineCode(b)
        val l3 = l1:::l2
        l3:::List(AddI)
      }
      case Minus(a, b) => {
        val l1 = compileToStackMachineCode(a)
        val l2 = compileToStackMachineCode(b)
        val l3 = l1:::l2
        l3:::List(SubI)
      }
      case Div(a, b) => {
        val l1 = compileToStackMachineCode(a)
        val l2 = compileToStackMachineCode(b)
        val l3 = l1:::l2
        l3:::List(DivI)
      }
      case Mult(a, b) => {
        val l1 = compileToStackMachineCode(a)
        val l2 = compileToStackMachineCode(b)
        val l3 = l1:::l2
        l3:::List(MultI)
      }
      case Exp(a) => {
        val l1 = compileToStackMachineCode(a)
        l1:::List(ExpI)
      }
      case Log(a) => {
        val l1 = compileToStackMachineCode(a)
        l1:::List(LogI)
      }case Sine(a) => {
        val l1 = compileToStackMachineCode(a)
        l1:::List(SinI)
      }case Cosine(a) => {
        val l1 = compileToStackMachineCode(a)
        l1:::List(CosI)
      }

    }
}
