package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] =
      ins match {
        case PushI(v) => v :: stack
        case PopI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: rest => rest
        }
        case AddI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: List() => throw new IllegalArgumentException
          case a :: b :: rest => (a + b) :: rest
        }
        case SubI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: List() => throw new IllegalArgumentException
          case a :: b :: rest => (b - a) :: rest
        }
        case MultI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: List() => throw new IllegalArgumentException
          case a :: b :: rest => (b * a) :: rest
        }
        case DivI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: List() => throw new IllegalArgumentException
          case a :: b :: rest => (b / a) :: rest
        }
        case ExpI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: b => math.exp(a) :: b
        }
        case LogI => stack match {
          case List() => throw new IllegalArgumentException
          case a :: b => math.log(a) :: b
        }
        case SinI  => stack match{
          case List() => throw new IllegalArgumentException
          case a::b => math.sin(a)::b
        }
        case CosI => stack match{
          case List() => throw new IllegalArgumentException
          case a::b => math.cos(a)::b
        }
      }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double ={
      if(instructionList.isEmpty) throw new IllegalArgumentException
      else {
        val ns = instructionList.foldLeft(List[Double]()) (emulateSingleInstruction(_, _))
        ns.head
      }
    }

}