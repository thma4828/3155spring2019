package edu.colorado.csci3155.project2
//Theodore Margoles, 3155, Project 2
//All work is my own! ~Theodore
object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => {
            val e1 = evalExpr(l, env)
            e1 match {
                case NumValue(f) => FigValue(new MyCanvas(List(Polygon(List((0.0, 0.0), (f, 0.0))))))
                case _ => throw new IllegalArgumentException
            }
        }
        case EquiTriangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match{
                case NumValue(f) => {
                    val T = Polygon(List((0.0, 0.0),(0.0, f),(f*0.5, math.sqrt(3)*f*0.5)))
                    FigValue(new MyCanvas(List(T)))
                }
                case _ => throw new IllegalArgumentException
            }
        } // TODO: Handle Equilateral Triangle
        case Rectangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match{
                case NumValue(f) => {
                    val R = Polygon(List( (0.0, 0.0), (0.0, f), (f, 0.0), (f, f)))
                    FigValue(new MyCanvas(List(R)))
                }
                case _ => throw new IllegalArgumentException
            }
        }// TODO: Handle square given the side length
        case Circle(rad) => {
            val v = evalExpr(rad, env)
            v match{
                case NumValue(f) => {
                    val C = MyCircle((f, f), f)
                    FigValue(new MyCanvas(List(C)))
                }
                case _ => throw new IllegalArgumentException
            }
        } //TODO: Handle circle
        case Plus (e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
                case (FigValue(o1), FigValue(o2)) => FigValue(o1.overlap(o2))
                case _ => throw new IllegalArgumentException
            }
        }
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match{
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
                case (FigValue(o1), FigValue(o2)) => FigValue(o1.placeRight(o2))
                case _ => throw new IllegalArgumentException
            }
        }// TODO: Handle multiplication of numbers or figures
        case Div(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match{
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 / f2)
                case (FigValue(o1), FigValue(o2)) => FigValue(o1.placeTop(o2))
                case (FigValue(o1), NumValue(f1)) => FigValue(o1.rotate(f1))
                case _ => throw new IllegalArgumentException
            }
        } // TODO: Handle division
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e1) => {
            Closure(x, e1, env)
        } //TODO: Handle function definitions

        case LetRec(f, x, e1, e2) => {
            //combined let and fundef
            val env2 = ExtendREC(f, x, e1, env)
            evalExpr(e2, env2)
        } // TODO: Handle recursive functions -- look at Environment.scala

        case FunCall(fCallExpr, arg) => {
            val v1 = evalExpr(fCallExpr, env)
            val v2 = evalExpr(arg, env)
            v1 match{
                case Closure(param, closure_ex, closed_env) => {
                    val new_env = Extend(param, v2, closed_env)
                    evalExpr(closure_ex, new_env)
                }
            }
        } // TODO: Handle function calls
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
