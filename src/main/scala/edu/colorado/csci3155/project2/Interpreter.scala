package edu.colorado.csci3155.project2

/*
   A Lettuce interpreter with evalExpr function that has missing cases to be handled. See TODOs below.
 */

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
        case Line(l) => { //TODO: Handle a line object
            val x=evalExpr(l,env) 
            x match{
                case NumValue(d) => { 
                    val p=new Polygon(List((0.0,0.0 ), (d,0.0)) )
                    val c =new MyCanvas(List(p) )
                    FigValue(c) 
                }
                case _ => throw new IllegalArgumentException("Line: no NumValue")
            }
        }

        case EquiTriangle(sideLength) =>{ // TODO: Handle Equilateral Triangle
            val x =evalExpr(sideLength,env )
            x match{
                case NumValue(d)=>{
                    val p=new Polygon(List((0,0),(0,d),(d/ 2,math.sqrt(3*d)/ 2)) )
                    val c= new MyCanvas(List(p))
                    FigValue(c)
                }
                case _ => throw new IllegalArgumentException("EquiTriangle: no NumValue")
             }
           
        } 

        case Rectangle(sideLength) =>{// TODO: Handle square given the side length
            val x =evalExpr( sideLength,env)
            x match {
                case NumValue(d) =>{
                    val p=new Polygon(List((0,0),(0,d ) , (d, d),(d, 0))) 
                    val c=new MyCanvas(List(p))
                    FigValue(c)
                }

                case _ => throw new IllegalArgumentException("Rectangle: no NumValue")
            } 
         } 

        case Circle(rad) => { //TODO: Handle circle
            val x=evalExpr(rad , env) 
            x match {  
                case NumValue(d)=>{
                    val ci= new MyCircle( (d ,d), d) 
                    val c =new MyCanvas( List(ci ))
                    FigValue(c)
                }
                case _ => throw new IllegalArgumentException("Circle: no NumValue")
            }
        } 

        case Plus (e1, e2) =>{// TODO: Handle addition of numbers or figures
            val x1=evalExpr(e1,env )
            val x2=evalExpr(e2, env) 
            (x1, x2 )match{
                case (NumValue(n1),NumValue(n2))=>NumValue(n1+ n2)
                case ( FigValue(n1),FigValue( n2))=>FigValue(n1 overlap n2)
                case _ => throw new IllegalArgumentException("Plus: no NumValue and/or FigValue")
            }
        } 

        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)

        case Mult(e1, e2) =>{ // TODO: Handle multiplication of numbers or figures
            val x1=evalExpr( e1,env) 
            val x2 = evalExpr(e2, env )
            (x1 , x2)  match{ 
                 case (NumValue(n1), NumValue(n2))=>NumValue(n1* n2 )
                case (FigValue(n1) ,FigValue(n2) )=> FigValue(n1 placeRight n2)
                case _ => throw new IllegalArgumentException("Minus: no NumValue and/or FigValue")
            } 

        } 

        case Div(e1, e2) =>{ // TODO: Handle division
            val x1= evalExpr(e1, env) 
            val x2 = evalExpr(e2 ,env)  
            (x1 ,x2) match{
                case(NumValue(n1) ,NumValue(n2))=> NumValue(n1/n2)
                case(FigValue(n1),FigValue(n2))=> FigValue(n1 placeTop n2)
                case(FigValue(n1) ,NumValue(n2))=>FigValue(n1 rotate n2) 
                case _ => throw new IllegalArgumentException( "Division: no NumValue and/or FigValue")
            } 
        } 


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

        case FunDef(x, e) =>{//TODO: Handle function definitions
            Closure( x,e,env) 

        } 

        case LetRec(f, x, e1, e2) => {  // TODO: Handle recursive functions -- look at Environment.scala
            val env2=ExtendREC(f,x , e1 ,env )
            evalExpr(e2,env2) 
         }


        case FunCall(fCallExpr, arg) =>{ // TODO: Handle function calls
            val x1=evalExpr(fCallExpr, env)
            val x2=evalExpr(arg ,env)
            x1 match{ 

                case Closure (x_new,ex_closure ,env_closed)  =>{
                    
                    val env2=Extend(x_new,x2,env_closed) 
                    evalExpr(ex_closure, env2)
                }
                case _ => throw new IllegalArgumentException(s"Function call error: expression $fCallExpr does not evaluate to a closure")
            }
         } 


    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
