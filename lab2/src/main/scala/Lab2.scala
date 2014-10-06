object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   * Samuel Carnes
   * 
   * Partner: Dillon Drenzek
   */

  /*
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case S(s) => try s.toDouble catch{case _:Throwable => Double.NaN}
      case B(b) => if(b) 1 else 0
      case Undefined => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Undefined => false
      case N(n) => n match {
        case 0 => false
        case _ if n.isNaN() => false
        case _ => true
      }
      case S(s) => true
      case B(b) => b
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case B(b) => if(b) "true" else "false"
      case N(n) => n.toString
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case N(_) | B(_) | S(_) | Undefined => e
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined
      case Unary(uop, e1) => uop match {
        case Neg => N(-toNumber(eToVal(e1)))
        case Not => B(!toBoolean(eToVal(e1)))
        case _ => throw new UnsupportedOperationException
      }
      
      case Binary(bop, e1, e2) => bop match {
        case Plus => { (e1, e2) match{
          case (S(_), _) | (_, S(_)) => S(toStr(eToVal(e1)) + toStr(eToVal(e2)))
          case(_,_) => N(toNumber(eToVal(e1)) + toNumber(eToVal(e2)))
        	}
        }
        case Minus => N(toNumber(eval(e1)) - toNumber(eval(e2)))
        case Times => N(toNumber(eval(e1)) * toNumber(eval(e2)))
        case Div => N(toNumber(eval(e1)) / toNumber(eval(e2)))
        
        case Eq => B(eval(e1) == eval(e2))        
        case Ne => B(eval(e1) != eval(e2))
        
        case Lt => B(toNumber(eval(e1)) < toNumber(eval(e2)))
        case Le => B(toNumber(eval(e1)) <= toNumber(eval(e2)))
        case Gt => B(toNumber(eval(e1)) > toNumber(eval(e2)))
        case Ge => B(toNumber(eval(e1)) >= toNumber(eval(e2)))
        
        case And => if (!toBoolean(eval(env, e1))) eval(env, e1) else (eval(env, e2))           
        case Or => if (toBoolean(eval(env, e1))) eval(env, e1) else (eval(env, e2))
        			   	
        case Seq => eval(e1); eval(e2)
        
        case _ => throw new UnsupportedOperationException          
      }
      
      case If(e1, e2, e3) => if (toBoolean(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      
      case Var(x) => get(env, x)  
      

      case _ => throw new UnsupportedOperationException
    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }

}