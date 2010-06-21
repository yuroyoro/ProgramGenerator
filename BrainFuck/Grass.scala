import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, NoPosition}

object G{

sealed abstract class Op {
  val pos:Position
}

case class App( n:Int, m:Int, pos:Position ) extends Op{
}
case class Abs( n:Int, body:List[App] ,pos:Position ) extends Op{
}

abstract class Value
class Fn(code : List[Op], env : List[Value]) extends Value
class CharFn(char : Char) extends Value
object Succ extends Value
object Out extends Value
object In extends Value

class CED( c:List[Op], e:List[Value], d:List[CE] ){
}
class CE( c:List[Op], e:List[Value] )

class GrassRuntime( val op:List[Op],val source:String){
  val e0 = In :: new CharFn('w') :: Succ :: Out :: Nil
  val d0 = List( new CE(Nil, Nil), new CE(List(App(1, 1, NoPosition)), Nil))

  def run = new CED( op, e0, d0 )
}

class GrassParser(
  wTokens:List[String],
  WTokens:List[String],
  vTokens:List[String]
)extends RegexParsers{
  import scala.util.parsing.input.CharSequenceReader._
  override def skipWhitespace = false

  def p(s:String):Parser[String] = s
  def make( tk:List[String] ) = ( p( tk.head ) /: tk.tail ){ _ ||| p( _ ) }

  def wrap[A](p: Parser[A]) = Parser{r => Success(r.pos,  r)} ~ p

  def w :Parser[String] = rep( comment ) ~> ( make( wTokens ) ) <~ rep( comment )
  def W :Parser[String] = rep( comment ) ~> ( make( WTokens ) ) <~ rep( comment )
  def v :Parser[String] = rep( comment ) ~> ( make( vTokens ) ) <~ rep( comment )
  val any :Parser[String] = elem("", _ != EofCh) ^^ { _.toString }

  def token   :Parser[String] = make( wTokens ) ||| make( WTokens ) ||| make( vTokens )
  def comment :Parser[String] = not( token ) <~ any ^^ ( (Unit) => "" )

  def app :Parser[App] = wrap( rep1( W ) ~ rep1( w ) ) ^^
    { case ~( p, x ~ y ) => App( x.size, y.size, p ) }

  def abs :Parser[Abs] = wrap( rep1( w ) ~ rep( app ) ~ rep(v) ) ^^
    { case ~( p, ws ~ body ~ vs ) => Abs( ws.size, body, p ) }

  def prog :Parser[List[Op]] = rep( abs ) ~ rep( app ) ~ rep( v ) ^^
    { case a ~ p ~ v => a ::: p  }

  def parse( s:String ):Option[GrassRuntime] = parseAll( prog , s ) match {
    case Success( op, _ )  =>  Some( new GrassRuntime( op, s ) )
    case Failure( msg, _ ) => { println( msg ); None }
    case Error( msg, _ )   => { println( msg ); None }
  }

  def test( s:String ) = parse( s ) foreach{ r => dump( r.op, 0 ) }

  def dump( x:List[Op] , n:Int ):Unit = x.foreach{ o => o match {
    case Abs( i,b,_ ) => {
      println( stringWrapper("%sAbs( %s )").format( stringWrapper("  ") * n , i ) )
      dump( b , n + 1 )
    }
    case App( i,j,_) => println( stringWrapper("%sApp( %s, %s )").format( stringWrapper("  ") * n, i, j ) )
  }}
}

}

val g = new G.GrassParser( List("w"),List("W"),List("v") )

g.parseAll(g.prog, "wvwwww" )
g.parseAll(g.prog, "wWWwwww" )
g.parseAll(g.prog, """w v wWwwwwWwwwwwwWWWWWwWWWWWwwww v wwwwWWW
wwWwwWWWWWWwwwwWww v wWWwWww v wwWWw v wWWWwwWWWWWwwwWwwWWWWWWwWWW
WWWWwWWWWwWWWWWwWWWWWWwWWWWWWWWWWWWWWwwwwwwwwWwWwWWWwwWWWWww
wwwwwWWWWWwwwwwwwWWWWWWwwwwwwwWWWWWWWWWWWWWWWWWWWWWwwwwwwwww
wwwwwwwwwwwwWwwwwwwwwwwwwwwwwwwwwWwwwwwwwwwwwwwWwwwwwwWWwwww
wwWWWwwwwwwWWWWWWwwwwwwwwwwwwwwwwwwwwWwwwwwwwwwwWWwwwwWWWwww
wWWWWwWWWWWwwwwwwwwwwwwwwwwwwWWWWWWWWWWWwWwwwWWwWWWwWWWWwWWW
WWWWWWWWWWWWWWwwwwwwwwwwwwwwwwwWwwwwwwwwwwwwwwwwwwwwwwwwwwww
wwwwwwwwwwWwwwwwwwwwwwWWwwwwwwwwwwwWWWwwwwwwwWWWWwWWWWWwwwww
wwwWWWWWWwwwwwwwwwwwwwwwwwwwwwWWWWWWWwwwwwwwwwwwwwwwwwwwwwww
wwwwwWWWWWWWWWwwwwwwwwWwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwWWwww
wwwwwwwwwwWWWwwwwwwwwwwwwwWWWWwwwwwwwwWWWWWwwwwwwwwwwwwwwwww
wwwwwwwwwWWWWWWwwwwwwwwwwwwwwwwwwwwwwwww""")

