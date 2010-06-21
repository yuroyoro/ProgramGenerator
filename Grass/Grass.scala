import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, NoPosition}

sealed abstract class Insn extends ( CED => CED ){
  val pos:Position
}
case class App( m:Int, n:Int, pos:Position ) extends Insn{
  override def apply( ced:CED ) =
    ced.e( m - 1 )( ced.e( n - 1 ), ced )

  override def toString = "App(%s,%s)".format(m, n)
}
case class Abs( m:Int, body:List[App] ,pos:Position ) extends Insn{
  override def apply( ced:CED ) = m match {
    case 1 => CED( ced.c, Fn( body, ced.e ) :: ced.e, ced.d )
    case _ => CED( ced.c, Fn( Abs( m - 1, body, pos ) :: Nil, ced.e ) :: ced.e, ced.d )
  }
  override def toString = "Abs(%s)".format(m)
}

case class CED( c:List[Insn], e:List[Value], d:List[CE] )
case class CE( c:List[Insn], e:List[Value] )

class GrassRuntime( val insn:List[Insn], val source:String){

  val e0 = Out :: Succ :: CharFn('w') :: In :: Nil
  val d0 = CE(Nil, Nil) :: CE( App(1, 1, NoPosition) :: Nil, Nil) :: Nil

  def run:Unit = {
    var c = eval( CED( insn, e0, d0 ) )
    while( c != None ){
      val Some(m) = c
      c = eval( m )
    }
  }

  def eval( ced:CED ) = ced.c match {
    case Nil => ced.d match {
      case Nil => None
      case x::xs  => Some( CED( x.c, ced.e.head:: x.e , xs ))
    }
    case code :: remains => Some( code( CED( remains, ced.e, ced.d )) )
  }
}

abstract class Value extends ( (Value, CED) => CED )
case class Fn(code : List[Insn], env : List[Value]) extends Value {
  override def apply( v:Value, ced:CED ) =
    CED( code , v :: env, CE( ced.c, ced.e ) :: ced.d )
  override def toString = "Fn"
}
case class CharFn(char : Char) extends Value {
  val ChurchTrue  = Fn(
    Abs( 1, App( 3, 2, NoPosition ) :: Nil, NoPosition ) :: Nil,
         Fn( Nil, Nil ) :: Nil )
  val ChurchFalse = Fn( Abs( 1, Nil,  NoPosition) :: Nil,  Nil)

  override def apply( v:Value, ced:CED ) = v match {
    case CharFn( c ) =>
      CED( ced.c, ced.e ::: ( if( char == c ) ChurchTrue else ChurchFalse ) :: Nil, ced.d )
    case _ => throw new Exception("eval error value is not CharFn")
  }
  override def toString = "CharFn(%s, %s)".format( char , char.toInt)
}
object Succ extends Value {
  override def apply( v:Value, ced:CED ) = v match {
    case CharFn( c ) =>
      val char = ( (c + 1) % 256 ).toChar
      CED( ced.c, CharFn( char ) :: ced.e, ced.d )
    case _ => throw new Exception("eval error value is not CharFn")
  }
  override def toString = "Succ"
}
object Out extends Value {
  override def apply( v:Value, ced:CED ) = v match {
    case CharFn( c ) =>
      print(c)
      CED( ced.c, v :: ced.e, ced.d )
    case _ => throw new Exception("eval error value is not CharFn")
  }
  override def toString = "Out"
}
object In extends Value {
  override def apply( v:Value, ced:CED ) ={
    val c = readChar
    CED( ced.c, CharFn( c ) :: ced.e, ced.d )
  }
  override def toString = "In"
}

class GrassParser(
  wTokens:List[String],
  fTokens:List[String],
  vTokens:List[String]
)extends RegexParsers{
  import scala.util.parsing.input.CharSequenceReader._
  override def skipWhitespace = false

  def p(s:String):Parser[String] = s
  def make( tk:List[String] ) = ( p( tk.head ) /: tk.tail ){ _ ||| p( _ ) }

  def wrap[A](p: Parser[A]) = Parser{r => Success(r.pos,  r)} ~ p

  def w :Parser[String] = rep( comment ) ~> ( make( wTokens ) ) <~ rep( comment )
  def f :Parser[String] = rep( comment ) ~> ( make( fTokens ) ) <~ rep( comment )
  def v :Parser[String] = rep( comment ) ~> ( make( vTokens ) ) <~ rep( comment )
  val any :Parser[String] = elem("", _ != EofCh) ^^ { _.toString }

  def token   :Parser[String] = make( wTokens ) ||| make( fTokens ) ||| make( vTokens )
  def comment :Parser[String] = not( token ) <~ any ^^ ( (Unit) => "" )

  def app :Parser[App] = wrap( rep1( f ) ~ rep1( w ) ) ^^
    { case ~( p, x ~ y ) => App( x.size, y.size, p ) }

  def abs :Parser[Abs] = wrap( rep1( w ) ~ rep( app ) ~ rep(v) ) ^^
    { case ~( p, ws ~ body ~ vs ) => Abs( ws.size, body, p ) }

  def prog :Parser[List[Insn]] = rep( abs ) ~ rep( app ) ~ rep( v ) ^^
    { case a ~ p ~ v => a ::: p  }

  def parse( s:String ):Option[GrassRuntime] = parseAll( prog , s ) match {
    case Success( insn, _ )  =>  Some( new GrassRuntime( insn, s ) )
    case Failure( msg, _ ) => { println( msg ); None }
    case Error( msg, _ )   => { println( msg ); None }
  }
  def run( s:String ) = parse( s ) foreach{ _.run }

  def test( s:String ) = parse( s ) foreach{ r => dump( r.insn, 0 ) }

  def dump( x:List[Insn] , n:Int ):Unit = {
    val sp = (for( i <- 0 to n ) yield{ "  " } ).mkString
    x.foreach{ o => o match {
      case Abs( i,b,_ ) => {
        println( sp + "Abs( " + i + ")")
        dump( b , n + 1 )
      }
      case App( i,j,_) => println( sp + "App( " + i + ", " + j + " )")
    }}
  }
}

object Main {
  def main( args:Array[String] ) = {
    val g = new GrassParser( List("w"),List("W"),List("v") )

    println( "print w" )
    val printW = "wWWwwww"
    g.run( printW )
    println( "print x" )
    val printX ="wWWWwwwwWWWw"
    g.run( printX )

    val hw =  """w v wWwwwwWwwwwwwWWWWWwWWWWWwwww v wwwwWWW
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
    wwwwwwwwwWWWWWWwwwwwwwwwwwwwwwwwwwwwwwww"""
    g.test( hw )
    g.run( hw )

    println()
    println( "プログラミング言語 天使ちゃんマジ天使" )
    val angelg = new GrassParser( List("天使ちゃん"),List("マジ"),List("！") )

    println( "print w" )
    val aprintW = "天使ちゃんマジマジ天使ちゃん天使ちゃん天使ちゃん天使ちゃん"
    angelg.run( aprintW )
    println()
    println( "print x" )
    val aprintX ="天使ちゃんマジマジマジ天使ちゃん天使ちゃん天使ちゃん天使ちゃんマジマジマジ天使ちゃん"
    angelg.run( aprintX )
    val angelhw = Source.fromPath( "./AngelHello.grass" ).mkString.replaceAll("\n", "")

    println()
    angelg.test( angelhw)
    angelg.run( angelhw )

    println()
    println( "プログラミング言語 ブブゼラ" )
    val bubuzera= new GrassParser( List("ェ"),List("エ"),List("ベ") )

    println( "print w" )
    val bprintW = "ヴェエエェェェェ"
    bubuzera.run( bprintW )
    println()
    println( "print x" )
    val bprintX ="ヴェエエエェェェェエエエェ"
    bubuzera.run( bprintX )

    println()
    val bhw =  """ヴェ ベ ェエェェェェエェェェェェェエエエエエェエエエエエェェェェ ベ ェェェェエエエ
    ェェエェェエエエヴエエエェェェェエェェ ベ ェエエェエェェ ベ ェェエエェ ベ ェエエエェェエエエエエェェェエェヴェエエエエエエェエエエ
    エエエエェエエエエヴェエエエエエェエエエエエエェエエエエエエエエエエエエエエェェェェェェェェエェエェエエエェェエエエエェェ
    ェェェェェエエエエエヴェェェェェェェエエエエエエェェェェェェェエエエエエエエエエエエエエエエエエエエエエェェェェェヴェェェェ
    ェェェェェェェェェェェェエェェェェェェェェェェェェェェェェェェェェエェェェェェェェェェェェェェエェェェェェェエエェェェェ
    ェェエエエェェェェヴェェエエエエエエェェェェェェェェェェェェェェェェェェェェエェェェェェェェェェェエエェェェェエヴエエェェェ
    ェエエエエェエエエエエェェェェェェェェェェェェェェェェェェエエエエエエエエエエエェエェェェエエェエエエェエエエヴエェエエエ
    エエエエエエエエエヴエエエエエェェェェェェェェェェェェェェェェェエェェェェェェェェェェェェェェェェェェェェェェェェェェェェ
    ェェェェェェェェェヴェエェェェェェェェェェェェエエェェェェェェェェェェェエエエェェェェェェェエエエエェエエヴエエヴエェェェェェ
    ェェェエエエエヴエエェェェェェェェェェェェェェェェェェェェェェエエエエエエエェェェェェェェェェェェェェェェェェェェェェェェ
    ェェェェェエエヴエエエエエエエェェェェェェェェエェェェェェェェェェェェェェェェェェェェェェェェェェェェェェェェェエエェェェ
    ェェェェェェェヴェェェエエエェェェェェェェェェェェェェエエエエェェェェェェェェエエエエエェェェェェェェェェヴェェェェェェェェ
    ェェェェェェェヴェェエエエエエエェェェェェェェェェェェェェェェェェェェェェェェェェ"""
    bubuzera.test( bhw )
    bubuzera.run( bhw )
  }
}

