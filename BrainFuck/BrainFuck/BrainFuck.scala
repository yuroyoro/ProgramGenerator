import scala.util.parsing.combinator._

sealed abstract class Op { def exec(bf:BF) }

case class Inc(token:String) extends Op {
  def exec( bf:BF ) = bf.inc
}
case class Dec(token:String) extends Op{
  def exec( bf:BF ) = bf.dec
}
case class Next(token:String) extends Op{
  def exec( bf:BF ) = bf.next
}
case class Prev(token:String) extends Op{
  def exec( bf:BF ) = bf.prev
}
case class Get(token:String) extends Op{
  def exec( bf:BF ) = bf.get
}
case class Put(token:String) extends Op{
  def exec( bf:BF ) = bf.put
}
case class NoOp() extends Op{
  def exec( bf:BF ) = {}
}
case class Loop( op:List[Op]) extends Op{
  def exec( bf:BF ) = {
    while( bf.m != 0 ){ op.foreach( _.exec( bf ) ) }
  }
}

class BF( op:List[Op]){

  var mem = new Array[Int](30000)
  var pt = 0

  def m = mem( pt )
  def m( b:Int ) = { mem( pt ) = b }

  def inc  = { m( m + 1 ) }
  def dec  = { m( m - 1 ) }
  def next = { pt = pt + 1 }
  def prev = { pt = pt - 1 }
  def get = m( readChar.toInt )
  def put = print( m.toChar )

  def reset = {
   pt = 0
   mem = new Array[Int](30000)
  }

  def run = op.foreach{ _.exec( this ) }
}

object BF extends RegexParsers{
  import scala.util.parsing.input.CharSequenceReader._

  def inc :Parser[Op] = "+" ^^ ( x => Inc( x ) )
  def dec :Parser[Op] = "-" ^^ ( x => Dec( x ) )
  def next:Parser[Op] = ">" ^^ ( x => Next( x ) )
  def prev:Parser[Op] = "<" ^^ ( x => Prev( x ) )
  def put :Parser[Op] = "." ^^ ( x => Put( x ) )
  def get :Parser[Op] = "," ^^ ( x => Get( x ) )

  def start :Parser[Op] = "[" ^^ ( x => NoOp() )
  def end   :Parser[Op] = "]" ^^ ( x => NoOp() )
  val any   :Parser[Op] = elem("", _ != EofCh) ^^ ( x => NoOp() )

  def token       :Parser[Op] = inc  ||| dec ||| next |||
                                prev ||| get ||| put
  def comment     :Parser[Op] = not( token | start | end ) <~ any ^^ ( x => NoOp() )

  def loop        :Parser[Op] = ( start ~> rep(instruction) <~ end ) ^^ {
   op => new Loop( op )
  }

  def instruction :Parser[Op] = loop | token | comment
  def brainfuck   :Parser[List[Op]] = rep(instruction)

  def parse( s:String ):Option[BF] = parseAll( brainfuck , s ) match {
    case Success( op, _ )  =>  Some( new BF( op ) )
    case Failure( msg, _ ) => { println( msg ); None }
    case Error( msg, _ )   => { println( msg ); None }
  }
  def run( s:String ) = parse( s ) foreach{ _.run }
}

object Main {
  def main( args:Array[String] ) = BF.run(">++++++ほげほげ+++[<+++++ほげほげ+++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>++++++ほげほげ+++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]+++++ほげほげ+++++.")
}

