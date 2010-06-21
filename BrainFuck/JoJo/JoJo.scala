import scala.util.parsing.combinator._

sealed abstract class Op {
  def exec(jojo:JoJo)
  val token:String
  override def toString = token
}

case class Inc(token:String) extends Op {
  def exec( jojo:JoJo ) = jojo.inc
}
case class Dec(token:String) extends Op{
  def exec( jojo:JoJo ) = jojo.dec
}
case class Next(token:String) extends Op{
  def exec( jojo:JoJo ) = jojo.next
}
case class Prev(token:String) extends Op{
  def exec( jojo:JoJo ) = jojo.prev
}
case class Get(token:String) extends Op{
  def exec( jojo:JoJo ) = jojo.get
}
case class Put(token:String) extends Op{
  def exec( jojo:JoJo ) = jojo.put
}
case class NoOp() extends Op{
  val token = ""
  def exec( jojo:JoJo ) = {}
}
case class Loop( op:List[Op]) extends Op{
  val token = ""
  override def toString = "[" + op.mkString + "]"
  def exec( jojo:JoJo ) = {
    while( jojo.m != 0 ){ op.foreach( _.exec( jojo ) ) }
  }
}

class JoJo( val op:List[Op]){
  val size = 30000
  val offset = size / 2

  var mem = new Array[Int]( size )
  var pt = 0

  def m = mem( pt + offset )
  def m( b:Int ) = { mem( pt + offset ) = b }

  def inc  = { m( m + 1 ) }
  def dec  = { m( m - 1 ) }
  def next = { pt = pt + 1 }
  def prev = { pt = pt - 1 }
  def get = m( readChar.toInt )
  def put = print( m.toChar )

  def reset = {
   pt = 0
   mem = new Array[Int]( size )
  }

  def run = op.foreach{ _.exec( this ) }
}

object JoJo extends RegexParsers{
  override def skipWhitespace = false
  import scala.util.parsing.input.CharSequenceReader._

  def inc :Parser[Op] = ("オラ") ^^ ( x => Inc( x ) )
  def dec :Parser[Op] = ("無駄") ^^ ( x => Dec( x ) )
  def next:Parser[Op] = ("スターフィンガ" | "やれやれだぜ" ) ^^ ( x => Next( x ) )
  def prev:Parser[Op] = ("ロードローラ" | "貧弱") ^^ ( x => Prev( x ) )
  def put :Parser[Op] = ("ハーミットパープル") ^^ ( x => Put( x ) )
  def get :Parser[Op] = ("新手のスタンド使いか") ^^ ( x => Get( x ) )

  def start :Parser[Op] = ("あ・・・ありのまま今起こったことを話すぜ" ) ^^ ( x => NoOp() )
  def end   :Parser[Op] = ("ザ・ワールド" ) ^^ ( x => NoOp() )
  val any   :Parser[Op] = elem("", _ != EofCh) ^^ ( x => NoOp() )

  def token       :Parser[Op] = inc  ||| dec ||| next |||
                                prev ||| get ||| put
  def comment     :Parser[Op] = not( token | start | end ) <~ any ^^ ( x => NoOp() )

  def loop        :Parser[Op] = ( start ~> rep(instruction) <~ end ) ^^ {
   op => new Loop( op )
  }

  def instruction :Parser[Op] = loop | token | comment
  def brainfuck   :Parser[List[Op]] = rep(instruction)

  def parse( s:String ):Option[JoJo] = parseAll( brainfuck , s ) match {
    case Success( op, _ )  =>  Some( new JoJo( op ) )
    case Failure( msg, _ ) => { println( msg ); None }
    case Error( msg, _ )   => { println( msg ); None }
  }
  def run( s:String ) = parse( s ) foreach{ _.run }
}

/**
 *
object Main {
  def main( args:Array[String] ) =  JoJo.run("""
オラオラオラオラオラオラオラオラオラッ！！

「あ・・・ありのまま今起こったことを話すぜ
俺は奴の前で階段を登っていたと思ったら、いつの間にか降りていた
な…何を言っているのかわからねーと思うが、
俺も何をされたのかわからなかった…
頭がどうにかなりそうだった…催眠術だとか超スピードだとか、
そんなチャチなもんじゃあ断じてねえ。
もっと恐ろしいものの片鱗を味わったぜ…」

スターフィンガー！
オラオララララ！
オラッ！オラオラララララオラオラオラァ！！！
スターフィンガー！！！
オラァオラオラオラオラオラオラッオラ！！
オラオラァァァァァオララララララララララ！
スターフィンガー！

オラオラオラオラオラ！　つけの領収書だぜ！

力比べというわけか！
知るがいい…！『ザ・ ワールド』の真の能力は…まさに！『世界を支配する』能力だと言うことを！

「ロードローラだ！ロードローラだ！ロードローラだ！」
無駄ッッッ！

ザ・ワールドッッ

スターフィンガー！
「ハーミットパープル」
スターフィンガー
オラオラ！

「ハーミットパープル」

オラオラオラオラオラオラオラ
ハーミットパープル！ハーミットパープル！

オラオラオラ

ハーミットパープル！
スターフィンガー！

無駄ァ！
ハーミットパープル

無駄！無駄！
無駄無駄無駄無駄無駄無駄無駄無駄無駄無駄
WRYYYYYYYYYYYYYY！
“ジョースター・エジプト・ツアー御一行様”は貴様にとどめを刺して全滅の最後というわけだな

ハーミットパープル！
ロードローラだ！

オーラオラオーラオラオラオラオーラオラオラオラオラッ！
ハーミットパープル！
無駄無駄無駄無駄無駄無駄無駄無駄ッ
ハーミットパープル！
オラオラオラアアアアアアアア！
ハーミットパープル！
無駄ッ無駄ッ無駄ッ無駄無駄無駄ァツ！

ハーミットパープル

もうおそい！　脱出不可能よッ！ 無駄無駄無駄無駄無駄無駄無駄無駄ぁぁ！
ハーミットパープル！

最高に『ハイ！』ってやつだアアアアア！アハハハハハハハハハーッ！！
スターフィンガー
オラ
ハーミットパープル！

てめーの敗因は・・・たったひとつだぜ・・・ＤＩＯ　たったひとつの単純（シンプル）な答えだ・・・　『てめーは　おれを怒らせた』
""")
}

