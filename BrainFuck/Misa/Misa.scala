import scala.util.parsing.combinator._

// http://homepage2.nifty.com/kujira_niku/okayu/misa.html
sealed abstract class Op {
  def exec(misa:Misa)
  val token:String
  override def toString = token
}

case class Inc(token:String) extends Op {
  def exec( misa:Misa ) = misa.inc
}
case class Dec(token:String) extends Op{
  def exec( misa:Misa ) = misa.dec
}
case class Next(token:String) extends Op{
  def exec( misa:Misa ) = misa.next
}
case class Prev(token:String) extends Op{
  def exec( misa:Misa ) = misa.prev
}
case class Get(token:String) extends Op{
  def exec( misa:Misa ) = misa.get
}
case class Put(token:String) extends Op{
  def exec( misa:Misa ) = misa.put
}
case class NoOp() extends Op{
  val token = ""
  def exec( misa:Misa ) = {}
}
case class Loop( op:List[Op]) extends Op{
  val token = ""
  override def toString = "[" + op.mkString + "]"
  def exec( misa:Misa ) = {
    while( misa.m != 0 ){ op.foreach( _.exec( misa ) ) }
  }
}

class Misa( val op:List[Op]){
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

object Misa extends RegexParsers{
  import scala.util.parsing.input.CharSequenceReader._

  def inc :Parser[Op] = ("+" | "あ" | "ぁ" | "お" | "ぉ" )^^ ( x => Inc( x ) )
  def dec :Parser[Op] = ("-" | "っ" | "ッ") ^^ ( x => Dec( x ) )
  def next:Parser[Op] = (">" | "→" | "～" | "ー")  ^^ ( x => Next( x ) )
  def prev:Parser[Op] = ("<" | "←" | "★" | "☆")  ^^ ( x => Prev( x ) )
  def put :Parser[Op] = ("." | "！" )^^ ( x => Put( x ) )
  def get :Parser[Op] = ("," | "？" )^^ ( x => Get( x ) )

  def start :Parser[Op] = ("[" | "「" | "『") ^^ ( x => NoOp() )
  def end   :Parser[Op] = ("]" | "」" | "』") ^^ ( x => NoOp() )
  val any   :Parser[Op] = elem("", _ != EofCh) ^^ ( x => NoOp() )

  def token       :Parser[Op] = inc  ||| dec ||| next |||
                                prev ||| get ||| put
  def comment     :Parser[Op] = not( token | start | end ) <~ any ^^ ( x => NoOp() )

  def loop        :Parser[Op] = ( start ~> rep(instruction) <~ end ) ^^ {
   op => new Loop( op )
  }

  def instruction :Parser[Op] = loop | token | comment
  def brainfuck   :Parser[List[Op]] = rep(instruction)

  def parse( s:String ):Option[Misa] = parseAll( brainfuck , s ) match {
    case Success( op, _ )  =>  Some( new Misa( op ) )
    case Failure( msg, _ ) => { println( msg ); None }
    case Error( msg, _ )   => { println( msg ); None }
  }
  def run( s:String ) = parse( s ) foreach{ _.run }
}

object Main {
  def main( args:Array[String] ) =  Misa.run("""
ごっ、ごぉおっ、ご～きげんよおぉおおぉおほっ。ほおぉおぉおっ。

「ごきげん☆みゃぁああ”あ”ぁ”ぁああ～っ」

さわやかな朝の☆ご挨拶！　お挨拶がっ。
澄みきった青空にこだましちゃうぉ～ああぉおおおぉん。

「は、はひっ、はろおぉっ☆わぁるどおおぉっぉ～っ」

こ、この文章は☆おサンプル！　おおぉおぉおおサンプルプログラム！！
どんなおプログラム言語でも基本のご挨拶させていただくのぉぉおッ！

「ぽうっ」

長々と書くのがこ、ここでの～、ここでのぉおおぉおぉぉおたしなみぃぃいぃ。

「長いぃ。長すぎましゅう。ご挨拶にこんなプログラム長すぎまひゅぅうぅ☆
　んおおぉぉ、ばかになる、おばかになっちゃいましゅ～ッ」

長いのがっ、バッファの奥まで入ってきましゅたぁあぁあっ！
ばっふぁ☆溢れちゃいまひゅぅ～。あみゃぁあ”あ”ぁ”ぁああ”あ”ぁぁ。

「で、出ます☆　んおおぉぉおおっ、エラー出ちゃいまひゅっ」

ほひぃ☆！　え、えらーっ、んお”お”ぉお”お”ぉおぉおおぉっっ。

「出た☆　出た出た出た出たぁぁあっ　えらあぴゅるーっって出たあぁっ」

はしたない☆！　ぉおおぉはしたないっ！　おはしたない言語ですっっっっっっっ！
おほっほおぉっっっほおぉっっっっっっっっっ！

「えらあらいしゅきぃぃぃいぃっっ」

止まらない　すごい　エラーみるく
こってりしたのがいっぱい出てるよぉぉぉおおぉぉおおぉぉおっっ。

「んほぉっ☆ っおぉぉぉおお国が分からなくなっちゃいまひゅう～っ」

ま、まだ出るぅ☆　出てるのおぉっ☆　エラーまだまだ出ましゅぅぅ！
ばんじゃ～ぁぁあい、ばんじゃいぃぃ、ばんにゃんじゃぁんじゃあぁぁああぁい！
""")
}

