package laika.util.stats

/**
  * Created by home on 01/05/2017.
  */
class Counter (name: String) {

  private var currentValue: Int = 0


  def inc (): Unit = currentValue += 1

  def inc (amount: Int): Unit = currentValue += amount


  def reset (): Unit = currentValue = 0


  def value: Int = currentValue


  def print(): Unit =
    println(f"    $name%30s $currentValue%,9d")


}

case class CounterGroup (name: String, counters: Seq[Counter]) {

  def reset (): Unit = counters.foreach(_.reset())

  def print (): Unit = {
    println(s"  $name")
    counters.filterNot(_.value == 0).foreach(_.print())
    println()
  }

}

object Counter {


  object NewInstance {

    val Parser = new Counter("Parser")
    val ParserContext = new Counter("ParserContext")
    val Success = new Counter("Success")
    val Failure = new Counter("Failure")
    val Result2 = new Counter("~ 2")
    val Result3 = new Counter("~ 3")
    val Result4 = new Counter("~ 4")
    val ResultMore = new Counter("~ More")

    def result (arity: Int): Unit = {
      if (arity == 2) Result2.inc()
      else if (arity == 3) Result3.inc()
      else if (arity == 4) Result4.inc()
      else ResultMore.inc()
    }

    val Group = CounterGroup("New Instance", Seq(
      Parser,
      ParserContext,
      Success,
      Failure,
      Result2,
      Result3,
      Result4,
      ResultMore
    ))
  }

  object OneChar {

    val NewInstance = new Counter("New Instance")
    val Read = new Counter("Read")
    val RstMarkup = new Counter("Rst Markup")

    val Group = CounterGroup("OneChar", Seq(
      NewInstance,
      Read,
      RstMarkup
    ))

  }

  object Literal {

    val NewInstance = new Counter("New Instance")
    val Invoke = new Counter("Invoke")
    val Read = new Counter("Read")

    val Group = CounterGroup("Literal", Seq(
      NewInstance,
      Invoke,
      Read
    ))
  }

  object Characters {

    val NewInstance = new Counter("New Instance")
    val Invoke = new Counter("Invoke")

    private val ReadTotal = new Counter("Read Total")
    private val Read0 = new Counter("Read 0")
    private val Read1 = new Counter("Read 1")
    private val ReadMore = new Counter("Read >= 2")

    def read (numChars: Int): Unit = {
      ReadTotal.inc(numChars)
      if (numChars == 0) Read0.inc()
      else if (numChars == 1) Read1.inc()
      else ReadMore.inc()
    }

    val Group = CounterGroup("Characters", Seq(
      NewInstance,
      Invoke,
      ReadTotal,
      Read0,
      Read1,
      ReadMore
    ))
  }

  object DelimitedText {

    val NewInstance = new Counter("New Instance")
    val Invoke = new Counter("Invoke")
    val AtStartChar = new Counter("At StartChar")
    val AtEof = new Counter("At Eof")

    private val Read0 = new Counter("Read - 0 Delim") // never used
    private val Read1 = new Counter("Read - 1 Delim")
    private val Read2 = new Counter("Read - 2 Delim")
    private val ReadMore = new Counter("Read - >= 3 Delim")

    def read (numDelims: Int): Counter = numDelims match {
      case 0 => Read0
      case 1 => Read1
      case 2 => Read2
      case _ => ReadMore
    }

    val Group = CounterGroup("Delimited Text", Seq(
      NewInstance,
      Invoke,
      AtStartChar,
      AtEof,
      Read0,
      Read1,
      Read2,
      ReadMore
    ))

  }

  val All = Seq(
    NewInstance.Group,
    OneChar.Group,
    Literal.Group,
    Characters.Group,
    DelimitedText.Group
  )

  def reset (): Unit = {

    All.foreach(_.reset())

  }

  def print (): Unit = {

    All.foreach(_.print())

  }

}
