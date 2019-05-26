import java.lang.Math._
import java.io._

import StochasticProcessesSoftware.expFolder
import play.api.libs.json.{JsArray, Json}

sealed trait Distribution
case class Uniform(a: Double, b: Double) extends Distribution
case class Exponential(lambda: Double) extends Distribution
case class Poisson(lambda: Double) extends Distribution
case class Geometric(p: Double) extends Distribution

trait RandomVariable {
  def variable: List[Double]
  def Sk: List[Double]
  def integral: List[Double]
  def mu: Double
  def sigma: Double
}

case class RandomVariableToStorage(name: String, distribution: Distribution, record: Boolean = true,
                                   countOfVariables: Int = StochasticProcessesSoftware.randomVariableCount,
                                   integralCount: Int = StochasticProcessesSoftware.integralCount) extends RandomVariable {

  def calculateIntegral(t: Double, n: Int = integralCount) = (1 to n).map(i => (t / n) * F(i * (t / n) - (t / n) / 2.0)).sum

  private def calculateMu: Double = distribution match {
    case Uniform(a, b) => (a + b) / 2
    case Exponential(lambda) => 1 / lambda
    case Poisson(lambda) => lambda
    case Geometric(p) => (1 - p) / p
  }

  private def calculateSigma: Double = distribution match {
    case Uniform(a, b) => (b - a) * (b - a) / 12
    case Exponential(lambda) => 1 / (lambda * lambda)
    case Poisson(lambda) => lambda
    case Geometric(p) => (1 - p) * (p * p)
  }

  private def F: Double => Double = (x: Double) => distribution match {
    case Uniform(a, b) => if (x < a) 0.0 else if (x > b) 1.0 else (x - a) / (b - a)
    case Exponential(lambda) => 1 - exp(-lambda * x)
    case Poisson(lambda) => (1 to x.toInt).map(k => pow(lambda, k) * exp(-lambda) / (1 to (k min 33)).product).sum
    case Geometric(p) => 1 - pow(1 - p, round(x + 1))
  }

  private def generateRandomVariable: Double = distribution match {
    case Uniform(a, b) => (b - a) * random + a
    case Exponential(lambda) => -log(random) / lambda
    case Poisson(lambda) =>
      var (i, q) = (0, random)
      while (q >= exp(-lambda)) {
        i += 1
        q *= random()
      }
      i.toDouble
    case Geometric(p) => (log(random) / log(1 - p)).toInt
  }

  private def generate =
    (1 to countOfVariables).foreach(i => {
      val rv = generateRandomVariable
      val sk = Sk.last + rv
      val int = calculateIntegral(i)
      variable = variable ::: List(rv)
      integral = integral ::: List(int)
      Sk = Sk ::: List(sk)
    })

  private def write = {
      val folder = s"src/main/resources$expFolder/random_variables/$name"
      val variableBuffer = new BufferedWriter(new FileWriter(s"$folder/variable.json"))
      val SkBuffer = new BufferedWriter(new FileWriter(s"$folder/Sk.json"))
      val integralBuffer = new BufferedWriter(new FileWriter(s"$folder/integral.json"))

      def flushAll = List(variableBuffer, integralBuffer, SkBuffer).foreach(_.flush())
      def writeAll(s: String) = List(variableBuffer, integralBuffer, SkBuffer).foreach(_.write(s"$s\n"))

      val propertyBuffer = new BufferedWriter(new FileWriter(s"$folder/property.json"))
      propertyBuffer.write(Json.toJson[List[Double]](List(mu, sigma)).toString())
      propertyBuffer.flush()

      if (countOfVariables >= pow(10, 4).toInt) {
        writeAll("[")
        writeAll("0.0,")
        var currentSk = 0.0
        (1 to countOfVariables).foreach(i => {
          val rv = generateRandomVariable
          val sk = currentSk + rv
          val int = calculateIntegral(i)
          currentSk = sk

          val comma = if (i != countOfVariables) "," else ""
          variableBuffer.write(s"$rv$comma\n")
          SkBuffer.write(s"$sk$comma\n")
          integralBuffer.write(s"$int$comma\n")
          if (i % 1000 == 0) flushAll
        })
        writeAll("]")
        flushAll
      } else {
        generate
        variableBuffer.write(Json.toJson[List[Double]](variable).toString())
        integralBuffer.write(Json.toJson[List[Double]](integral).toString())
        SkBuffer.write(Json.toJson[List[Double]](Sk).toString())
        flushAll
      }
    }

  var variable = List[Double](0.0)
  var integral = List[Double](0.0)
  var Sk = List[Double](0.0)
  val (mu, sigma) = (calculateMu, calculateSigma)

  if (record) write else generate
}

case class RandomVariableFromStorage(name: String) extends RandomVariable {
  private val folder = s"$expFolder/random_variables/$name"

  private val propertyReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/property.json")).getLines.mkString
  private val variableReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/variable.json")).getLines.mkString
  private val SkReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/Sk.json")).getLines.mkString
  private val integralReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/integral.json")).getLines.mkString

  val variable = List(0.0) ::: Json.parse(variableReader).as[JsArray].value.map(_.as[Double]).toList
  val integral = List(0.0) ::: Json.parse(integralReader).as[JsArray].value.map(_.as[Double]).toList
  val Sk = List(0.0) ::: Json.parse(SkReader).as[JsArray].value.map(_.as[Double]).toList
  val (mu, sigma) = Some(Json.parse(propertyReader).as[JsArray].value.map(_.as[Double]).toList).map(list => (list.head, list(1))).get
}
