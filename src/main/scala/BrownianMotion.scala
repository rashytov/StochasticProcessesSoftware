import java.lang.Math._

case class BrownianMotion(time: Int = StochasticProcessesSoftware.timeCount, method: Int = 0, normalCount: Int = 1000) {
  private var process = List(0.0)
  private val normal = () => sqrt(-2 * log(random)) * sin(2 * PI * random)
  private val alpha = (1 to normalCount).map(_ => normal())

  def getPoints = (0 to time).map(i => (i.toDouble, process(i))).toList

  method match {
    case 1 => (1 to time).foreach(_ => process = process ++ List(process.last + sqrt(1.0 / time) * normal()))
    case 2 => (1 to time).foreach(_ => process = process ++ List(process.last + (if (random < 0.5) -1 else 1) / sqrt(time)))
    case _ => (1 to time).foreach(i => process = process ++ List((1 until normalCount).map(k => sqrt(2) * alpha(k) * sin((k + 1.0 / 2) * (1.0 * i / time) * PI) / ((k + 1.0 / 2) * PI)).sum))
  }
}
