case class RandomProcess(ksi:RandomVariable, eta: RandomVariable, time: Int = StochasticProcessesSoftware.timeCount) {
  private val count = (ksi.variable.size min eta.variable.size) - 1
  private val processByTime = (x: Int) => {
    val iteratorSk = ksi.Sk.toIterator
    val iteratorEta = eta.variable.toIterator
    (0 to count).count(_ => iteratorSk.next() + iteratorEta.next() < x)
  }
  private val process = (0 to time).map(processByTime(_))

  def getPoints = (0 to time).map(i => (i, process(i))).toList

}
