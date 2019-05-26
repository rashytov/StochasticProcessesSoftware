import java.io.{BufferedWriter, File, FileWriter}
import java.lang.Math.{log, _}

import play.api.libs.json.{JsArray, Json}

object StochasticProcessesSoftware {

  val randomVariableCount = pow(10, 6).toInt
  val integralCount = pow(10, 3).toInt
  val timeCount = pow(10, 4).toInt

  var experimentNumber: Int = 0
  def expFolder = s"/experiments/experiment_$experimentNumber"

  def main(args: Array[String]): Unit = {

  }

  def paintOnePublish(bm: Int, exp: Int, pr: String) = {
    val bmReader = scala.io.Source.fromURL(getClass.getResource(s"/brownian_motion/bm_$bm.json")).getLines.mkString
    val bmProcess = Json.parse(bmReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1))

    val wcReader = scala.io.Source.fromURL(getClass.getResource(s"/experiments/experiment_$exp/random_processes/$pr.json")).getLines.mkString
    var wcProcess = Json.parse(wcReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1))
    if (pr == "geometric_uniform") wcProcess = wcProcess.map(e => (e._1, e._2 / 10.0))
    if (pr == "uniform_exponential") wcProcess = wcProcess.map(e => (e._1, e._2 / 3.0))
    Graph.paintLines(wcProcess, bmProcess, pr)
  }

  def paintPublish = {
    val publish = List(
      (23, 10, "exponential_geometric"),
      (5, 10, "geometric_uniform"),
      (65, 9, "exponential_uniform"),
      (100, 8, "uniform_exponential"),
      (82, 8, "poisson_exponential"),
      (55, 10, "poisson_geometric")
    )
    publish.foreach(p => paintOnePublish(p._1, p._2, p._3))
  }

  def paintGroupLogProcesses = {
    val variables = List("exponential", "uniform", "geometric", "poisson")
    variables.foreach(ksiStr => variables.dropRight(1).foreach(etaStr => {
      val group = (1 to 10).map(i => {
        experimentNumber = i
        val folder = s"$expFolder/random_processes_log"
        val processReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/${ksiStr}_${etaStr}.json")).getLines.mkString
       Json.parse(processReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1)).drop(10)
      }).toList
      Graph.paintGroup(group, plotName = s"${ksiStr}_${etaStr}")
      experimentNumber = 0
    }))
  }

  def runGenerates = {
    (1 to 10).foreach(i => {
      val time = System.currentTimeMillis()
      experimentNumber = i
      generateDefaultVariables
      generateDefaultProcesses
      println(s"Success #$i in " + (System.currentTimeMillis() - time))
    })
  }

  def paintProcesses = {
    val variables = List("exponential", "uniform", "geometric", "poisson")
    variables.foreach(ksiStr => variables.dropRight(1).foreach(etaStr => {
      val time = System.currentTimeMillis()
      val folder = s"$expFolder/random_processes"

      val processReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/${ksiStr}_${etaStr}.json")).getLines.mkString
      val process = Json.parse(processReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1))
      Graph.paintLines(process, plotName = s"${ksiStr}_${etaStr}")
      println(s"Success ${ksiStr}_${etaStr} in " + (System.currentTimeMillis() - time))
    }))
  }

  def paintLogProcesses = {
    val variables = List("exponential", "uniform", "geometric", "poisson")
    variables.foreach(ksiStr => variables.dropRight(1).foreach(etaStr => {
      val time = System.currentTimeMillis()
      val folder = s"$expFolder/random_processes_log"

      val processReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/${ksiStr}_${etaStr}.json")).getLines.mkString
      val process = Json.parse(processReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1))
      Graph.paintLines(process, plotName = s"${ksiStr}_${etaStr}")
      println(s"Success ${ksiStr}_${etaStr} in " + (System.currentTimeMillis() - time))
    }))
  }

  def paintBrownianMotion ={
    (1 to 100).foreach(i => {
      val time = System.currentTimeMillis()
      val folder = s"brownian_motion"

      val processReader = scala.io.Source.fromURL(getClass.getResource(s"$folder/bm_$i.json")).getLines.mkString
      val process = Json.parse(processReader).as[JsArray].value.map(_.as[Double]).toList.zipWithIndex.map(el => (el._2.toDouble, el._1))
      Graph.paintLines(process, plotName = s"bm_$i")
      println(s"Success bm_$i in " + (System.currentTimeMillis() - time))
    })
  }

  def generateBrownianMotion = {
    (1 to 100).foreach(i => {
      val time = System.currentTimeMillis()
      val process = new BrownianMotion().getPoints.map(_._2)

      val folder = s"src/main/resources/brownian_motion"
      val processBuffer = new BufferedWriter(new FileWriter(s"$folder/bm_$i.json"))
      processBuffer.write(Json.toJson[List[Double]](process).toString())
      processBuffer.flush()
      println(s"Success bm_$i in " + (System.currentTimeMillis() - time))
    })
  }

  def generateDefaultProcesses = {
    val variables = List("exponential", "uniform", "geometric", "poisson").map(RandomVariableFromStorage)
    variables.foreach(ksi => variables.foreach(eta => {
      val time = System.currentTimeMillis()
      val (ksiStr, etaStr) = (ksi.name, eta.name)
      val (mu, sigma, integral) = (ksi.mu, ksi.sigma, eta.integral)
      val process = RandomProcess(ksi, eta)

      val wc = process.getPoints.map(i => (i._2 - (1/mu)*integral(i._1))/ sqrt((sigma * sigma) * (1 / (mu * mu * mu)) * timeCount))
      val lg = process.getPoints.filter(_._1 >= 3).map(i => (i._2 - (1/mu)*integral(i._1))/ sqrt(2*(sigma * sigma) * (1 / (mu * mu * mu)) * i._1 * log(log(i._1))))

      val folder = s"src/main/resources$expFolder/random_processes"
      val folderLog = s"src/main/resources$expFolder/random_processes_log"

      val processBuffer = new BufferedWriter(new FileWriter(s"$folder/${ksiStr}_${etaStr}.json"))
      val processBufferLog = new BufferedWriter(new FileWriter(s"$folderLog/${ksiStr}_${etaStr}.json"))


      processBuffer.write(Json.toJson[List[Double]](wc).toString())
      processBufferLog.write(Json.toJson[List[Double]](lg).toString())
      processBuffer.flush()
      processBufferLog.flush()
      println(s"Success ${ksiStr}_${etaStr} in " + (System.currentTimeMillis() - time))
    }))
  }

  def generateDefaultVariables = {
    var time = System.currentTimeMillis()
    var totalTime = System.currentTimeMillis()

    println("START TO STORAGE!")
    RandomVariableToStorage("exponential", Exponential(1.0))
    println("Success: Exponential! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableToStorage("uniform", Uniform(0.0, 1.0))
    println("Success: Uniform! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableToStorage("geometric", Geometric(0.5))
    println("Success: Geometric! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableToStorage("poisson", Poisson(1.0), countOfVariables = pow(10, 4).toInt)
    println("Success: Poisson! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    println("FINISH TO STORAGE! in " + (System.currentTimeMillis() - totalTime))

    println()
    totalTime = System.currentTimeMillis()

    println("START FROM STORAGE!")
    RandomVariableFromStorage("exponential")
    println("Success: Exponential! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableFromStorage("uniform")
    println("Success: Uniform! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableFromStorage("geometric")
    println("Success: Geometric! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    RandomVariableFromStorage("poisson")
    println("Success: Poisson! in " + (System.currentTimeMillis() - time))
    time = System.currentTimeMillis()
    println("FINISH FROM STORAGE! in " + (System.currentTimeMillis() - totalTime))
  }

}
