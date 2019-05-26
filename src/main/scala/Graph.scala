import java.awt.{Color, Frame}
import javax.swing.JFrame

import org.math.plot.Plot2DPanel

object Graph {

  def paintGroup(list: List[List[(Double, Double)]], plotName: String = "Graph") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    list.foreach(p => {
      val points = p.filter(_._1.toInt % 25 == 0)
      plot.addLinePlot(s"$plotName-points", Color.RED, points.map(_._1).toArray, points.map(_._2).toArray)
      plot.addScatterPlot(s"$plotName-points", Color.RED, points.map(_._1).toArray, points.map(_._2).toArray)
    })
  }

  def paintLines(simulatedPoints: List[(Double, Double)] = List(), theoreticalPoints: List[(Double, Double)] = List(), plotName: String = "Graph") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val (minX, maxX, minY, maxY) = (0.0,
      simulatedPoints ::: theoreticalPoints map(_._1) max,
      (simulatedPoints ::: theoreticalPoints map (_._2) min) - 0.1,
      (simulatedPoints ::: theoreticalPoints map (_._2) max) + 0.1)

    if (simulatedPoints.nonEmpty) {
      plot.addLinePlot(s"$plotName-Simulated points", Color.RED, simulatedPoints.map(_._1).toArray, simulatedPoints.map(_._2).toArray)
      val p = simulatedPoints.filter(_._1.toInt % 10 == 0)
      plot.addScatterPlot(s"$plotName-Simulated points", Color.RED, p.map(_._1).toArray, p.map(_._2).toArray)
    }
    if (theoreticalPoints.nonEmpty) {
      plot.addLinePlot(s"$plotName-Theoretical points", Color.BLUE, theoreticalPoints.map(_._1).toArray, theoreticalPoints.map(_._2).toArray)
      val p = theoreticalPoints.filter(_._1.toInt % 10 == 0)
      plot.addScatterPlot(s"$plotName-Theoretical points", Color.BLUE, p.map(_._1).toArray, p.map(_._2).toArray)
    }
    plot.setFixedBounds(List(minX, minY).toArray, List(maxX, maxY).toArray)
  }

  def paintScatters(simulatedPoints: List[(Double, Double)] = List(), theoreticalPoints: List[(Double, Double)] = List(),plotName: String = "Graph") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val (minX, maxX, minY, maxY) = (0.0,
      simulatedPoints ::: theoreticalPoints map(_._1) max,
      (simulatedPoints ::: theoreticalPoints map(_._2) min) - 0.1,
      (simulatedPoints ::: theoreticalPoints map(_._2) max) + 0.1)

    if (simulatedPoints.nonEmpty) plot.addScatterPlot(s"$plotName-Simulated points", Color.RED, simulatedPoints.map(_._1).toArray, simulatedPoints.map(_._2).toArray)
    if (theoreticalPoints.nonEmpty) plot.addScatterPlot(s"$plotName-Theoretical points", Color.BLUE, theoreticalPoints.map(_._1).toArray, theoreticalPoints.map(_._2).toArray)
    plot.setFixedBounds(List(minX, minY).toArray, List(maxX, maxY).toArray)
  }

  def paintStaircases(simulatedPoints: List[(Double, Double)] = List(), theoreticalPoints: List[(Double, Double)] = List(), plotName: String = "Graph") = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setExtendedState(Frame.MAXIMIZED_BOTH)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val (minX, maxX, minY, maxY) = (0.0,
      simulatedPoints ::: theoreticalPoints map(_._1) max,
      (simulatedPoints ::: theoreticalPoints map(_._2) min) - 0.1,
      (simulatedPoints ::: theoreticalPoints map(_._2) max) + 0.1)

    if (simulatedPoints.nonEmpty) plot.addStaircasePlot(s"$plotName-Simulated points", Color.RED, simulatedPoints.map(_._1).toArray, simulatedPoints.map(_._2).toArray)
    if (theoreticalPoints.nonEmpty) plot.addStaircasePlot(s"$plotName-Theoretical points", Color.BLUE, theoreticalPoints.map(_._1).toArray, theoreticalPoints.map(_._2).toArray)
    plot.setFixedBounds(List(minX, minY).toArray, List(maxX, maxY).toArray)
  }
}
