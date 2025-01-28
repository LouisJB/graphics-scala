package utils

import java.util.concurrent.atomic.AtomicBoolean
import scala.swing.Frame
import java.awt.event.WindowAdapter
import java.awt.Color
import scala.util.Random
import Math._


class WindowCloser() {
  // to avoid modal dialog to block main we can use a boolean signal
  private val closed = new AtomicBoolean(false)
  def addClosingHandler(frame: Frame): Unit = {
    frame.peer.addWindowListener(new WindowAdapter() {
      override def windowClosed(ev: java.awt.event.WindowEvent): Unit = {
        closed.synchronized {
          closed.set(true)
          closed.notify()
        }
        super.windowClosed(ev)
      }
    })
  }

  def waitOnClose(): Unit = {
    println("awaitng frame close")
    closed.synchronized {
      while (!closed.get()) {
        closed.wait()
      }
    }
  }
}

trait SettableFgColor {
  def setFgColor(c: Color): Unit
}

object GraphicUtils {
  import Math._
  val rand = new Random()
  def rndColor = new Color(rand.nextFloat, rand.nextFloat, rand.nextFloat)

  def brighter(c: Color) = {
    val factor = 0.85
    var r = c.getRed()
    var g = c.getGreen()
    var b = c.getBlue()
    val alpha = c.getAlpha()

    val i = (1.0/(1.0-factor)).toInt
    if ( r == 0 && g == 0 && b == 0) {
      new Color(i, i, i, alpha);
    }
    else {
      if ( r > 0 && r < i ) r = i
      if ( g > 0 && g < i ) g = i
      if ( b > 0 && b < i ) b = i

      new Color(min((r/factor).toInt, 255),
                min((g/factor).toInt, 255),
                min((b/factor).toInt, 255),
                alpha)
    }
  }
}

object MathUtils {
  lazy val rand = new Random()
  def randInt(range: Int, min: Int = 0) =
    (abs(rand.nextInt()) % range) + min

  def sqr(x : Int) = x * x
}

case class XY(x: Int, y: Int)

object ShapesUtils {
  // create array of x-y points
  // non-rotated polygon around a circle
  def poly(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0) = {
    (0 to 360 by arc).map ( a =>
      val r = ((a + initialAngle) * 2 * PI) / 360
      (x + (sin(r) * size).toInt, y + (cos(r) * size).toInt)
    ).toArray
  }

  // polygon with rotated axis/coordinate system and minor axis scaling
  def polyEx(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0, maybeMinorAxis: Option[Int] = None) = {
    val minorAxis = maybeMinorAxis.getOrElse(size)
    (0 to 360 by arc).map ( a =>
      val phi = (a * 2 * PI) / 360
      val theta = (initialAngle * 2 * PI) / 360
      val dx = sin(phi) * size
      val dy = cos(phi) * minorAxis
      val ddx = dx * cos(theta) + dy * sin(theta)
      val ddy = dx * -1.0 * sin(theta) + dy * cos(theta)
      
      (x + ddx.toInt, y + ddy.toInt)
    ).toArray
  }

  def polyDistorted(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0, maybeMinorAxis: Option[Int] = None) = {
    val minorAxis = maybeMinorAxis.getOrElse(size)
    
    val width = size - sin(initialAngle) * minorAxis
    val height =  minorAxis + cos(initialAngle) * size
    
    (0 to 360 by arc).map ( a =>
      val r = ((a + initialAngle) * 2 * PI) / 360
      (x + (sin(r) * width).toInt, y + (cos(r) * height).toInt)
    ).toArray
  }

  def toArrayXYPair(points: Array[(Int, Int)]) =
    (points.map(_._1), points.map(_._2), points.length)
}
