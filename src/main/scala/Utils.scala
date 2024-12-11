import java.util.concurrent.atomic.AtomicBoolean
import scala.swing.Frame
import java.awt.event.WindowAdapter
import java.awt.Color
import scala.util.Random

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
  def sqr(x : Int) = x * x
}
