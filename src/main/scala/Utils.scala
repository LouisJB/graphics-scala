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
  val rand = new Random()
  def rndColor = new Color(rand.nextFloat, rand.nextFloat, rand.nextFloat)
}

object MathUtils {
  def sqr(x : Int) = x * x
}
