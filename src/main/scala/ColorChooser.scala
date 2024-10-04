/*
 * scala-swing (https://www.scala-lang.org)
 *
 * Copyright EPFL, Lightbend, Inc., contributors
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

import java.awt.{Color, Font}

import scala.swing.BorderPanel._
import scala.swing.Swing._
import scala.swing._
import scala.swing.event._

/**
 * Demo for ColorChooser.
 * Based on http://download.oracle.com/javase/tutorial/uiswing/components/colorchooser.html
 *
 * @author andy@hicks.net
 */

object ColorChooserFrame {
  @main def showColorChooser() = {
    val p = new SettableFgColor {
      def setFgColor(c: Color) = {}
    }
    val cc = new ColorChooserFrame(p)
  }
}
class ColorChooserFrame(parent: SettableFgColor) {
  def top: Frame = new Frame {
    title = "Color Chooser"
    size = new Dimension(400, 400)

    contents = ui

    pack().centerOnScreen()
    open()
  }
  def ui: BorderPanel = new BorderPanel {
    val colorChooser: ColorChooser = new ColorChooser {
      reactions += {
        case ColorChanged(_, c) =>
          parent.setFgColor(c)
      }
    }

    colorChooser.border = TitledBorder(EtchedBorder, "Choose Color")
    layout(colorChooser)  = Position.Center
  }

  top.open()
}
