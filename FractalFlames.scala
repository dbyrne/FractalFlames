import scala.swing._
import java.awt.Graphics
import javax.swing._
import scala.util.Random
import FractalComponents._

/*
Copyright (C) 2010 David Byrne
david.r.byrne@gmail.com

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

object FractalFlames {

  def main(args: Array[String]){
    val frame=new JFrame("Fractal Flames")
    val panel=new MyPanel()
    frame add panel
    frame setSize (500, 500)
    frame setVisible true
    while (true) {
      panel repaint()
    }
  }
}

class MyPanel extends JPanel {

  var values = Array.ofDim[Double](500,500,4)
  
  val flame = pinwheel
  
  override def paintComponent(g:Graphics):Unit = {
  
    super.paintComponent(g)
    render(g, getWidth(), this.getHeight())
  
  }
  
  def render(g:Graphics, xres:Int, yres:Int) {
   
    var alpha = Array.ofDim[Double](500,500)
 
    val (minX, maxX, minY, maxY) = flame coords
    val rangeX = maxX - minX
    val rangeY = maxY - minY
    var count = 0

    val colors = flame colors    
    val maxColorIndex = colors.size - 1

    while (count < 1000000) {
      val p = flame.points.next()
      val pixelX = math.round(((p._1.x - minX) / rangeX) * 500).asInstanceOf[Int]
      val pixelY = math.round(((p._1.y - minY) / rangeY) * 500).asInstanceOf[Int]
      if (pixelX < 500 && pixelX > 0 && pixelY < 500 && pixelY > 0) {
        values(pixelX)(pixelY)(0) += 1 //Pixel Density
        val c = colors(math.round(p._2 * maxColorIndex).asInstanceOf[Int])
        values(pixelX)(pixelY)(1) += c.getRed()
	values(pixelX)(pixelY)(2) += c.getGreen()
        values(pixelX)(pixelY)(3) += c.getBlue()
      }
      count += 1
    }
    
    var max = 0.0
    for (r <- 0 until 500; c <- 0 until 500) {
      if (values(c)(r)(0) > max) {
        max = values(c)(r)(0)
      }
      alpha(c)(r) = math.log(values(c)(r)(0)) //log of the density
    }

    max = math.log(max)
    for (r <- 0 until 500; c <- 0 until 500) {
      alpha(c)(r) = alpha(c)(r) / max //replace log density with alpha values
    }
  /*
    max = 0.0
    val recipGamma = 1.0/flame.gamma
    for (r <- 0 until 500; c <- 0 until 500) {
      values(c)(r)(1) = values(c)(r)(1) * math.pow(alpha(c)(r), recipGamma)
      values(c)(r)(2) = values(c)(r)(2) * math.pow(alpha(c)(r), recipGamma)
      values(c)(r)(3) = values(c)(r)(3) * math.pow(alpha(c)(r), recipGamma)
      if (values(c)(r)(1) > max) max = values(c)(r)(1)
      if (values(c)(r)(2) > max) max = values(c)(r)(2)
      if (values(c)(r)(3) > max) max = values(c)(r)(3)
    }
    println(max)
    */
    val recipGamma = 1.0/flame.gamma
    for (r <- 0 until 500; c <- 0 until 500) {
      g setColor(new Color(math.round((values(c)(r)(1)/values(c)(r)(0)) * math.pow(alpha(c)(r), recipGamma)).asInstanceOf[Int],
                           math.round((values(c)(r)(2)/values(c)(r)(0)) * math.pow(alpha(c)(r), recipGamma)).asInstanceOf[Int],
                           math.round((values(c)(r)(3)/values(c)(r)(0)) * math.pow(alpha(c)(r), recipGamma)).asInstanceOf[Int]))
      g drawLine(c,r,c,r)
    }
  }
  
}