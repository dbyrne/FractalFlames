import java.awt.image.BufferedImage
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.io.IOException
import scala.util.Random
import FractalComponents._
import SampleFlames._

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
    val renderer=new Renderer(1000,1000)
    while (true) {
      renderer.render()
    }
  }
}

class Renderer(xres:Int,yres:Int) {

  val superSampling = 2
  var values = Array.ofDim[Double](xres*superSampling,yres*superSampling,4)
  
  val flame = curlSample
  
  def render() {
   
    var alpha = Array.ofDim[Double](xres*superSampling,yres*superSampling)
    var valFinal = Array.ofDim[Double](xres,yres,3)
 
    val (minX, maxX, minY, maxY) = flame coords
    val rangeX = maxX - minX
    val rangeY = maxY - minY
    var count = 0

    val colors = flame colors
    val maxColorIndex = colors.size - 1

    while (count < 1000000) {
      val p = flame.points.next()
      val pixelX = math.round(((p._1.x - minX) / rangeX) * xres * superSampling).asInstanceOf[Int]
      val pixelY = math.round(((p._1.y - minY) / rangeY) * yres * superSampling).asInstanceOf[Int]
      if (pixelX < xres * superSampling && pixelX > 0 && pixelY < yres * superSampling && pixelY > 0) {
        values(pixelX)(pixelY)(0) += 1 //Pixel Density
        val c = colors(math.round(p._2 * maxColorIndex).asInstanceOf[Int])
        values(pixelX)(pixelY)(1) += c.getRed()
	values(pixelX)(pixelY)(2) += c.getGreen()
        values(pixelX)(pixelY)(3) += c.getBlue()
      }
      count += 1
    }
    
    var max = 0.0
    
    for (r <- 0 until yres * superSampling; c <- 0 until xres * superSampling) {
      if (values(c)(r)(0) > max) {
        max = values(c)(r)(0)
      }
      alpha(c)(r) = math.log(values(c)(r)(0)) //log of the density
    }
    
    max = math.log(max)
    for (r <- 0 until yres * superSampling; c <- 0 until xres * superSampling) {
      alpha(c)(r) = alpha(c)(r) / max //replace log density with alpha values
    }
    
    val recipGamma = 1.0/flame.gamma
    for (r <- 0 until yres; c <- 0 until xres) {
      for (r2 <- r * superSampling until (r+1) * superSampling;
           c2 <- c * superSampling until (c+1) * superSampling) {
 
        for (i <- 0 until 3) {
          valFinal(c)(r)(i) += (values(c2)(r2)(i+1)/values(c2)(r2)(0)) * math.pow(alpha(c2)(r2), recipGamma)
        }

      }
      
      for (i <- 0 until 3) {
        valFinal(c)(r)(i) = valFinal(c)(r)(i) / (superSampling*superSampling)
      }      
    }
 
    val bImage = new BufferedImage(xres, yres, BufferedImage.TYPE_INT_RGB) 

    for (r <- 0 until yres; c <- 0 until xres) {

      val color = new Color(math.round(valFinal(c)(r)(0)).asInstanceOf[Int],
                           math.round(valFinal(c)(r)(1)).asInstanceOf[Int],
                           math.round(valFinal(c)(r)(2)).asInstanceOf[Int])
      bImage.setRGB(c, r, color.getRGB)
    }

    try {
      val outputfile = new File("saved.png")
      ImageIO.write(bImage, "png", outputfile)
    } catch {
      case e:IOException => println("Could not save image")
    }
  }
}
