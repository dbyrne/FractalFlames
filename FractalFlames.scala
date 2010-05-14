import scala.swing._
import java.awt.Graphics
import javax.swing._
import scala.util.Random

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

case class Point(x:Double,y:Double) {
  def +(p:Point):Point = Point(p.x + x, p.y + y)
  def *(d:Double):Point = Point(x * d, y * d)
  lazy val r = math.sqrt(x * x + y * y)
  lazy val r2 = x * x + y * y
  lazy val arctan2 = math.atan2(x,y)
}

case class Function(weight:Double,
                    colorWeight:Double,
                    affineFunction:(Point) => Point,
                    variations:scala.List[(Point) => Point]) {
                    
  def iterate(p:Point):Point = {
    val tp = affineFunction(p)
    variations.foldLeft(Point(0,0))(_+_(tp))
  }
  
}

case class Flame(coords:(Double,Double,Double,Double),
                 gamma:Double,
                 colors:Array[Color],
                 functions:scala.List[Function]) {
                 
  val r = new Random()
  val (minX,maxX,minY,maxY) = coords
  val p = Point(r.nextDouble * (maxX-minX) + minX, r.nextDouble * (maxY-minY) + minY)
  val c = r nextDouble()

  val points = Iterator.iterate((p,c))(x => chaosGame(x)).drop(20)
  
  def chaosGame(x:(Point,Double)):(Point,Double) = {
    val rd = r.nextDouble()*functions.last.weight 
    for (f <- functions) {
      if (rd <= f.weight) {
        return (f.iterate(x._1),(x._2+f.colorWeight)/2.0)
      }
    }
    throw new Exception("Problem with Function Weights")
  }
    
}

class MyPanel extends JPanel {

  var values = Array.ofDim[Double](500,500,2)
  
  override def paintComponent(g:Graphics):Unit = {
  
    super.paintComponent(g)
    val startTime = System currentTimeMillis()
    render(g, getWidth(), this.getHeight())
  
  }
 
  val flame = Flame((-.25,1.25,-.75,.75),
                     1.0,
                     Rainbow(),
                     scala.List[Function] ( //Function list must be sorted by weights right now
                                           Function(0.5,
                                                    0.1,
                                                    affineTransform(0.7158, -0.311476, -0.250535, 0.728467, 6.63E-4, 0.075547),
                                                    scala.List(linear(0.33), handkerchief(1.0))),
                                           Function(1.0,
                                                    1.0,
                                                    affineTransform(0.343091, -0.799811, 0.687143, 0.524347, 0.593742, 0.249853),
                                                    scala.List(sinusoidal(.8),horseshoe(0.2)))))
 
  def sinusoidal(w:Double)=(p:Point) => Point(math.sin(p.x),math.sin(p.y)) * w

  def handkerchief(w:Double)=(p:Point) => {
    Point(math.sin(p.arctan2 + p.r), math.cos(p.arctan2 - p.r)) * p.r * w
  }

  def spherical(w:Double)=(p:Point) => {
    val r2 = p.r2 + 1e-6
    Point(p.x / r2,p.y / r2) * w
  }

  def fisheye(w:Double)=(p:Point) => {
    val r_comp = 2 / p.r       
    Point(r_comp * p.y, r_comp * p.x) * w //order of x and y is reversed
  }

  def swirl(w:Double)=(p:Point) => {
    val sr = math.sin(p.r2);
    val cr = math.cos(p.r2);
    Point(sr * p.x - cr * p.y, cr * p.x + sr * p.y) * w
  }

  def horseshoe(w:Double)=(p:Point) => {
    val c1 = math.sin(p.arctan2);
    val c2 = math.cos(p.arctan2);
    Point(c1*p.x - c2*p.y, c2*p.x + c1*p.y) * w
  }

  def power(w:Double)=(p:Point) => {
    val rst = math.pow(p.r, math.sin(p.arctan2))
    Point(math.cos(p.arctan2), math.sin(p.arctan2)) * w
  }

  def linear(w:Double)=(p:Point) => p * w

  def affineTransform(a:Double,b:Double,c:Double,d:Double,e:Double,f:Double)=(p:Point) => {
    Point(a * p.x + c * p.y + e,b * p.x + d * p.y + f)
  }
  
  def render(g:Graphics, xres:Int, yres:Int) {
   
    var alpha = Array.ofDim[Double](500,500)
 
    val (minX, maxX, minY, maxY) = flame coords
    val rangeX = maxX - minX
    val rangeY = maxY - minY
    var count = 0
    while (count < 1000000) {
      val p = flame.points.next()
      val pixelX = math.round(((p._1.x - minX) / rangeX) * 500).asInstanceOf[Int]
      val pixelY = math.round(((p._1.y - minY) / rangeY) * 500).asInstanceOf[Int]
      if (pixelX < 500 && pixelX > 0 && pixelY < 500 && pixelY > 0) {
        values(pixelX)(pixelY)(0) += 1 //Pixel Density
        values(pixelX)(pixelY)(1) = p._2 //(values(pixelX)(pixelY)(1) + p._2) / 2.0 //Pixel Color
      }
      count += 1
    }
    val colors = flame colors
    
    var max = 0.0
    for (r <- 0 until 500; c <- 0 until 500) {
      alpha(c)(r) = math.log(values(c)(r)(0)) //log of the density
      if (alpha(c)(r) > max) {
        max = alpha(c)(r)
      }
    }

    for (r <- 0 until 500; c <- 0 until 500) {
      alpha(c)(r) = alpha(c)(r) / max //replace log density with alpha values
    }
  
    max = 0.0
    val recipGamma = 1.0/flame.gamma
    for (r <- 0 until 500; c <- 0 until 500) {
      val colInd = values(c)(r)(1) * math.pow(alpha(c)(r), recipGamma) 
      if (colInd > max) max = colInd
    }
  
    for (r <- 0 until 500; c <- 0 until 500) {
      val colInd = math.round(((values(c)(r)(1) * math.pow(alpha(c)(r),recipGamma))/max)*1019).asInstanceOf[Int]
      g setColor(colors(colInd))
      g drawLine(c,r,c,r)
    }
  }
  
  def BlackAndWhite(): Array[Color]= {
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val color = math.round(i*(255.0/maxColors)).asInstanceOf[Int]
      colors(i) = new Color(color,color,color)
    }
    colors
  }
  
  def Rainbow(): Array[Color]= {
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    colors(0) = new Color(0,0,0)
    for (i <- 1 to maxColors-1) {
      val data = 2.0 * math.Pi * (i / 510.0)
      val red = (math.sin(data) * 127.0) + 127.0
      val green = (math.cos(data) * 127.0) + 127.0
      val blue = (-((math.sin(data) + math.cos(data)) * .707) * 127.0) + 127.0;
      
      colors(i) = new Color(math.round(red).asInstanceOf[Int],
			    math.round(green).asInstanceOf[Int],
			    math.round(blue).asInstanceOf[Int])
    }
  
    colors
  }
  
}