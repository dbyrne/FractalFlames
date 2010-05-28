import scala.util.Random
import scala.swing.Color

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



object FractalComponents {

  val psi = 3.359885666

  case class Point(x:Double,y:Double) {
    def +(p:Point):Point = Point(p.x + x, p.y + y)
    def *(d:Double):Point = Point(x * d, y * d)
    lazy val r = math.sqrt(x * x + y * y)
    lazy val r2 = x * x + y * y
    lazy val arctan2 = math.atan2(x,y)
  }
    
  case class Function(weight:Double,
                      colorWeight:Double,
                      coefficients:(Double,Double,Double,Double,Double,Double),
                      variations:scala.List[(Point) => Point]=Nil) extends Variations {
                       
    def iterate(p:Point):Point = {
      val tp = affineTransform(p)
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

  trait Variations {
  
    def coefficients:(Double,Double,Double,Double,Double,Double)
    val (a,b,c,d,e,f) = coefficients

    def affineTransform(p:Point):Point = Point(a * p.x + c * p.y + e,b * p.x + d * p.y + f)
    val e2 = e*e

    //Parametric variation
    def julian(w:Double, p1:Double, p2:Double)=(p:Point) => {
      val p3 = math.floor(math.abs(p1) * psi)
      val t = (math.atan2(p.y,p.x) + 2*math.Pi*p3)/p1
      val rp = math.pow(p.r,p2/p1)
      Point(math.cos(t), math.sin(t)) * rp * w
    }

    //Parametric variation  
    def curl(w:Double, c1:Double, c2:Double)=(p:Point) => {
      val t1 = 1 + c1*p.x + c2*(p.x*p.x - p.y*p.y)
      val t2 = c1*p.y + 2*c2*p.x*p.y
      val t3 = 1 / (t1*t1 + t2*t2)
      Point(t1*p.x + t2*p.y, t1*p.y - t2*p.x) * t3 * w
    }

    def polar(w:Double)=(p:Point) => Point(p.arctan2/math.Pi, p.r - 1) * w

    def spiral(w:Double)=(p:Point) => {
      val recipR = 1.0 / p.r
      Point(math.cos(p.arctan2) + math.sin(p.r),
            math.sin(p.arctan2) - math.cos(p.r)) * recipR * w
    }

    //dependent on e and f values from affine transform
    def popcorn(w:Double)=(p:Point) => {
      Point(p.x + e * math.sin(math.tan(3 * p.y)),
            p.y + f * math.sin(math.tan(3 *p.x))) * w
    }
  
    //dependent on e^2 value from affine transform
    def rings(w:Double)=(p:Point) => {
      val m = (p.r + e2) % (2 * e2)
      val i = ((m - e2) + p.r) - (p.r * e2)
      Point(math.cos(p.arctan2), math.sin(p.arctan2)) * i * w
    }
  
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

  }
  
  def loadColors(f:String):Array[Color]={
    var colors = scala.List[Color](new Color(0,0,0))
    for (l <- io.Source.fromPath("perch.txt").getLines("\n")) {
      colors = new Color(l.toInt) :: colors
    }
    colors.reverse.toArray
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
