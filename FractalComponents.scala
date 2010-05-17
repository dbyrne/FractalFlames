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

  val flower = Flame((-2,2,-2.5,1.5),
		     2.2,
                     Rainbow(),
                     scala.List[Function] (
                                           Function(0.71,
                                                    0.0,
                                                    affineTransform(0.271587, -0.62814, 0.743008, 0.90192, 0.15938, 0.165033),
                                                    scala.List(linear(0.6851911667585093),swirl(0.31480883324149067))),
                                           Function(1.0,
                                                    1.0,
                                                    affineTransform(-0.039407, -0.385973, 0.452706, -0.055322, -0.372491, -0.62311),
                                                    scala.List(polar(0.35591518143252265),spiral(0.6)))))

  val brain = Flame((-3,5,-4,4),
                    2.0,
                    Rainbow(),
                    scala.List[Function] (
                                          Function(0.5,
                                                   0.0,
                                                   affineTransform(1.015245, -1.466244, 2.361193, 1.471095, -0.128157, 1.185009),
                                                   scala.List(linear(.5),spiral(.5))),
                                          Function(1.0,
                                                   1.0,
                                                   affineTransform(-0.567588, -0.536166, -0.325195, 0.801476, -0.90069, 1.08108),
                                                   scala.List(linear(.5),spiral(.5)))))
  
  val trout = Flame((-.75,.5,-.85,.40),
                    1.0,
                    Rainbow(),
                    scala.List[Function] (
                                          Function(0.5,
                                                   0.0,
                                                   affineTransform(-0.611536, -0.198789, 0.308349, -0.517899, -0.28474, -0.250627),
                                                   scala.List(popcorn(1.0,-0.28474,-0.250627),rings(1.0,math.pow(-0.28474,2)))),
                                          Function(1.0,
                                                   1.0,
                                                   affineTransform(0.102329, -0.67052, 0.732627, 0.189097, 0.057882, -0.08797),
                                                   scala.List(swirl(1.0)))))
  
  val pinwheel = Flame((-.25,1.25,-.75,.75),
                        4.0,
                        Rainbow(),
                        scala.List[Function] ( //Function list must be sorted by weights right now
                                              Function(0.5,
                                                       0.0,
                                                       affineTransform(0.7158, -0.311476, -0.250535, 0.728467, 6.63E-4, 0.075547),
                                                       scala.List(linear(0.33), handkerchief(1.0))),
                                              Function(1.0,
                                                       1.0,
                                                       affineTransform(0.343091, -0.799811, 0.687143, 0.524347, 0.593742, 0.249853),
                                                       scala.List(sinusoidal(.8),horseshoe(0.2)))))


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

  def polar(w:Double)=(p:Point) => Point(p.arctan2/math.Pi, p.r - 1) * w

  def spiral(w:Double)=(p:Point) => {
    val recipR = 1.0 / p.r
    Point(math.cos(p.arctan2) + math.sin(p.r),
          math.sin(p.arctan2) - math.cos(p.r)) * recipR * w
  }

  //dependent on c and f values from affine transform
  def popcorn(w:Double,e:Double,f:Double)=(p:Point) => {
    Point(p.x + e * math.sin(math.tan(3 * p.y)),
          p.y + f * math.sin(math.tan(3 *p.x))) * w
  }
  
  //dependent on c^2 value from affine transform
  def rings(w:Double, c2:Double)=(p:Point) => {
    val m = (p.r + c2) % (2 * c2)
    val i = ((m - c2) + p.r) - (p.r * c2)
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

  def affineTransform(a:Double,b:Double,c:Double,d:Double,e:Double,f:Double)=(p:Point) => {
    Point(a * p.x + c * p.y + e,b * p.x + d * p.y + f)
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