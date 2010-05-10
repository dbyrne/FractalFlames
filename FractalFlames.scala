import scala.swing._
import java.awt.Graphics
import javax.swing._
import scala.util.Random

object FractalFlames {
  def main(args: Array[String]){
    val frame=new JFrame("Fractal Flames")
    val panel=new MyPanel()
    frame add panel
    frame setSize (500, 500)
    frame setVisible true
  }
}

case class Point(x:Double,y:Double) {
  def +(p:Point):Point = Point(p.x + x, p.y + y)
}

abstract class Variation(val weight:Double) {
  def apply(p:Point):Point = {
    val tp = calculate(p)
    Point(weight * tp.x,weight * tp.y)
  }
  def calculate(p:Point):Point
}

case class AffineTransform(a:Double,b:Double,c:Double,d:Double,e:Double,f:Double) {
  def apply(p:Point):Point = Point(a * p.x + c * p.y + e,b * p.x + d * p.y + f)
}

class Sinusoidal(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = Point(math.sin(p.x),math.sin(p.y))
}

class Spherical(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r2 = p.x * p.x + p.y * p.y + 1e-6
    Point(p.x / r2,p.y / r2)
  }
}

class Fisheye(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r_comp = 2 / math.sqrt(p.x * p.x + p.y * p.y)       
    Point(r_comp * p.y, r_comp * p.x) //order of x and y is reversed
  }
}

class Swirl(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r2 = p.x * p.x + p.y * p.y
    val sr = math.sin(r2);
    val cr = math.cos(r2);
    Point(sr * p.x - cr * p.y, cr * p.x + sr * p.y)
  }
}

class Horseshoe(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val recip_r = 1.0 / math.sqrt(p.x * p.x + p.y * p.y)
    Point(recip_r * (p.x * p.x + p.y * p.y), recip_r * (2 * p.x * p.y))
  }
}

class Power(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    
    val theta = math.atan2(p.x,p.y + 1e-6)
    val rst = math.pow(math.sqrt(p.x*p.x + p.y*p.y), math.sin(theta))
    Point(math.cos(theta), math.sin(theta))
  }
}

class Linear(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = p
}

case class Function(weight:Double,
		    colorWeight:Double,
		    affineTransform:AffineTransform,
		    variations:scala.List[Variation]) {
  def iterate(p:Point):Point = {
    val tp = affineTransform.apply(p)
    variations.foldLeft(Point(0,0))(_+_.apply(tp))
  }
}

case class Flame(coords:(Double,Double,Double,Double), gamma:Double, functions:scala.List[Function]) {
  val r = new Random()  
  def pickFunctionAndIterate(p:Point,c:Double):(Point,Double) = {
    val rd = r.nextDouble()*functions.last.weight
    for (f <- functions) {
      if (rd <= f.weight) {
        return (f.iterate(p),(c+f.colorWeight)/2.0)
      }
    }
    throw new Exception("Problem with function weights")
  }
  
}

class MyPanel extends JPanel {
  override def paintComponent(g:Graphics):Unit = {
    super.paintComponent(g)
    val startTime = System currentTimeMillis()
    render(g, getWidth(), this.getHeight())
    println(System.currentTimeMillis() - startTime)
  }
  
  def render(g:Graphics, xres:Int, yres:Int) {

      val flame = Flame((-.5,1.5,-1.5,.5),
		  4,
		  scala.List[Function] ( //Function list must be sorted by weights right now
		  Function(0.2155,
			   1.0,
			   AffineTransform(0.22590188704331657, 0.1381586175641758, -0.1381586175641758, 0.22590188704331657, 0.16728220092141166, -0.02117443969466626),
			   scala.List[Variation](new Linear(1))),		  
		  Function(1.0,
			   0.25,
			   AffineTransform(0.2848782511884477, 0.9577355012682768, -0.9668702316000889, 0.13050167874329055, 0.026213727563908717, -0.8868378321884305),
			   scala.List[Variation](new Linear(1)))))
  
  /*
    val flame = Flame((-12,12,-12,12),
		  2.38,
		  scala.List[Function] ( //Function list must be sorted by weights right now
		  Function(.125,
			   0.0,
			   AffineTransform(-0.070416, -0.335019, -0.416769, 0.031453, -0.170371, -0.311659),
			   scala.List[Variation](new Linear(1.772),new Spherical(5.052), new Power(.25))),		  
		  Function(.625,
			   0.273,
			   AffineTransform(-0.227785, -0.380106, 0.594681, 0.057683, 0, 0),
			   scala.List[Variation](new Linear(1))),
		  Function(1.5,
			   0.208,
			   AffineTransform(0.622799, 0.111881, 0.211325, 0.799623, -0.21504, -0.285111),
			   scala.List[Variation](new Linear(1.113), new Spherical(7.5), new Power(.25)))))
  
  */
  /*
    val flame = Flame((-2,3,-2.5,2.5),
		  2.3,
		  scala.List[Function] ( //Function list must be sorted by weights right now
		  Function(.188,
			   0.782,
			   AffineTransform(0.685796, -0.203333, 0.043371, 0.480696, 0.008717, -0.030978),
			   scala.List[Variation](new Linear(.94),new Spherical(0.0600000000000001))),		  
		  Function(.423,
			   0.0,
			   AffineTransform(0.504957, -0.062872, 0.134236, 0.505966, 1.256077, -0.733679),
			   scala.List[Variation](new Linear(.94),new Spherical(0.0600000000000001))),
		  Function(1.323,
			   1.0,
			   AffineTransform(0.924163, -0.278798, 1.156495, 0.629876, -0.344429, 0.500259),
			   scala.List[Variation](new Linear(.94), new Spherical(0.0600000000000001)))))
    */
    
    /*
    val flame = Flame((-4.5,5.5,-6.5,3.5),
		  2.2,
		  scala.List[Function] ( //Function list must be sorted by weights right now
		  Function(0.026,
			   0.15,
			   AffineTransform(0.747489, 0.420727, -0.875301, 0.093216, -0.608663, 0.609638),
			   scala.List[Variation](new Linear(0.772), new Spherical(3.766), new Horseshoe(-0.203))),
		  Function(1.746,
			   1.0,
			   AffineTransform(0.97707,0.07041,-0.593528,1.037807,-1.185448,-0.120777),
			   scala.List[Variation](new Linear(0.001),new Spherical(8.5),new Fisheye(0.15)))))
    */

    var values = Array.ofDim[Double](500,500,2)
    val (minX, maxX, minY, maxY) = flame.coords
    val rangeX = maxX - minX
    val rangeY = maxY - minY
    val r = new Random()
    var count = 0
    var p = (Point(r.nextDouble * rangeX + minX, r.nextDouble  * rangeY + minY),r.nextDouble)
    while (count < 5000020) {

      p = flame.pickFunctionAndIterate(p._1,p._2)

      if (count > 20) {
	val pixelX = math.round(((p._1.x - minX) / rangeX) * 500).asInstanceOf[Int]
	val pixelY = math.round(((p._1.y - minY) / rangeY) * 500).asInstanceOf[Int]
	if (pixelX < 500 && pixelX > 0 && pixelY < 500 && pixelY > 0) {
	  values(pixelX)(pixelY)(0) += 1 //Pixel Density
	  values(pixelX)(pixelY)(1) = p._2 //(values(pixelX)(pixelY)(1) + p._2) / 2.0 //Pixel Color
	}
      }
      count += 1
    }
    val colors = calculateColors()
    
    var max = 0.0
    for (r <- 0 until 500; c <- 0 until 500) {
      values(c)(r)(0) = math.log(values(c)(r)(0)) //log of the density
      if (values(c)(r)(0) > max) {
        max = values(c)(r)(0)
      }
    }

    for (r <- 0 until 500; c <- 0 until 500) {
      values(c)(r)(0) = values(c)(r)(0) / max //replace log density with alpha values
    }
    
    println(max)
    max = 0.0
    for (r <- 0 until 500; c <- 0 until 500) {
      val colInd = values(c)(r)(1) * math.pow(values(c)(r)(0),1.0/flame.gamma) 
      if (colInd > max) max = colInd
    }
    
    
    for (r <- 0 until 500; c <- 0 until 500) {
      val colInd = math.round(((values(c)(r)(1) * math.pow(values(c)(r)(0),1.0/flame.gamma))/max)*1019).asInstanceOf[Int]
      g.setColor(colors(colInd))
      g.drawLine(c,r,c,r)
    }
  }
  def calculateColors(): Array[Color]= {
  
  /*
  val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val color = math.round(i*(255.0/maxColors)).asInstanceOf[Int]
      colors(i) = new Color(color,color,color)
    }
    colors 
  */
  /*
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val value = i%510
      val color = math.abs(255-value)
      colors(i) = new Color(color,color,color)
    }
    colors 
  */
  
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    colors(0) = new Color(0,0,0)
    for (i <- 1 to maxColors-1) {
      val data = 2.0 * math.Pi * (i / 1020.0)
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