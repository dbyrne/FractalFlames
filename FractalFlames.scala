import scala.swing._
import java.awt._
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
  def calculate(p:Point):Point = Point(Math.sin(p.x),Math.sin(p.y))
}

class Spherical(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r2 = p.x * p.x + p.y * p.y
    if (r2 == 0) return Point(0,0)
    Point(p.x / r2,p.y / r2)
  }
}

class Fisheye(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r_comp = 2 / Math.sqrt(p.x * p.x + p.y * p.y)       
    Point(r_comp * p.y, r_comp * p.x) //order of x and y is reversed
  }
}

class Swirl(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val r2 = p.x * p.x + p.y * p.y
    val sr = Math.sin(r2);
    val cr = Math.cos(r2);
    Point(sr * p.x - cr * p.y, cr * p.x + sr * p.y)
  }
}

class Horseshoe(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = {
    val recip_r = 1.0 / Math.sqrt(p.x * p.x + p.y * p.y)
    Point(recip_r * (p.x * p.x + p.y * p.y), recip_r * (2 * p.x * p.y))
  }
}

class Linear(weight:Double) extends Variation(weight) {
  def calculate(p:Point):Point = p
}

case class Function(weight:Double,
		    affineTransform:AffineTransform,
		    variations:scala.List[Variation]) {
  def iterate(p:Point):Point = {
    val tp = affineTransform.apply(p)
    variations.foldLeft(Point(0,0))(_+_.apply(tp))
  }
}

case class Flame(coords:(Double,Double,Double,Double), functions:scala.List[Function]) {
  
  def pickFunctionAndIterate(r:Double,p:Point):Point = {
    val rd = r*functions.last.weight
    //println("rd: " + rd)
    for (f <- functions) {
      //println("f: " + f.weight)
      if (rd <= f.weight) {
        return f.iterate(p)
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
  
  def render(g:Graphics, xres:Int, yres:Int){

    val flame = Flame((-2,3,-2.5,2.5),
		  scala.List[Function] ( //Function list must be sorted by weights right now
		  Function(.188,
			   AffineTransform(0.685796, -0.203333, 0.043371, 0.480696, 0.008717, -0.030978),
			   scala.List[Variation](new Linear(.94),new Spherical(0.0600000000000001))),		  
		  Function(.423,
			   AffineTransform(0.504957, -0.062872, 0.134236, 0.505966, 1.256077, -0.733679),
			   scala.List[Variation](new Linear(.94),new Spherical(0.0600000000000001))),
		  Function(1.323,
			   AffineTransform(0.924163, -0.278798, 1.156495, 0.629876, -0.344429, 0.500259),
			   scala.List[Variation](new Linear(.94), new Spherical(0.0600000000000001)))))

    /*
    val f2 = Function(1.72,
		      AffineTransform(0.97707,0.07041,-0.593528,1.037807,-1.185448,-0.120777),
		      scala.List[Variation](new Linear(0.001),new Spherical(8.5),new Fisheye(0.15)))
    val f1 = Function(0.026,
		      AffineTransform(0.747489, 0.420727, -0.875301, 0.093216, -0.608663, 0.609638),
		      scala.List[Variation](new Linear(0.772), new Spherical(3.766), new Horseshoe(-0.203)))
    */

    var values = new Array[Array[Int]](500,500)
    val (min_x, max_x, min_y, max_y) = flame.coords
    val range_x = max_x - min_x
    val range_y = max_y - min_y
    val r = new Random()
    var count = 0
    var p = Point(r.nextDouble() * range_x + min_x, r.nextDouble() * range_y + min_y)
    while (count < 100020) {

      p = flame.pickFunctionAndIterate(r.nextDouble,p)

      if (count > 20) {
	val pixel_x = Math.round(((p.x - min_x) / range_x) * 500).asInstanceOf[Int]
	val pixel_y = Math.round(((p.y - min_y) / range_y) * 500).asInstanceOf[Int]
	if (pixel_x < 500 && pixel_x > 0 && pixel_y < 500 && pixel_y > 0) {
	  values(pixel_x)(pixel_y) += 1
	}
      }
      count += 1
    }
    val colors = calculateColors()
    
    



    var valueList = new Array[Int](250000)
    var ci = 0
    for (r <- 0 until 500; c <- 0 until 500) {
      valueList(ci) = values(c)(r)
      ci += 1
    }
    println("Value list accumulated")
    scala.util.Sorting.quickSort(valueList)
    println("Value list sorted")
    val histogram = new scala.collection.mutable.HashMap[Int, Int]
    ci = 0
    for (v <- valueList) {
      histogram += v -> ci
      ci += 1
    }
    
    val size = histogram.size
    println("Histogram size: " + size)
    
    for (r <- 0 until 500; c <- 0 until 500) {
      val col_ind = Math.round( (histogram( values(c)(r) ).asInstanceOf[Double] / ci) * 1019 ).asInstanceOf[Int]
      g.setColor (colors(col_ind))
      g.drawLine(c,r,c,r)
    }
  }
  def calculateColors(): Array[Color]= {
  /*
  val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val color = Math.round(i*(255.0/maxColors)).asInstanceOf[Int]
      colors(i) = new Color(color,color,color)
    }
    colors 
  */
  
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val value = i%510
      val color = Math.abs(255-value)
      colors(i) = new Color(color,color,color)
    }
    colors 
  
  
  /*
    val maxColors = 1020
    val colors = new Array[Color](maxColors)
    for (i <- 0 to maxColors-1) {
      val data = 2.0 * Math.Pi * (i / 1020.0)
      val red = (Math.sin(data) * 127.0) + 127.0
      val green = (Math.cos(data) * 127.0) + 127.0
      val blue = (-((Math.sin(data) + Math.cos(data)) * .707) * 127.0) + 127.0;
      
      colors(i) = new Color(Math.round(red).asInstanceOf[Int],
			    Math.round(green).asInstanceOf[Int],
			    Math.round(blue).asInstanceOf[Int])
    }
    colors
  */
  }
  
}