import scala.swing._
import java.awt._
import javax.swing._
import scala.util.Random

object FractalFlames {
  def main(args: Array[String]){
    val frame=new JFrame("Fractal Flames")
    val panel=new MyPanel()
    frame add panel
    frame setSize (350, 350)
    frame setVisible true
  }
}

case class Point(x:Double,y:Double)

abstract class Transform {
  def apply(p:Point):Point
}

case class AffineTransform(a:Double,b:Double,c:Double,d:Double,e:Double,f:Double) extends Transform {
  def apply(p:Point):Point = Point(a * p.x + b * p.y + c,d * p.x + e * p.y + f)
}

case class SinusoidalTransform() extends Transform {
  def apply(p:Point):Point = Point(Math.sin(p.x),Math.sin(p.y))
}

case class SphericalTransform() extends Transform {
  def apply(p:Point):Point = {
    val r2 = p.x * p.x + p.y * p.y
    if (r2 == 0) return Point(0,0)
    Point(p.x / r2,p.y / r2)
  }
}

case class LinearTransform() extends Transform {
  def apply(p:Point):Point = p
}

case class Function(affineTransform:AffineTransform, variation:Transform, postTransform:AffineTransform) {
  def iterate(p:Point):Point = postTransform(variation.apply(affineTransform.apply(p)))
}

class MyPanel extends JPanel {
  override def paintComponent(g:Graphics):Unit = {
    super.paintComponent(g)
    val startTime = System currentTimeMillis()
    render(g, getWidth(), this.getHeight())
    println(System.currentTimeMillis() - startTime)
  }
  
  def render(g:Graphics, xres:Int, yres:Int){
    val min_x = -1
    val max_x = 1
    val min_y = -1
    val max_y = 1
    val r = new Random()
    var count = 0
    var p = Point(r.nextDouble() * 2 - 1, r.nextDouble() * 2 - 1)
    while (count < 20000020) {
    
      val f1 = Function(AffineTransform(.2,.3,.1,.1,.9,-.3),SinusoidalTransform(),AffineTransform(.5,.2,.5,.1,.7,0))
      val f2 = Function(AffineTransform(-.15,-.25,-.2,.45,.25,.25),SphericalTransform(),AffineTransform(.3,.8,.1,.6,.1,.2))
      val f3 = Function(AffineTransform(.1,.2,0,.15,-.4,.1),SinusoidalTransform(),AffineTransform(.6,-.3,-.1,.4,-.3,-.3))

      p = r.nextInt(3) match {
	case 0 => g.setColor (Color.RED);f1.iterate(p)
        case 1 => g.setColor (Color.BLUE);f2.iterate(p)
        case 2 => g.setColor (Color.GREEN);f3.iterate(p)
      }

      if (count > 20) {
	val pixel_x = Math.round(((p.x + 1) / 2) * 350).asInstanceOf[Int]
	val pixel_y = Math.round(((p.y + 1) / 2) * 350).asInstanceOf[Int]
	g.drawLine(pixel_x,pixel_y,pixel_x,pixel_y)
      }
      count += 1
    }
  }
}