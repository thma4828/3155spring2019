package edu.colorado.csci3155.project2

/* A class to maintain a canvas. */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def rotate(angle:Double):Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        var minx = 10000.0
        var miny = 10000.0
        var maxx = -10000.0
        var maxy = -10000.0
        for(p <- cList){ //for each point in the list of vertices
            if(p._1 < minx) minx = p._1
            if(p._1 > maxx) maxx = p._1
            if(p._2 < miny) miny = p._2
            if(p._2 > maxy) maxy = p._2
        }
        (minx, maxx, miny, maxy)
    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        Polygon(cList.map(c => (c._1 + shiftX, c._2 + shiftY))) //just return the new polygon shifted over!
    }

    override def rotate(theta: Double):Polygon = {
        Polygon(cList.map(c => {
            //x cos() - y sin, x sin() + y cos()
            ((c._1*math.cos(theta)) - (c._2*math.sin(theta)), (c._1*math.sin(theta)) +  (c._2*math.cos(theta))) }
        ))
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) =  {
        (c._1 - r, c._1 + r, c._2 - r, c._2 + r)
    }

    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        MyCircle((c._1 + shiftX, c._2 + shiftY), r)
    }

    override def rotate(theta:Double): MyCircle = {
        //rotate the center and leave radius unchanged....
        MyCircle(( c._1*math.cos(theta) - c._2*math.sin(theta), c._1*math.sin(theta) + c._2*math.cos(theta)), r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        var xmin = 10000.0 //make sure this initialization doesn't cause bugs.
        var xmax = -10000.0
        var ymin = 10000.0
        var ymax = -10000.0
        for(fig <- listOfObjects){
            val currBox = fig.getBoundingBox
            if(currBox._1 < xmin) xmin = currBox._1
            if(currBox._2 > xmax) xmax = currBox._2
            if(currBox._3 < ymin) ymin = currBox._3
            if(currBox._4 > ymax) ymax = currBox._4
        }
        (xmin, xmax, ymin, ymax)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map(obj => obj.translate(shiftX, shiftY))) //this should work??
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        //canvas c1 is this current object
        val box1 = getBoundingBox
        val box2 = myc2.getBoundingBox
        val xmin1 = box1._1
        val xmax1 = box1._2
        val ymin1 = box1._3
        val ymax1 = box1._4

        val xmin2 = box2._1
        val xmax2 = box2._2
        val ymin2 = box2._3
        val ymax2 = box2._4


        val xshift = xmax1 - xmin1
        val yshift = ((ymax1 - ymin1)/2) - ((ymax2 - ymin2) / 2)
        //now we need to translate c2 to get c2hat
        val myc2hat = myc2.translate(xshift, yshift)
        //then you would overlap myc2hat with this object and return new canvas.....
        overlap(myc2hat)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val box1 = getBoundingBox
        val box2 = myc2.getBoundingBox
        val xmin1 = box1._1
        val xmax1 = box1._2
        val ymin1 = box1._3
        val ymax1 = box1._4

        val xmin2 = box2._1
        val xmax2 = box2._2
        val ymin2 = box2._3
        val ymax2 = box2._4

        val xshift = ((xmax1 - xmin1)/2) - ((xmax2 - xmin2)/ 2)
        val yshift = ymax1 - ymin1
        val myc2hat = myc2.translate(xshift, yshift)
        overlap(myc2hat)
    }
    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map(obj => obj.rotate(angRad)))
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
