import com.sksamuel.scrimage._
import java.awt.image.BufferedImage
import java.awt.image.BufferedImage._
import scala.collection.immutable.Seq
import scala.collection.mutable.Set
import scala.util._

object Main extends App {
  val im1 = new DirectionalPainter( new Picture(256, 128)
                                  , Set(LeftOblique, RightOblique)
                                  ).paint(0.1)
  val im2 = new AvgPainter(im1).paint

  im2.save("/tmp/testOut.png")
}

object Painter {
  def colorSSD(c1: RGBColor, c2: RGBColor): Double = {
    val RGBColor(r1, g1, b1, _) = c1
    val RGBColor(r2, g2, b2, _) = c2
    Math.sqrt(Math.pow(r1-r2, 2) + Math.pow(g1-g2, 2) + Math.pow(b1-b2, 2))
  }
  def coordSSD(c1: (Int, Int), c2: (Int, Int)): Double = {
    val (x1, y1) = c1
    val (x2, y2) = c2
    Math.sqrt(Math.pow(x1-x2, 2) + Math.pow(y1-y2, 2))
  }
  def colorAvg(colors: Seq[RGBColor]): Option[RGBColor] = {
    if(colors.length == 0) None
    else {
      val res = ((0.0, 0.0, 0.0) /: colors){
        case ((r, g, b), RGBColor(r_, g_, b_, _)) => (r+r_, g+g_, b+b_)
      } match {
        case (r, g, b) => {
          val num = colors.length
          RGBColor((r/num).round.toInt, (g/num).round.toInt, (b/num).round.toInt)
        }
      }
      Some(res)
    }
  }
}

case class Coord(val x: Int, val y: Int) {
  def +(other: Coord) = Coord(x+other.x, y+other.y)
  def -(other: Coord) = Coord(x-other.x, y-other.y)
}

abstract class Painter(private val picture: Picture) {
  private val width = picture.width
  private val height = picture.height

  private val possibleColors: Set[RGBColor] = {
    val numColors = (width * height).toDouble
    val depth = Math.pow(numColors, 1.0/3.0).round.toInt
    assert(depth < 256)
    val mul = 256/depth
    val c = for ( r <- Stream.range(0, depth)
                ; g <- Stream.range(0, depth)
                ; b <- Stream.range(0, depth)
              )
            yield RGBColor(r * mul, g * mul, b * mul)
    Set(c:_*)
  }

  private val possibleCoords: Set[Coord] = {
    val c = for ( x <- Stream.range(0, width)
                ; y <- Stream.range(0, height)
                )
            yield Coord(x, y)
    Set(c:_*)
  }

  for(coord <- possibleCoords) {
    picture.get(coord) match {
      case Some(color) => set(coord, color)
      case None => Unit
    }
  }

  assert(possibleCoords.size == possibleColors.size)

  final def set(coord: Coord, color: RGBColor): Boolean = {
    val r = coordEmpty(coord) && colorAvailable(color)
    if(r) {
      this.possibleColors.remove(color)
      this.possibleCoords.remove(coord)
      picture.set(coord, Some(color))
    }

    if(Random.nextDouble() < 0.05) {
      val percent = (1 - (unusedColors.length.toDouble / (width * height) )) * 100
      println(f"$percent%1.2f")
    }

    r
  }

  final def get(coord: Coord): Option[RGBColor] = this.picture.get(coord)

  final def coordValid(coord: Coord): Boolean
    = 0 <= coord.x && coord.x < width && 0 <= coord.y && coord.y < height

  final def coordEmpty(coord: Coord): Boolean
    = possibleCoords.contains(coord)


  final def colorAvailable = possibleColors.contains _

  final def unusedColors: Stream[Color] = possibleColors.toStream
  final def unusedCoords: Stream[Coord] = possibleCoords.toStream

  private final def randomFromSet[a](set: Set[a]): Option[a]
    = if(set.isEmpty) None else Some(set.toVector(Random.nextInt(set.size)))
  final def randomColor: Option[Color] = randomFromSet(possibleColors)
  final def randomCoord: Option[Coord] = randomFromSet(possibleCoords)

  final def neighborsOf(windowSize: Int, coord: Coord): Seq[Coord] = {
    val xs = for { i <- -windowSize to windowSize; j <- -windowSize to windowSize
                 ; if (i != 0 || j != 0) }
             yield Coord(coord.x+i, coord.y+j)
    xs.filter(coordValid)
  }

  final def paint(percentage: Double): Picture = {
    assert(0 <= percentage && percentage <= 1)
    while(unusedColors.size > width * height * (1-percentage)) step
    this.picture
  }

  final def paint: Picture = paint(1)

  def step: Unit
}

class Picture(val width: Int, val height: Int) {
  private val array: Array[Option[RGBColor]] = Array.fill(width * height)(None)
  def get(coord: Coord): Option[RGBColor]
    = this.array(coord.x * height + coord.y)
  def set(coord: Coord, v: Option[RGBColor]): Unit
    = this.array(coord.x * height + coord.y) = v

  def save(path: String): Unit = {
    val im = new Image( new BufferedImage(width, height, TYPE_INT_RGB)
                      , new ImageMetadata(Nil)
                      )
    for(x <- 0 until width)
      for(y <- 0 until height) {
        val color = this.get(Coord(x, y)).getOrElse(Color.White).toPixel
        im.setPixel(x, y, color)
      }
    im.output(path)
  }
}

class LinearPainter(val picture: Picture)
  extends Painter(picture) {
  override def step = {
    val color = unusedColors.minBy(_.toInt);
    val coord = unusedCoords.minBy(coord => (coord.x, coord.y));
    assert(this.set(coord, color))
  }
}

class RandomPainter(val picture: Picture)
  extends Painter(picture) {
  override def step = {
    assert(this.set(randomCoord.get, randomColor.get))
  }
}

class AvgPainter(val picture: Picture)
  extends Painter(picture) {
  override def step = {
    val coords = unusedCoords.map(
      coord => (coord, (neighborsOf(1, coord).map(get)).flatten)
    )
    val coordsSorted = coords.sortBy(- _._2.length)
    val chosenOnes = coordsSorted.takeWhile(_._2.length == coordsSorted.head._2.length)
    for((coord, neighbors) <- chosenOnes) {
      val target = Painter.colorAvg(neighbors).get
      val color = unusedColors.par.minBy(Painter.colorSSD(target, _))
      assert(set(coord, color))
    }
  }
}

abstract class Direction { val unit: Coord }
case object Horizontal   extends Direction { val unit = Coord(1, 0) }
case object Vertical     extends Direction { val unit = Coord(0, 1) }
case object LeftOblique  extends Direction { val unit = Coord(1, 1) }
case object RightOblique extends Direction { val unit = Coord(1, -1) }

class DirectionalPainter(val picture: Picture, val directions: Set[Direction])
  extends Painter(picture) {
  assert(directions.size > 0)

  override def step = {
    val directions_ = directions.toIndexedSeq
    val direction = directions_(Random.nextInt(directions_.length))

    val mid = randomCoord.get
    val bs = Stream.iterate(mid)(_ - direction.unit).takeWhile(coordEmpty _)
    val fs = Stream.iterate(mid)(_ + direction.unit).takeWhile(coordEmpty _).drop(1)
    val coords = bs.reverse #::: fs

    val c = randomColor.get
    val colors = unusedColors.sortBy(Painter.colorSSD(c, _))
    for((coord, color) <- coords.zip(colors)) assert(set(coord, color))
  }
}
