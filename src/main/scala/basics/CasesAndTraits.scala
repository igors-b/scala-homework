package basics

object CasesAndTraits {

  sealed trait Shape extends Located with Bounded {
    def area: Double
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }


  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius

    override def area: Double = math.Pi * radius * radius
  }

  final case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends Shape {
    override def minX: Double = x - width/2
    override def maxX: Double = x + width/2
    override def minY: Double = y - height/2
    override def maxY: Double = y + height/2

    override def area: Double = width * height
  }

  final case class square(x: Double, y: Double, sideLength: Double) extends Shape {
    override def minX: Double = x - sideLength/2
    override def maxX: Double = x + sideLength/2
    override def minY: Double = y - sideLength/2
    override def maxY: Double = y + sideLength/2

    override def area: Double = sideLength * sideLength
  }

  final case class Triangle(x: Double, y: Double, sideA: Double, sideB: Double, sideC: Double) extends Shape {
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???

    val p: Double = (sideA + sideB + sideC) / 2
    override def area: Double = math.sqrt(p * (p - sideA) * (p - sideB) * (p - sideC))
  }

  sealed trait Shape3D extends Located3D with Bounded3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def surfaceArea: Double = 0

    override def volume: Double = 0
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def surfaceArea: Double = 4 * math.Pi * radius * radius

    override def volume: Double = surfaceArea * radius / 3
  }

  final case class Cube(x: Double, y: Double, z: Double, sideLength: Double) extends Shape3D {
    override def minX: Double = x - sideLength/2
    override def maxX: Double = x + sideLength/2
    override def minY: Double = y - sideLength/2
    override def maxY: Double = y + sideLength/2
    override def minZ: Double = z - sideLength/2
    override def maxZ: Double = z + sideLength/2

    override def surfaceArea: Double = 6 * sideLength * sideLength

    override def volume: Double = sideLength * sideLength * sideLength
  }

  final case class Cuboid(x: Double, y: Double, z: Double, width: Double, length: Double, height: Double) extends Shape3D {
    override def minX: Double = x - width/2
    override def maxX: Double = x + width/2
    override def minY: Double = y - length/2
    override def maxY: Double = y + length/2
    override def minZ: Double = z - height/2
    override def maxZ: Double = z + height/2

    override def surfaceArea: Double = width * length * 2 + width * height * 2 + length * height * 2

    override def volume: Double = width * length * height
  }
}
