import monocle._
import monocle.macros.Lenses
import cats.effect.IO
import scala.math.Ordering.Implicits._
import scala.util.matching.Regex
import java.util.UUID
import cats.instances.list._
import cats.syntax.all._

sealed trait TemperatureScale
//@Lenses
case class Celcius(degrees: Int, sensorLocation: Option[String]) extends TemperatureScale
case class Fahrenheit(degrees: Int) extends TemperatureScale
case class Kelvin(degrees: Int) extends TemperatureScale

object Definitions {
    val celciusToFahrenheit = Iso[Celcius, Fahrenheit](c => Fahrenheit(c.degrees * 9 / 5 + 32))(f => Celcius((f.degrees - 32) * 5 / 9, sensorLocation = None))
    val kelvinToCelcius = Iso[Kelvin, Celcius](k => Celcius(k.degrees - 270, sensorLocation = None))(c => Kelvin(c.degrees + 270))

    val currentTemperature: IO[Celcius] =
        IO.delay(scala.util.Random.between(-20, 45)).map(Celcius.apply(_, Some("Random")))


    implicit val ordCelcius = Ordering.by[Celcius, Int](_.degrees)
    implicit val ordFahrenheit = Ordering.by[Fahrenheit, Int](_.degrees)
    implicit val ordKelvin = Ordering.by[Kelvin, Int](_.degrees)
}

import Definitions._

/* 1. Start by definiing an ISO between two types., One has temparatures in Celcius and the other in Fahrenheit */
assert(celciusToFahrenheit.get(Celcius(0, sensorLocation = None)) == Fahrenheit(32))
assert(celciusToFahrenheit.reverseGet(Fahrenheit(50)) == Celcius(10, sensorLocation = None))

/* 2. Add a third class Kelvin and write 1 ISO to celcius then get the ISO to Fahrenheit for free. */
val kelvinToFahrenheit = kelvinToCelcius.composeIso(celciusToFahrenheit)
assert(kelvinToCelcius.get(Kelvin(10)) == Celcius(-260, sensorLocation = None))
assert(kelvinToFahrenheit.get(Kelvin(10)) == Fahrenheit(-436))

/* 3. Write a function that returns the temperature in Celcius then use the ISOs to reuse that function for getting temperatures in the other two */
assert(currentTemperature.unsafeRunSync() >= Celcius(-20, sensorLocation = None))
assert(currentTemperature.unsafeRunSync() <= Celcius(45, sensorLocation = None))

assert(currentTemperature.map(celciusToFahrenheit.get).unsafeRunSync() >= Fahrenheit(-4))
assert(currentTemperature.map(celciusToFahrenheit.get).unsafeRunSync() <= Fahrenheit(113))

assert(currentTemperature.map(kelvinToCelcius.reverseGet).unsafeRunSync() >= Kelvin(250))
assert(currentTemperature.map(kelvinToCelcius.reverseGet).unsafeRunSync() <= Kelvin(315))

/* 4. Write a function that warms a Celcius by some degrees. Write a Lens from Celcius to its Int (degrees), */
def warmUp(warmWith: Int)(celcius: Celcius): Celcius = Celcius(celcius.degrees + warmWith, sensorLocation = celcius.sensorLocation)

val celciusFocusDegrees = Lens[Celcius, Int](_.degrees)(d => c => Celcius(d, c.sensorLocation))
def warmUpWithLens(warmWith: Int)(celcius: Celcius) = celciusFocusDegrees.modify(_ + warmWith)(celcius)
def warmUpWithLens2(warmWith: Int) = celciusFocusDegrees.modify(_ + warmWith)

warmUp(5)(Celcius(25, sensorLocation = Some("One")))
warmUpWithLens(5)(Celcius(25, sensorLocation = Some("Two")))
warmUpWithLens2(5)(Celcius(25, sensorLocation = Some("Three")))

/* 4.b Similarly write functions that dig into the sensor location */
val celciusFocusSensorLocation: Lens[Celcius, Option[String]] = Lens[Celcius, Option[String]](_.sensorLocation)(sl => _.copy(sensorLocation = sl))

def optionalSelf[A] = Optional[Option[A], A](identity)(a => _ => Some(a))
val celsiusSensLoc = celciusFocusSensorLocation.composeOptional(optionalSelf)

def capLoc(loc: String) = loc.capitalize

def setLocation(location: Option[String]) = celciusFocusSensorLocation.set(location)
setLocation(location = Some("Home"))(Celcius(degrees = 25, sensorLocation = None))

def capCelcius: Celcius => Celcius = celsiusSensLoc.modify(capLoc)
def capCelcius2: Celcius => Celcius = celciusFocusSensorLocation.modify(_.map(capLoc))
val capCelcius3: Celcius => Celcius = celciusFocusSensorLocation.composeOptional(optionalSelf).modify(capLoc)

capCelcius3(Celcius(degrees = 12, sensorLocation = None))
capCelcius3(Celcius(degrees = 12, sensorLocation = Some("mine")))

/* 4.c explorations into optics composition Optional + Lens */

case class B(c: Int, extraInfo: String)
case class A(b: Option[B])

val AOptionB = Optional[A, B](_.b)(b => a => a.copy(b = Some(b)))
val BFocusC = Lens[B, Int](_.c)(c => b => b.copy(c = c))

val AOptionC = AOptionB.composeLens(BFocusC)

assert (AOptionC.set(1)(A(b=None)).b == None)
assert (AOptionB.set(B(1, "Yes I can"))(A(b=None)).b == Some(B(1, "Yes I can")))


/* 5. Write a Prism between String and Fahrenheit that can correctly convert 5°F and 5℉. */

def strTemperaturePrism[TempUnit](re: Regex, construct: Int => TempUnit, toString: TempUnit => String): Prism[String, TempUnit] =
Prism[String, TempUnit] {
    case re(degrees) => degrees.toIntOption.map(construct)
    case _ => None
}(toString)

val StringToFahrenheit: Prism[String, Fahrenheit] =
  strTemperaturePrism(raw"(\d+)(?:°?F|℉)".r, Fahrenheit.apply, _.degrees.toString + "℉")

assert (StringToFahrenheit.getOption("12F") == Some(Fahrenheit(12)))

/* 6. Write a Similar Prism for Farenheight and Kelvin. */

val StringToKelvin: Prism[String, Kelvin] =
  strTemperaturePrism(raw"(\d+)(?:°?K|K)".r, Kelvin.apply, _.degrees.toString + "K")

val StringToCelcius: Prism[String, Celcius] =
  strTemperaturePrism(raw"(\d+)(?:°?C|℃)".r, Celcius(_, Some("Console")), _.degrees.toString + "℃")

val AllTempStringsToKelvin = Prism[String, Kelvin] {
    case StringToKelvin(k)     => Some(k)
    case StringToCelcius(c)    => Some(kelvinToCelcius.reverseGet(c))
    case StringToFahrenheit(f) => Some(kelvinToFahrenheit.reverseGet(f))
    case _ => None
}(StringToKelvin.reverseGet)

assert (StringToKelvin.getOption("12K") == Some(Kelvin(12)))
assert (StringToCelcius.getOption("3℃") == Some(Celcius(3, Some("Console"))))

assert (AllTempStringsToKelvin.getOption("3℃") == Some(Kelvin(273)))
assert (AllTempStringsToKelvin.getOption("3K") == Some(Kelvin(3)))

/*
   7. Compose the three pisms and the above ISOs to parse any of three into Kelvin
*/

/*
   8. Write a function that loads Celcius Sensors from a `database`. And connect it to the reading of a temperature,
      to prepare a list of all readings as per the sensors in the `database
  */

trait Sensor[+Scale <: TemperatureScale] {
 def uuid: UUID
 def name: String
 def location: String
 def readTemperature: IO[Scale]
}

case class FujitsuTemperatureSensor(uuid: UUID, name: String, location: String) extends Sensor[Kelvin] {
    def readTemperature: IO[Kelvin] = currentTemperature.map(kelvinToCelcius.reverseGet)
}

case class GETemperatureSensor(uuid: UUID, name: String, location: String) extends Sensor[Celcius] {
    def readTemperature: IO[Celcius] = currentTemperature
}

case class FordTemperatureSensor(uuid: UUID, name: String, location: String) extends Sensor[Fahrenheit] {
    def readTemperature: IO[Fahrenheit] = currentTemperature.map(celciusToFahrenheit.get)
}

object Database {
    def loadAllSensors: IO[List[UUID]] =
        IO.delay(scala.util.Random.between(5, 12))
          .flatMap(times => IO.delay(UUID.randomUUID()).replicateA(times))
}

object System {
    def lookupSensor(uuid: UUID): IO[Sensor[TemperatureScale]] = IO.delay(scala.util.Random.between(0, 3)).map {
      case 0 => FujitsuTemperatureSensor(uuid, "fuji" + uuid, "Tokyo")
      case 1 => GETemperatureSensor(uuid, "ge" + uuid, "Chicago")
      case 2 => FordTemperatureSensor(uuid, "ford" + uuid, "Detroit")
    }
}

val temperatureToCelcius = Iso[TemperatureScale, Celcius] {
    case k:Kelvin => kelvinToCelcius.get(k)
    case c:Celcius => c
    case f:Fahrenheit => celciusToFahrenheit.reverseGet(f)
}(identity)

val sensors: IO[List[Sensor[TemperatureScale]]] = Database
  .loadAllSensors
  .flatMap(_.traverse(System.lookupSensor))

val results: IO[List[(String, Celcius)]] = sensors
  .flatMap(_.traverse(sensor => sensor
     .readTemperature
     .map(temperatureToCelcius.get)
     .map(celsiusSensLoc.set(sensor.location))
     .tupleLeft(sensor.name)))

  println(results.unsafeRunSync())

/*
0. Explain map, flatMap, IO[A]. Use map, flatMap on Option, List, Lens...
0.1. Write our own Functor and then Monad and use them in some examples;
0.2. Exercise: Take an Option[String] and always turn it into a value of String.;
0.3. Turn an Option[Int] into None if the integer in odd using only map or flatmap. Compare map and flatMap?
0.3. Turn an Option[Int] into Some(1) using only map or flatMap. Is it possible?
0.3. Turn an empty list into a three element list using map or flatMap.
0.4. Filter the odd valued elements of a list. [0, 2, 1, 3] => [0, 2] (or based on the index);
0.5. Compose two Functors together and try and do the same with Monad;

F[_]: Functor
G[_]: Functor
F[G[_]]: Functor
G[F[_]]: Functor

F[_]: Monad
G[_]: Monad
F[G[_]]: Monad?
G[F[_]]: Monad?



1. Use an optic to save a new record to a database and get an object with id (primary key) added;
2. Extend the prism to give an error when the parsing is not possible;
3. Use optics to decouple code (a method evaluates the cost of electricity given something that can give the degrees in Fahrenheit);
4. Replace extending Sensor without using inheritance.
 */

