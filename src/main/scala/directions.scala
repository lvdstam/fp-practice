enum Direction {
 case North, West, South, East

 def next: Direction = Direction.fromOrdinal((ordinal + 1) % 4)
 def prev: Direction = Direction.fromOrdinal((ordinal + 3) % 4)
}

import Direction._

object Application extends App {
  assert(North.next == West)
  assert(North.prev == East)
}