package lectures.part2oop

object Enums extends App {

  enum Permissions {
    case READ, WRITE, EXECUTE, NONE

    // Add fileds and methods

    def openDocument(): Unit =
      if (this == READ) println("opening")
      else println("get your azz outa here")
  }

  val somePermissions: Permissions = Permissions.READ

  somePermissions.openDocument()

  enum PermissionsWithBits(bits: Int) {
    case READ extends PermissionsWithBits(4)
    case WRITE extends PermissionsWithBits(2)
    case EXECUTE extends PermissionsWithBits(1)
    case NONE extends PermissionsWithBits(0)
  }

  object PermissionsWithBits {
    def fromBits(bits: Int) = // whatever logic
      PermissionsWithBits.NONE
  }

  // Stander API
  val somePermissionsOrdinal = somePermissions.ordinal
  val allPermissions = PermissionsWithBits.values // array with all values
  val readPermission = Permissions.valueOf("READ") // Permissions.READ - works only with no parametrized enums

  println(somePermissionsOrdinal)
  println(allPermissions)
  println(readPermission)
}
