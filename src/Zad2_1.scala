object Zad2_1 extends App {

  type Predicate[T] = T => Boolean

  object Password {

    protected val lowercase: Set[Char] = ('a' to 'z') toSet
    protected val uppercase: Set[Char] = ('A' to 'Z') toSet
    protected val digits: Set[Char] = ('0' to '9') toSet

    def isProper(password: String, predicates: Predicate[String]*): Boolean =
      predicates forall (_(password))

    def minLength(min_len: Int)(password: String): Boolean =
      (password length) >= min_len

    def maxLength(max_len: Int)(password: String): Boolean =
      (password length) <= max_len

    def hasLowercase(password: String): Boolean =
      password exists lowercase

    def hasUppercase(password: String): Boolean =
      password exists uppercase

    def hasDigits(min_count: Int)(password: String): Boolean = {
      (password count digits) >= min_count
    }
  }

  assert(Password.isProper("any"))

  assert(Password.isProper("any", Password.minLength(3)))
  assert(!Password.isProper("any", Password.minLength(4)))

  assert(Password.isProper("any", Password.maxLength(5)))
  assert(!Password.isProper("any", Password.maxLength(2)))

  assert(Password.isProper("ANy", Password.hasLowercase))
  assert(!Password.isProper("ANY", Password.hasLowercase))

  assert(Password.isProper("anY", Password.hasUppercase))
  assert(!Password.isProper("any", Password.hasUppercase))

  assert(Password.isProper("an2Y121", Password.hasDigits(1)))
  assert(!Password.isProper("any", Password.hasDigits(1)))

  assert(Password.isProper("an2Y121", Password.hasDigits(2)))
  assert(!Password.isProper("any1", Password.hasDigits(2)))

  assert(Password.isProper("any99Password",
    Password.minLength(3),
    Password.maxLength(20),
    Password.hasLowercase,
    Password.hasUppercase,
    Password.hasDigits(2)
  ))

  assert(!Password.isProper("any99password",
    Password.minLength(3),
    Password.maxLength(20),
    Password.hasLowercase,
    Password.hasUppercase,
    Password.hasDigits(2)
  ))

  println("END.")
}
