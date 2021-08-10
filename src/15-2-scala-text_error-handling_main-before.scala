// ScalaText error-handling MainBefore
object Main extends App{
    object MainBefore {

        case class Address(id: Int, name: String, postalCode: Option[String])
        case class User(id: Int, name: String, addressId: Option[Int])

        val userDatabase: Map[Int, User] = Map (
            1 -> User(1, "太郎", Some(1)),
            2 -> User(2, "二郎", Some(2)),
            3 -> User(3, "プー太郎", None)
        )

        val addressDatabase: Map[Int, Address] = Map (
            1 -> Address(1, "渋谷", Some("150-0002")),
            2 -> Address(2, "国際宇宙ステーション", None)
        )

        sealed abstract class PostalCodeResult
        case class Success(postalCode: String) extends PostalCodeResult
        sealed abstract class Failure extends PostalCodeResult
        case object UserNotFound extends Failure
        case object UserNotHasAddress extends Failure
        case object AddressNotFound extends Failure
        case object AddressNotHasPostalCode extends Failure

        // どこでNoneが生じたか取得しようとするとfor式がつかえず地獄のようなネストになる
        def getPostalCodeResult(userId: Int): PostalCodeResult = {
            findUser(userId) match {
                case Some(user) =>
                    user.addressId match {
                        case Some(addressId) =>
                            findAddress(addressId) match {
                                case Some(address) =>
                                    address.postalCode match {
                                        case Some(postalCode) => Success(postalCode)
                                        case None => AddressNotHasPostalCode
                                    }
                                case None => AddressNotFound
                            }
                        case None => UserNotHasAddress
                    }
                case None => UserNotFound
            }
        }

        def findUser(userId: Int): Option[User] = {
            userDatabase.get(userId)
        }

        def findAddress(addressId: Int): Option[Address] = {
            addressDatabase.get(addressId)
        }

        def main(args: Array[String]): Unit = {
            println(getPostalCodeResult(1)) // Success(150-0002)
            println(getPostalCodeResult(2)) // AddressNotHasPostalCode
            println(getPostalCodeResult(3)) // UserNotHasAddress
            println(getPostalCodeResult(4)) // UserNotFound
        }
    }
    for (userId <- 1 to 4) {
        println(MainBefore.getPostalCodeResult(userId))
    }
    println(MainBefore.userDatabase)
}
