// ScalaText error-handling MainRefactored
object Main extends App{

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
    abstract class Failure extends PostalCodeResult
    case object UserNotFound extends Failure
    case object UserNotHasAddress extends Failure
    case object AddressNotFound extends Failure
    case object AddressNotHasPostalCode extends Failure

    // 本質的に何をしているかわかりやすくリファクタリング
    def getPostalCodeResult(userId: Int): PostalCodeResult = {
        (for {
            user <- findUser(userId)
            address <- findAddress(user)
            postalCode <- findPostalCode(address)
        } yield Success(postalCode)).merge
    }

    def findUser(userId: Int): Either[Failure, User] = {
        println("Function findUser, userId: " + userId)
        userDatabase.get(userId).toRight(UserNotFound)
    }

    def findAddress(user: User): Either[Failure, Address] = {
        println("Function findAddress, user: " + user)
        for {
            addressId <- user.addressId.toRight(UserNotHasAddress)
            address <- addressDatabase.get(addressId).toRight(AddressNotFound)
        } yield address
    }

    def findPostalCode(address: Address): Either[Failure, String] = {
        println("Function findPostalCode, address: " + address)
        println("Function findPostalCode, address.postalCode: " + address.postalCode)
        address.postalCode.toRight(AddressNotHasPostalCode)
    }

    for (userId <- 1 to 4) {
        //println(getPostalCodeResult(userId))
    }
    for (userId <- 1 to 4) {
        println("----- userId(" + userId + ") start -----")
        val result = findUser(userId).flatMap(user => {
            println("Class of user: " + user.getClass)
            findAddress(user).flatMap(address => {
                println("Class of address: " + address.getClass)
                findPostalCode(address).map(postalCode => {
                    println("Class of postalCode: " + postalCode.getClass)
                    Success(postalCode)
                })
            })
        })
        println("Result: " + result)
        println("Result merged: " + result.merge)
        println("----- end -----")
        println("")
    }
}
