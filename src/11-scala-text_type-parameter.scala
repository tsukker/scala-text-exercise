// ScalaText type-parameter
object Main extends App{
    trait Stack[+A] {
        def push[E >: A](e: E): Stack[E]
        def top: A
        def pop: Stack[A]
        def isEmpty: Boolean
    }

    class NonEmptyStack[+A](private val first: A, private val rest: Stack[A]) extends Stack[A] {
        def push[E >: A](e: E): Stack[E] = new NonEmptyStack[E](e, this)
        def top: A = first
        def pop: Stack[A] = rest
        def isEmpty: Boolean = false
    }

    case object EmptyStack extends Stack[Nothing] {
        def push[E >: Nothing](e: E): Stack[E] = new NonEmptyStack[E](e, this)
        def top: Nothing = throw new IllegalArgumentException("empty stack")
        def pop: Nothing = throw new IllegalArgumentException("empty stack")
        def isEmpty: Boolean = true
    }

    object Stack {
        def apply(): Stack[Nothing] = EmptyStack
    }

    if (true) {
        var intStack: Stack[Int] = Stack()
        println(intStack)  // intStack: Stack[Int] = EmptyStack
        intStack = intStack.push(100)
        println(intStack.top)
        intStack = intStack.push(200)
        println(intStack.top)
        intStack = intStack.pop
        println(intStack.top)

        var stringStack: Stack[String] = Stack()
        println(stringStack)  // stringStack: Stack[String] = EmptyStack
        stringStack = stringStack.push("hoge")
        println(stringStack.top)
        stringStack = stringStack.push("fuga")
        println(stringStack.top)
        stringStack = stringStack.pop
        println(stringStack.top)
    }
}
