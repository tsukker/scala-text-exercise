// ScalaText case-class-and-pattern-matching
object Main extends App{
    sealed abstract class DayOfWeek
    case object Sunday extends DayOfWeek
    case object Monday extends DayOfWeek
    case object Tuesday extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday extends DayOfWeek
    case object Friday extends DayOfWeek
    case object Saturday extends DayOfWeek

    def nextDayOfWeek(d: DayOfWeek): DayOfWeek = {
        d match {
            case Sunday => Monday
            case Monday => Tuesday
            case Tuesday => Wednesday
            case Wednesday => Thursday
            case Thursday => Friday
            case Friday => Saturday
            case Saturday => Sunday
        }
    }
    if (false) {
        println(nextDayOfWeek(Sunday))
    }

    sealed abstract class Tree
    case class Node(value: Int, left: Tree, right: Tree) extends Tree
    case object Empty extends Tree
    def max(tree: Tree): Int = {
        tree match {
            case Node(v, left, right) => v.max(max(left)).max(max(right))
            case Empty => Int.MinValue
        }
    }
    def min(tree: Tree): Int = {
        tree match {
            case Node(v, left, right) => v.min(min(left)).min(min(right))
            case Empty => Int.MaxValue
        }
    }
    def depth(tree: Tree): Int = {
        tree match {
            case Node(v, left, right) => depth(left).max(depth(right)) + 1
            case Empty => 0
        }
    }
    if (false) {
        assert(max(Node(10, Node(20, Empty, Empty), Empty)) == 20)
        println(max(Node(10, Node(20, Empty, Empty), Empty)))
        println(min(Node(10, Node(20, Empty, Empty), Empty)))
    }
    if (true) {
        assert (depth(Empty) == 0)
        assert (depth(Node(10, Empty, Empty)) == 1)
        assert (depth(Node(10, Node(20, Empty, Empty), Empty)) == 2)
        // 右のBranchの方が、左のBranchよりも深い
        assert (depth(Node(10, Node(20, Empty, Empty), Node(30, Node(40, Empty, Empty), Empty))) == 3)
    }
    def traverse(tree: Tree): List[Int] = {
        tree match {
            case Node(v, left, right) => v :: (traverse(left) ++ traverse(right))
            case Empty => Nil
        }
    }
    def insert(tree: Tree, value: Int): Tree = {
        tree match {
            case Node(v, left, right) => {
                if (value <= v) {
                    return Node(v, insert(left, value), right)
                } else {
                    return Node(v, left, insert(right, value))
                }
            }
            case Empty => Node(value, Empty, Empty)
        }
    }
    def sort(tree: Tree): Tree = {
        traverse(tree).foldLeft(Empty: Tree)((current, next) => insert(current, next))
    }
    if (true) {
        val sorted = sort(Node(10, Node(20, Empty, Empty), Node(30, Node(40, Empty, Empty), Empty)))
        println(sorted)
    }
}
