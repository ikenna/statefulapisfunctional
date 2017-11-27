
package imperative {

  import scala.reflect.ClassTag

  class Stack[T: ClassTag](capacity: Int) {

    private val items = new Array[T](capacity)
    private var topIndex: Int = 0

    def isEmpty: Boolean = topIndex == 0

    def isFull: Boolean = topIndex == items.length

    def pop(): T = {
      topIndex -= 1
      items(topIndex)
    }

    def push(item: T): Unit = {
      items(topIndex) = item
      topIndex += 1
    }

    def peek(): T = {
      items(topIndex - 1)
    }

    override def toString: String = s"Stack[topIndex = $topIndex, items= ${items.mkString(",")}]"
  }

  object StackTest extends App {
    val stack = new Stack[Int](3)
    println(s"Pushing 90. Result is: ${stack.push(90)}")
    println(s"Pushing 91. Result is: ${stack.push(91)}")
    println(s"Pushing 92. Result is: ${stack.push(92)}")
    println("Should pop 92: " + stack.pop())
    println(s"Pushing 93. Result is: ${stack.push(93)}")
    println("3 items pushed. Stack is full ? " + stack.isFull)
    //stack.push(94) // throws exception
  }

  /***
    * Results:
    *
    * Pushing 90. Result is: ()
    * Pushing 91. Result is: ()
    * Pushing 92. Result is: ()
    * Should pop 92: 92
    * Pushing 93. Result is: ()
    * 3 items pushed. Stack is full ? true
    */
}


package functional {

  import cats.Eval
  import cats.data.State

  import scala.reflect.ClassTag
  import scala.util.{Failure, Success, Try}

  case class Stack[A] private(items: Array[A], topIndex: Int) {
    override def toString: String = s"Stack[topIndex = $topIndex, items= ${items.mkString(",")}]"
  }

  object Stack {

    def empty[A: ClassTag](capacity: Int): Stack[A] = new Stack[A](new Array(capacity), 0)

    def isEmpty[A]: State[Stack[A], Boolean] = State(stack => (stack, stack.topIndex == 0))

    def isFull[A]: State[Stack[A], Boolean] = State(stack => (stack, stack.topIndex == stack.items.length))

    def pop[A]: State[Stack[A], Try[A]] = State { stack =>
      val newTopIndex = stack.topIndex - 1
      val item = Try(stack.items(newTopIndex))
      val newStack = Stack(stack.items, newTopIndex)
      (newStack, item)
    }

    def push[A](item: A): State[Stack[A], Try[Unit]] = State { stack =>
      Try {
        val copyItems = stack.items.clone()
        copyItems(stack.topIndex) = item
        val newTopIndex = stack.topIndex + 1
        Stack(copyItems, newTopIndex)
      } match {
        case Success(newStack) => (newStack, Success(()))
        case Failure(e) => (stack, Failure(e))
      }
    }

    def peek[A](): State[Stack[A], Try[A]] = State { stack =>
      (stack, Try(stack.items(stack.topIndex - 1)))
    }
  }

  object StackTest extends App {
    val stack: Stack[Int] = Stack.empty[Int](capacity = 3)
    val stackOperations1 = for {
      s0 <- Stack.push(90)
      _ = println(s"Pushing 90. Result is: $s0")
      s1 <- Stack.push(91)
      _ = println(s"Pushing 91. Result is: $s1")
      s2 <- Stack.push(92)
      _ = println(s"Pushing 92. Result is: $s2")
      s3 <- Stack.pop[Int]
      _ = println(s"Pop stack. Result is: $s3")
      s4 <- Stack.push(93)
      _ = println(s"Pushing 93. Result is: $s4")
      s5 <- Stack.isFull[Int]
      _ = println(s"Is stack full ?. Result is: $s5")
      s6 <- Stack.push(94)
      _ = println(s"Pushing 94. Result is: $s6")
    } yield ()


    val eval: Eval[Stack[Int]] = stackOperations1.runS(stack) //Running the computation and printing the final value of the state
    println(eval.value)

    println("\n Now lets the same operations again, but this time examing the state as we go along... \n")

    val stackOperations2 = for {
      _ <- Stack.push(90).inspect(state => println(s"Pushing 90. Current state is: $state"))
      _ <- Stack.push(91).inspect(state => println(s"Pushing 91. Current state is: $state"))
      _ <- Stack.push(92).inspect(state => println(s"Pushing 92. Current state is: $state"))
      _ <- Stack.pop[Int].inspect(state => println(s"Pop stack. Current state is: $state"))
      _ <- Stack.push(93).inspect(state => println(s"Pushing 93. Current state is: $state"))
      _ <- Stack.isFull[Int].inspect(state => println(s"Is stack full ? Current state is: $state"))
      _ <- Stack.push(94).inspect(state => println(s"Pushing 94. Current state is: $state"))
    } yield ()

    println(stackOperations2.runS(stack).value) //Running the computation and printing the final value of the state

  }


  /***
    *
    * Results
    *
    *
    * Pushing 90. Result is: Success(())
    * Pushing 91. Result is: Success(())
    * Pushing 92. Result is: Success(())
    * Pop stack. Result is: Success(92)
    * Pushing 93. Result is: Success(())
    * Is stack full ?. Result is: true
    * Pushing 94. Result is: Failure(java.lang.ArrayIndexOutOfBoundsException: 3)
    * Stack[topIndex = 3, items= 90,91,93]
    *
    * Now lets the same operations again, but this time inspecting the state as we go along...
    *
    * Pushing 90. Current state is: Stack[topIndex = 1, items= 90,0,0]
    * Pushing 91. Current state is: Stack[topIndex = 2, items= 90,91,0]
    * Pushing 92. Current state is: Stack[topIndex = 3, items= 90,91,92]
    * Pop stack. Current state is: Stack[topIndex = 2, items= 90,91,92]
    * Pushing 93. Current state is: Stack[topIndex = 3, items= 90,91,93]
    * Is stack full ? Current state is: Stack[topIndex = 3, items= 90,91,93]
    * Pushing 94. Current state is: Stack[topIndex = 3, items= 90,91,93]
    * Stack[topIndex = 3, items= 90,91,93]
    *
    *
    */

}


