package net.degoes.zio

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import zio._
import zio.clock.Clock
import zio.console.putStrLn
import zio.duration.Duration

import scala.io.Source
import scala.util.Using

object ZIOTypes {
  type ??? = Nothing

  /**
    * EXERCISE 1
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends App {

  import zio.console._

  /**
    * EXERCISE 2
    *
    * Implement a simple "Hello World!" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!") as 0
}

object PrintSequence extends App {

  import zio.console._

  /**
    * EXERCISE 3
    *
    * Using `*>` (`zipRight`), compose a sequence of `putStrLn` effects to
    * produce an effect that prints three lines of text to the console.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("1") *> putStrLn("2") *> putStrLn("3") as 0
}

object ErrorRecovery extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 4
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    failed fold(_ => 1, _ => 0)

  //    failed as 0 orElse ZIO.succeed(1)
}

object Looping extends App {

  import zio.console._

  /**
    * EXERCISE 5
    *
    * Implement a `repeat` combinator using `flatMap` and recursion.
    */
  def repeat[R, E, A](n: Int)(task: ZIO[R, E, A]): ZIO[R, E, A] =
    if (n == 1) task
    else task.flatMap(_ => repeat(n - 1)(task))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    repeat(100)(putStrLn("All work and no play makes Jack a dull boy")) as 0
}

object EffectConversion extends App {

  /**
    * EXERCISE 6
    *
    * Using ZIO.effect, convert the side-effecting of `println` into a pure
    * functional effect.
    */
  def myPrintLn(line: String): Task[Unit] = ZIO.effect(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (myPrintLn("Hello Again!") as 0) orElse ZIO.succeed(1)
}

object ErrorNarrowing extends App {

  import java.io.IOException

  import scala.io.StdIn.readLine

  implicit class Unimplemented[A](v: A) {
    def ? = ???
  }

  /**
    * EXERCISE 7
    *
    * Using `ZIO#refineToOrDie`, narrow the error type of the following
    * effect to IOException.
    */
  val myReadLine: IO[IOException, String] = ZIO.effect(readLine()).refineOrDie { case e: IOException => e }

  def myPrintLn(line: String): UIO[Unit] = UIO(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- myPrintLn("What is your name?")
      name <- myReadLine
      _ <- myPrintLn(s"Good to meet you, ${name}")
    } yield 0) orElse ZIO.succeed(1)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 8
    *
    * Using `ZIO#flatMap`, implement a simple program that asks the user for
    * their name (using `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Write your name") *> getStrLn.orDie.flatMap(putStrLn) as 0
}

object NumberGuesser extends App {

  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE 9
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      _ <- putStrLn("Guess random number")
      random <- nextInt(3)
      guess <- getStrLn.orDie
      _ <- analyzeAnswer(random + 1, guess)
    } yield 0
}

object AlarmApp extends App {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  import zio.console._
  import zio.duration._

  /**
    * EXERCISE 10
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds. Use `refineOrDie` to
    * narrow the error type as necessary.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.effect(input.toLong)
        .map(Duration(_, TimeUnit.SECONDS))
        .refineOrDie { case e: NumberFormatException => e }

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      putStrLn(s"$input is not valid number") *>
        ZIO.succeed(Duration(0, TimeUnit.SECONDS))

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /**
    * EXERCISE 11
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (getAlarmDuration.orDie >>= ZIO.sleep) *> putStrLn("Time to wakeup!!!") as 0
}

object Cat extends App {

  import java.io.IOException

  import zio.blocking._
  import zio.console._

  /**
    * EXERCISE 12
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    ZIO.accessM[Blocking](_.blocking.effectBlocking(
      Using(Source.fromFile(file))(_.mkString).get
      )).refineOrDie { case e: IOException => e }

  /**
    * EXERCISE 13
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil => (readFile(file) >>= putStrLn).orDie as 0
      case _ => putStrLn("Usage: cat <file>") as 2
    }
}

object CatIncremental extends App {

  import java.io.{FileInputStream, IOException, InputStream}

  import zio.blocking._
  import zio.console._

  /**
    * BONUS EXERCISE
    *
    * Implement a `blockingIO` combinator to use in subsequent exercises.
    */
  def blockingIO[A](a: => A): ZIO[Blocking, IOException, A] =
    ZIO.accessM[Blocking](_.blocking.effectBlocking(a))
      .refineToOrDie[IOException]

  /**
    * EXERCISE 14
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private(private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] = blockingIO(is.close())

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] = blockingIO {
      val array = Array.ofDim[Byte](1024)
      val bytesRead = is.read(array)
      if (bytesRead == -1) None else Some(Chunk.fromArray(array.take(bytesRead)))
    }
  }

  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blockingIO(FileHandle(new FileInputStream(file)))
  }

  /**
    * EXERCISE 15
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = args match {
    case List(fileName) => {
      val file = ZManaged.make(FileHandle.open(fileName))(_.close.ignore)
      file.use {
        f =>
          def cat(): ZIO[ZEnv, IOException, Unit] = f.read.flatMap {
            case None => ZIO.unit
            case Some(bytes) => putStr(new String(bytes.toArray)) *> cat()
          }
          cat()
      } as 0
      }.orDie
    case _ => putStrLn("Usage: cat file") as -1
  }

}

object AlarmAppImproved extends App {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  import zio.console._
  import zio.duration._

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .effect(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
          )
        .refineToOrDie[NumberFormatException]

    val fallback = putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }


  /**
    * EXERCISE 16
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
    * prints a dot every second that the alarm is sleeping for, and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def tick: URIO[Console with Clock, Unit] =
    ZIO.sleep(Duration(1, TimeUnit.SECONDS)) *> putStr(".")

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      d <- getAlarmDuration.orDie
      _ <- tick.forever.race(ZIO.sleep(d))
      _ <- putStrLn("\nTime to wakeup!!!")
    } yield 0
}

object ComputePi extends App {

  import zio.random._
  import zio.console._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(
    inside: Ref[Long],
    total: Ref[Long]
  )

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
    * EXERCISE 17
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    def calculatePoint(piState: PiState): URIO[ZEnv, Unit] =
      piState.total.update(_ + 1) *>
      randomPoint >>= (p =>
        ZIO.when(insideCircle(p._1, p._2))(piState.inside.update(_ + 1))
      )

    val threshold: Double = 0.0001
    // real PI 3.141592653589793

    def printPi(piState: PiState, lastPiRef: Ref[Double]): URIO[Clock with Console, Unit] =
      for {
        inside <- piState.inside.get
        total <- piState.total.get
        newPi = estimatePi(inside, total)
        lastPi <- lastPiRef.get
        _ <-
          if(Math.abs(newPi - lastPi) < threshold)
            putStrLn(s"Pi is: ${newPi} +- ${threshold}")
          else
            putStrLn(s"Pi estimate is: ${newPi}") *>
              ZIO.sleep(Duration(5, TimeUnit.SECONDS)) *>
              lastPiRef.set(newPi) *>
              printPi(piState, lastPiRef)
      } yield ()

    for {
      inside <- Ref.make(0L)
      total <- Ref.make(0L)
      lastPi <- Ref.make(0D)
      piState = PiState(inside, total)
      _ <- ZIO.raceAll(
        printPi(piState, lastPi),
        List.fill(8)(calculatePoint(piState).forever)
      )
    } yield 0

  }

}

object StmSwap extends App {

  import zio.console._
  import zio.stm._

  val ioCount = new AtomicInteger()

  /**
    * EXERCISE 18
    *
    * Demonstrate the following code does not reliably swap two values in the
    * presence of concurrency.
    */
  def exampleRef = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _ <- ZIO.effectTotal(ioCount.incrementAndGet())
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      } yield ()

    for {
      ref1 <- Ref.make(100)
      ref2 <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get) (_ + _)
    } yield value
  }

  val stmCount = new AtomicInteger()

  /**
    * EXERCISE 19
    *
    * Using `STM`, implement a safe version of the swap function.
    */
  def exampleStm = {
    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] =
      (for {
        v1 <- ref1.get
        v2 <- ref2.get
        // do not do this in real life, it is just demonstrating STM retries
        // one can not have effects in STM
        _ = stmCount.incrementAndGet()
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      } yield ()).commit

    for {
      ref1 <- TRef.make(100).commit
      ref2 <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get) (_ + _).commit
    } yield value
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Run this multiple times, correct result should be 100") *>
    putStr("IORef result: ") *>
    exampleRef.map(_.toString).flatMap(putStrLn) *>
    putStrLn(s"IORef count: ${ioCount.get()}") *>
    putStr("STM result: ") *>
    exampleStm.map(_.toString).flatMap(putStrLn) *>
    putStrLn(s"STM count: ${stmCount.get()}")  as 0
}

object StmLock extends App {

  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 20
    *
    * Using STM, implement a simple binary lock by implementing the creation,
    * acquisition, and release methods.
    */
  class Lock private(tref: TRef[Boolean]) {
    def acquire: UIO[Unit] = tref.set(true).commit

    def release: UIO[Unit] = tref.set(false).commit
  }

  object Lock {
    def make: UIO[Lock] = TRef.make(false).map(new Lock(_)).commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      lock <- Lock.make
      fiber1 <- lock.acquire
        .bracket_(lock.release)(putStrLn("Bob  : I have the lock!"))
        .repeat(Schedule.recurs(10))
        .fork
      fiber2 <- lock.acquire
        .bracket_(lock.release)(putStrLn("Sarah: I have the lock!"))
        .repeat(Schedule.recurs(10))
        .fork
      _ <- (fiber1 zip fiber2).join
    } yield 0) as 1
}

object StmLunchTime extends App {

  import zio.stm._

  /**
    * EXERCISE 21
    *
    * Using STM, implement the missing methods of Attendee.
    */
  final case class Attendee(state: TRef[Attendee.State]) {

    def isStarving: STM[Nothing, Boolean] = state.get.map(_ == Attendee.State.Starving)

    def feed: STM[Nothing, Unit] = state.set(Attendee.State.Full)
  }

  object Attendee {

    sealed trait State

    object State {

      case object Starving extends State

      case object Full extends State

    }

  }

  /**
    * EXERCISE 22
    *
    * Using STM, implement the missing methods of Table.
    */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z@Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] = seats.update(index, _ => true).unit

    def vacateSeat(index: Int): STM[Nothing, Unit] = seats.update(index, _ => false).unit
  }

  /**
    * EXERCISE 23
    *
    * Using STM, implement a method that feeds a single attendee.
    */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    for {
      index <- t.findEmptySeat.collect { case Some(index) => index }
      _ <- t.takeSeat(index) *> a.feed *> t.vacateSeat(index)
    } yield ()

  /**
    * EXERCISE 24
    *
    * Using STM, implement a method that feeds only the starving attendees.
    */
  def feedStarving(table: Table, list: List[Attendee]): UIO[Unit] =
    ZIO.traversePar_(list)(
      a =>
      (a.isStarving >>= (s =>
        if(s) feedAttendee(table, a) else STM.unit
      )).commit
    )

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(
        i =>
          TRef
            .make[Attendee.State](Attendee.State.Starving)
            .map(Attendee(_))
            .commit
        )
      table <- TArray
        .fromIterable(List.fill(TableSize)(false))
        .map(Table)
        .commit
      _ <- feedStarving(table, attendees)
    } yield 0
  }
}

object StmPriorityQueue extends App {

  import zio.console._
  import zio.duration._
  import zio.stm._

  /**
    * EXERCISE 25
    *
    * Using STM, design a priority queue, where lower integers are assumed
    * to have higher priority than higher integers.
    */
  class PriorityQueue[A] private(
    minLevel: TRef[Int],
    map: TMap[Int, TQueue[A]]
  ) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      for {
        min <- minLevel.get
        _ <- if(priority < min) minLevel.set(priority) else STM.unit
        tqOpt <- map.get(priority)
        _ <- tqOpt.fold(
          TQueue.make[A](Int.MaxValue) >>= (q => q.offer(a) *> map.put(priority, q))
        )(_.offer(a))
      } yield ()

    def take: STM[Nothing, A] =
      for {
        min <- minLevel.get
        tqOpt <- map.get(min)
        tq <- tqOpt.fold[STM[Nothing, TQueue[A]]](STM.retry)(STM.succeed)
        a <- tq.take
        size <- tq.size
        _ <-
          if(size == 0)
            map.delete(min) *> map.keys.map(l => if(l.isEmpty) Int.MaxValue else l.min) >>= (m => minLevel.set(m))
          else STM.unit
      } yield a
  }

  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      for {
        minLevel <- TRef.make(Int.MaxValue)
        map <- TMap.empty[Int, TQueue[A]]
      } yield new PriorityQueue(minLevel, map)
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(1.millis) *> queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(2.millis) *> queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
        .flatMap(putStrLn)
        .forever
        .fork *>
        getStrLn
    } yield 0).fold(_ => 1, _ => 0)
}

object StmReentrantLock extends App {

  import zio.stm._

  private final case class WriteLock(
    writeCount: Int,
    readCount: Int,
    fiberId: FiberId
  )

  private final class ReadLock private(readers: Map[Fiber.Id, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
        )
    }
  }

  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: Fiber.Id, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
    * EXERCISE 26
    *
    * Using STM, implement a reentrant read/write lock.
    */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = data.get.map(_.fold(_ => 0, _.writeCount)).commit

    def writeLocked: UIO[Boolean] = writeLocks.map(_ > 0)

    def readLocks: UIO[Int] = data.get.map(_.fold(_.total, _.readCount)).commit

    def readLocked: UIO[Boolean] = readLocks.map(_ > 0)

    val read: Managed[Nothing, Int] = ???

    val write: Managed[Nothing, Int] = ???
  }

  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] =
      TRef
        .make[Either[ReadLock, WriteLock]](Left(ReadLock.empty))
        .map(tref => new ReentrantReadWriteLock(tref))
        .commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = ???
}

object Sharding extends App {

  /**
    * EXERCISE 27
    *
    * Create N workers reading from a Queue, if one of them fails, then wait
    * for the other ones to process their current item, but terminate all the
    * workers.
    */
  def shard[R, E, A](
    queue: Queue[A],
    n: Int,
    worker: A => ZIO[R, E, Unit]
  ): ZIO[R, E, Nothing] =
    ???

  def run(args: List[String]) = ???
}

object Hangman extends App {

  import java.io.IOException

  import zio.console._
  import zio.random._

  /**
    * EXERCISE 28
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] = ???

  /**
    * EXERCISE 29
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] = ???

  /**
    * EXERCISE 30
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] = ???

  /**
    * EXERCISE 31
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] = ???

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      * f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      * Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult

  object GuessResult {

    case object Won extends GuessResult

    case object Lost extends GuessResult

    case object Correct extends GuessResult

    case object Incorrect extends GuessResult

    case object Unchanged extends GuessResult

  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 32
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ???
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {

  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }

    final def render: String = renderChar.toString
  }

  object Mark {

    case object X extends Mark

    case object O extends Mark

  }

  final case class Board private(value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
          )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
      row0: Int,
      col0: Int,
      rowInc: Int,
      colInc: Int,
      mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
      row0: Int,
      col0: Int,
      rowInc: Int,
      colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }

  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
      first: Iterable[Char],
      second: Iterable[Char],
      third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
              )
            )
          )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
      )
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
