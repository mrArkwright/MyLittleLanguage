// MyLittleLanguage test program

let main(): Unit = do
  let pi: Float = 3.1415927
  let tau: Float = 2.0 *. pi
  let s: Float = sin(1.0 /. 3.0 *. tau)

  Main.printDashs()

  let y: Float = foo(42.0 -. 2.0) +. foo(42.0 -. 1.0)
  printFloatLine(y)

  let z: Float = 3.0 +. 4.5
  printFloatLine(z)

  printFloatLine(fib(30.0))
  printFloatLine(s)
  printFloatLine(sqrt(3.0) /. 2.0)

  let bar: Int = if (1 < 2) then 42 else 100
  let baz: Int = bar - 200
  printIntLine(baz)
  printIntLine(Ops.abs(baz))

  //let plusThree(i: Int): Int = i + 3
  /*let baz1: Int = Ops.higher(plusThree, 10)
  printIntLine(baz1)*/

  //let someFunc: (Int) -> Int = plusThree

  printDashs()

  exitSuccess()
end

let plusThree(i: Int): Int = i + 3

module Ops begin

  let abs(i: Int): Int = if i < 0 then 0 - i else i

  //let higher(f: (Int) -> Int, i: Int): Int = f(i)

  /*
  repeat(f: Unit -> Unit, n: Int): Unit = if 0 < n then do
    f()
    repeat(f, n - 1)
  end else ()
  */

end

let fib(i: Float): Float = if i <. 3.0 then 1.0 else fib(i -. 1.0) +. fib(i -. 2.0)

let foo(i: Float): Float = i +. 3.0

let printDashs(): Unit = do
  let dash: Int = 45
  //Ops.repeat(printChar(dash), 80)
  printTimes(80, dash)
  printNewline()
end

let printFloatLine(f: Float): Unit = do
  printFloat(f)
  printNewline()
end

let printIntLine(i: Int): Unit = do
  printInt(i)
  printNewline()
end

let printTimes(n: Int, a: Int): Unit = if 0 < n then do
  printChar(a)
  printTimes(n - 1, a)
end else ()

let printNewline(): Unit = do
  let newline: Int = 10
  printChar(newline)
end

