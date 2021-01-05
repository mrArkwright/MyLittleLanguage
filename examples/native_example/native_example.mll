// MyLittleLanguage test program

let main(): Unit = do

  Main.Out.printDashs()

  example1()
  example2()
  example3()
  example4()
  example5()
  example6()
  example7()
  example8()
  example9()

  Out.printDashs()

  exitSuccess()

end

let example1(): Unit = do
  let pi: Float = 3.1415927
  let tau: Float = 2.0 *. pi
  let s: Float = sin(1.0 /. 3.0 *. tau)
  Out.printFloatLine(s)
end

let example2(): Unit = do
  let y: Float = foo(42.0 -. 2.0) +. foo(42.0 -. 1.0)
  Out.printFloatLine(y)
end

let example3(): Unit = do
  let z: Float = 3.0 +. 4.5
  Out.printFloatLine(z)
end

let example4(): Unit = do
  Out.printFloatLine(fib(30.0))
  Out.printFloatLine(sqrt(3.0) /. 2.0)
end

let example5(): Unit = do
  let bar: Int = if (1 < 2) then 42 else 100
  let baz: Int = bar - 200
  Out.printIntLine(baz)
  Out.printIntLine(Ops.abs(baz))
end

let example6(): Unit = do
  let plusFive(i: Int): Int = i + 5
  let result: Int = plusFive(53)
  Out.printIntLine(result)
end

let example7(): Unit = do
  let plusThreeLocal: (Int) -> Int = plusThree
  let result: Int = plusThreeLocal(42)
  Out.printIntLine(result)
end

let example8(): Unit = do
  let minusFive(i: Int): Int = i - 5
  let result: Int = Ops.chain(plusThree, minusFive, 10)
  Out.printIntLine(result)
end

let example9(): Unit = do
  let minusFive(f: Float): Float = f -. 5.0
  let result: Float = minusFive(10.0)
  Out.printFloatLine(result)
end

let plusThree(i: Int): Int = i + 3

module Ops begin

  let abs(i: Int): Int = if i < 0 then 0 - i else i

  let chain(f: (Int) -> Int, g: (Int) -> Int, i: Int): Int = g(f(i))

  let repeat(f: () -> Unit, n: Int): Unit = if 0 < n then do
    f()
    repeat(f, n - 1)
  end else ()

end

let fib(i: Float): Float = if i <. 3.0 then 1.0 else fib(i -. 1.0) +. fib(i -. 2.0)

let foo(i: Float): Float = i +. 3.0

module Out begin

  let dash: Int = 45
  let newline: Int = 10

  let printDashs(): Unit = do
    let printDash(): Unit = printChar(dash)
    Ops.repeat(printDash, 80)
    printChar(newline)
  end

  let printFloatLine(f: Float): Unit = do
    printFloat(f)
    printChar(newline)
  end

  let printIntLine(i: Int): Unit = do
    printInt(i)
    printChar(newline)
  end

end
