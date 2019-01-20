// MyLittleLanguage test program

def main(): Unit = do
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

  let foo: Int = if (1 < 2) then 42 else 100
  let bar: Int = foo - 200
  printInt(bar)
  printNewline()
  printInt(IntOps.abs(bar))
  printNewline()

  printDashs()

  exitSuccess()
end

module IntOps begin
  def abs(i: Int): Int = if i < 0 then 0 - i else i
end

def fib(i: Float): Float = if i <. 3.0 then 1.0 else fib(i -. 1.0) +. fib(i -. 2.0)

def foo(i: Float): Float = i +. 3.0

def printDashs(): Unit = do
  let dash: Int = 45
  printTimes(80, dash)
  printNewline()
end

def printFloatLine(f: Float): Unit = do
  printFloat(f)
  printNewline()
end

def printTimes(n: Int, a: Int): Unit =
  if 0 < n then do
    printChar(a)
    printTimes(n - 1, a)
  end else ()

def printNewline(): Unit = do
  let newline: Int = 10
  printChar(newline)
end

