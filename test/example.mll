// MyLittleLanguage test program

def fib(x: Float): Float = if x < 3.0 then 1.0 else fib(x-1.0) + fib(x-2.0)

def printTimes(n: Int, x: Int): Unit =
  if 1.0 < n then do
    printChar(x)
    printTimes(n - 1.0, x)
  end else ()

def foo(i: Int): Unit = do
  printInt(i)
  printChar(10.0)
end

def printDashs(): Unit = do
  printTimes(80.0, 45.0)
  printChar(10.0)
end

def main(): Unit = do
  let f: Float = fib(30.0)
  let twopi: Float = 2.0 * 3.14
  let s: Float = sin(1.0 / 3.0 * twopi)

  printDashs()

  printFloat(f)
  printChar(10.0)

  printFloat(sin(1.0 / 3.0 * twopi))
  printChar(10.0)

  foo(3)

  printDashs()

  exitSuccess()
end

/*

// code idea

let endline = '\n'

let printEndline = print endline

let main = do
    let f = fib 30
    let tau = 6.28
    let s = sin (1.0 / 3.0 * tau)

    for 1..80 $ print "-"
    printEndline

    printLine f
    printLine s

    for 1..80 do
        print "-"
    end
    printEndline
end

*/
