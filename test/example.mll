// MyLittleLanguage test program

def fib(x)
  if x < 3.0 then
    1.0
  else
    fib(x-1.0) + fib(x-2.0)

def printTimes(n x)
  if 1.0 < n then do
    printChar(x)
    printTimes(n - 1.0, x)
  end else 0.0

def foo(i) do
  printInt(i)
  printChar(10.0)
end

def printDashs() do
  printTimes(80.0, 45.0)
  printChar(10.0)
end



def main() do
  let f = fib(30.0)
  let twopi = 2.0 * 3.14
  let s = sin(1.0 / 3.0 * twopi)

  printDashs()

  printDouble(f)
  printChar(10.0)

  printDouble(sin(1.0 / 3.0 * twopi))
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
