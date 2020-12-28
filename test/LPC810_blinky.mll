// MyLittleLanguage embedded test program

let main(): Unit = do

  store(p0xA0002000, 1)
  store(p0xA0002080, 0xFFFFFFFE)
  blink(0)

end

let blink(x: Int): Unit = if 0 < x then do
  store(p0xA0002180, 1)
  wait(100000)
  blink(0)
end else do
  store(p0xA0002180, 0)
  wait(100000)
  blink(1)
end

let wait(i: Int): Unit = if 0 < i then do
  store(p0xA0002000, 1)
  wait(i - 1)
end else ()
