let setup(): Unit = ()

let loop(): Unit = do
  let high: Int8 = 08d1
  let low: Int8 = 08d0

  let ledBuiltin: Int8 = 08d13

  digitalWrite(ledBuiltin, high)
  delay(500)

  digitalWrite(ledBuiltin, low)
  delay(500)
end
