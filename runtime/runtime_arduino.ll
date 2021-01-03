declare void @Main.setup()

declare void @Main.loop()

define void @setup() {
entry:
  tail call void @Main.setup()
  ret void
}

define void @loop() {
entry:
  tail call void @Main.loop()
  ret void
}
