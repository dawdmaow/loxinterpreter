# Package

version = "0.1.0"
author = "Dawid Kotliński"
description = "A new awesome nimble package"
license = "MIT"
srcDir = "src"
bin = @["nimlang"]
# backend = "js"
backend = "c"
binDir = "dist"

# Dependencies

requires "nim >= 2.2.2", "print"

task test, "General tests":
  exec "nim --verbosity:0 --hints:off r testprint.nim"

task watch, "Watch tests":
  exec "watchexec -e nim,nims,nimble --shell bash -c -r \"nimble test\""

task release, "Build a JS release":
  # exec "nim js --sourceMap:on -o:dist/nimlang.js src/nimlang.nim"
  # cpFile "dist/nimlang.js", "svelte/static/lox.js"
  # cpFile "dist/nimlang.js.map", "svelte/static/lox.js.map"
  # exec "nim js --sourceMap:on -o:svelte/static/lox.js src/nimlang.nim"
  exec "nim js -o:svelte/static/lox.js src/nimlang.nim"

task wasm, "WASM Build":
  exec "nim c -d:emscripten src/nimlang.nim"
  cpFile "dist/loxwasm.js", "svelte/static/loxwasm.js"
  cpFile "dist/loxwasm.wasm", "svelte/static/loxwasm.wasm"
