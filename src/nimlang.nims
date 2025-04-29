if defined emscripten:
  switch "os", "linux"
  switch "cpu", "wasm32"
  switch "cc", "clang"

  switch "clang.exe", "emcc"
  switch "clang.linkerexe", "emcc"
  switch "clang.cpp.exe", "emcc"
  switch "clang.cpp.linkerexe", "emcc"
  # switch "listCmd"

  switch "gc", "arc" # TODO: use orc

  # switch "exceptions", "cpp"

  # switch "passC", "--no-entry"
  # switch "passL", "--no-entry"
  switch "passL", "-sALLOW_MEMORY_GROWTH"

  # setCommand "cpp"
  # switch "exceptions", "cpp"

  switch "passL", "-o dist/index.html"

  # switch "passL", "-sEXIT_RUNTIME=1"

  # switch "noMain", "off"

  setCommand "c"
  switch "exceptions", "goto"

  switch "define", "noSignalHandler"

  switch "passL", "-sEXPORTED_RUNTIME_METHODS=ccall,cwrap,UTF8ToString,callMain"
  switch "passL", "-sNO_DISABLE_EXCEPTION_CATCHING=1"

  switch "passL", "-gsource-map" # Generate source maps for debugging.
  switch "passC", "-gsource-map" # Generate source maps for debugging.

  # switch "passC", "-o dist/lox.wasm"
  switch "passL", "-o dist/loxwasm.js"
