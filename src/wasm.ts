const decoder = new TextDecoder()
const encoder = new TextEncoder()

const stdin = document.querySelector('#stdin') as HTMLInputElement
const stdout = document.querySelector('#stdout') as HTMLPreElement

// Unit is pages: 64kb
var memory = new WebAssembly.Memory({
  initial: 100,
  maximum: 1000
})

export type WasmImports = Readonly<{
  memory: WebAssembly.Memory
  initRepl(): number
  runLine(ctx: number): void
}>

function writeToStderr (stringPtr: number, stringLength: number): void {
  let string = decoder.decode(
    new Uint8Array(memory.buffer, stringPtr, stringLength),
    {
      stream: true
    }
  )

  stdout.textContent += string
}

function readFromStdin (bufferPtr: number, bufferLength: number): number {
  let value = stdin.value
  let buffer = new Uint8Array(memory.buffer, bufferPtr, bufferLength)

  // Write input value into provided memory (truncate if too much)
  buffer.set(encoder.encode(value).slice(0, bufferLength))

  stdin.value = ''

  return Math.min(bufferLength, value.length)
}

// In our 'build.zig' file we have configured esbuild to replace all uses of this constant with the filename of the
// compiled WebAssembly artifact.
declare const __WASM_ARTIFACT_FILENAME: string

let wasmImports: WasmImports = (
  await WebAssembly.instantiateStreaming(fetch(__WASM_ARTIFACT_FILENAME), {
    env: {
      memory: memory,
      writeToStderr: writeToStderr,
      readFromStdin: readFromStdin
    }
  } as const)
).instance.exports as WasmImports

let ctx = wasmImports.initRepl()

stdin.addEventListener('keydown', e => {
  if (e.key == 'Enter' && stdin.value.length > 0) {
    wasmImports.runLine(ctx)
  }
})
