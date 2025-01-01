const assert = require('assert')
const fs = require('fs')

let code = fs.readFileSync(0, {encoding: 'utf-8'})
  .trim().split('\n\n')[0]
  .split('\n').map(line => line.split(' '))

// console.log(code)

function run(a, b, c, d) {
  let registers = { a, b, c, d }
  let val = x => x in registers ? registers[x] : Number(x)
  let i = 0
  while (i < code.length) {
    let [ins, x, y] = code[i]
    switch (ins) {
    case 'cpy':
      registers[y] = val(x)
      i++
      break
    case 'inc':
      registers[x]++
      i++
      break
    case 'dec':
      registers[x]--
      i++
      break
    case 'jnz':
      if (val(x))
        i += val(y)
      else
        i++
      break
    }
  }
  return registers
}

console.log(run(0, 0, 0, 0).a)
console.log(run(0, 0, 1, 0).a)
