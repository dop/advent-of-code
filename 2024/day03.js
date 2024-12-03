const fs = require('fs')

const code = fs.readFileSync(0, {encoding:'utf-8'}).trim()

let rez1 = 0
let instrs = []
code.replace(/(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))/g, function (m) {
  const [c,x,y] = m.split(/[(),]/)
  instrs.push([c, x, y])
  if (c === 'mul') {
    rez1 += parseFloat(x) * parseFloat(y)
  }
})
console.log(rez1)

let rez2 = 0
let enabled = true
for (const instr of instrs) {
  if (instr[0] === 'mul' && enabled) {
    rez2 += parseFloat(instr[1]) * parseFloat(instr[2])
  }
  if (instr[0] === 'do') {
    enabled = true
  }
  if (instr[0] === 'don\'t') {
    enabled = false
  }
}
console.log(rez2)
