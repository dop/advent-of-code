const fs = require('fs')

let equations = fs.readFileSync(0, {encoding:'utf-8'})
  .trim().split('\n').map(row => row.split(/[ :]+/).map(Number))

// console.log({equations})

function is_possible(rez, current, operands) {
  if (rez === current && operands.length === 0) {
    return true;
  }
  if (operands.length) {
    const [operand, ...rest] = operands
    return is_possible(rez, current + operand, rest) || is_possible(rez, current * operand, rest)
  }
  return false;
}

function is_possible2(rez, current, operands) {
  if (rez === current && operands.length === 0) {
    return true;
  }
  if (operands.length) {
    const [operand, ...rest] = operands
    return is_possible2(rez, current + operand, rest) ||
      is_possible2(rez, current * operand, rest) ||
      is_possible2(rez, Number(current.toString() + operand.toString()), rest)
  }
  return false;
}

let good = []
let good2 = []

for (let [rez, o, ...r] of equations) {
  if (is_possible(rez, o, r)) {
    good.push(rez)
  }
  if (is_possible2(rez, o, r)) {
    good2.push(rez)
  }
}

let rez1 = good.reduce((sum, x) => sum + x, 0)
let rez2 = good2.reduce((sum, x) => sum + x, 0)

console.log(rez1)
console.log(rez2)
