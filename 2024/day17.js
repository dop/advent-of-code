const assert = require('assert')
const fs = require('fs')

let [registers, program] = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')

const [A, B, C] = registers.match(/\d+/g).map(Number)
program = program.match(/\d+/g).map(Number)

function run(ra, rb, rc) {
  let i = 0
  let out = []

  const val = (op) => {
    if (op < 4) return op
    if (op == 4) return ra
    if (op == 5) return rb
    if (op == 6) return rc
    assert.fail("bad op " + op)
  }

  while (i < program.length - 1) {
    let oc = program[i]
    let op = program[i+1]
    // console.log(i, ':', oc, op, [ ra, rb, rc ])
    if (oc == 0) {
      ra = Math.floor(ra / Math.pow(2, val(op)))
      i += 2
    }
    else if (oc == 1) {
      rb = Number(BigInt(rb) ^ BigInt(op))
      i += 2
    }
    else if (oc == 2) {
      rb = val(op) % 8
      i += 2
    }
    else if (oc == 3) {
      if (ra != 0) {
        i = op
      } else {
        i += 2
      }
    }
    else if (oc == 4) {
      rb = Number(BigInt(rb) ^ BigInt(rc))
      i += 2
    }
    else if (oc == 5) {
      out.push(val(op) % 8)
      i += 2
    }
    else if (oc == 6) {
      rb = Math.floor(ra / Math.pow(2, val(op)))
      i += 2
    }
    else if (oc == 7) {
      rc = Math.floor(ra / Math.pow(2, val(op)))
      i += 2
    }
  }
  return out
}

console.log(run(A, B, C).join(','))

let candidates = [0]
for (let j = program.length - 1; j >= 0; j--) {
  let next = []
  for (let cand of candidates) {
    for (let k = 0; k < 8; k++) {
      let a = cand * 8 + k
      if (run(a, B, C)[0] == program[j]) {
        next.push(a)
      }
    }
  }
  candidates = next
}

assert(run(candidates[0], B, C).join(',') === program.join(','))

console.log(candidates[0])
