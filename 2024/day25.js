const assert = require('assert')
const fs = require('fs')

let blocks = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')

let keys = []
let locks = []

let H, W
for (let block of blocks) {
  block = block.split('\n')
  if (H)
    assert.equal(H, block.length)
  else
    H = block.length
  if (W)
    assert.equal(W, block[0].length)
  else
    W = block[0].length
  assert(block[0][0] != block.at(-1)[0])
  if (block[0][0] == '#') {
    locks.push(block)
  } else {
    keys.push(block)
  }
}

let count = 0
for (let i = 0; i < keys.length; i++) {
  for (let j = 0; j < locks.length; j++) {
    let key = keys[i]
    let lock = locks[j]
    let ok = true
    check: {
      for (let r = 0; r < lock.length; r++) {
        for (let c = 0; c < lock[0].length; c++) {
          if (lock[r][c] == '#' && key[r][c] == '#') {
            ok = false
            break check
          }
        }
      }
    }
    if (ok)
      count++
  }
}
console.log(count)

