const assert = require('assert')
const fs = require('fs')
const {Heap} = require('../heap')

const W = 71
const H = 71
const of_str = (str) => str.split(',').map(Number)
const start = [0, 0]
const target = [W-1, H-1]
const in_bounds = (r, c) => 0 <= r && r < H && 0 <= c && c < W
const moves = [[0, 1], [1, 0], [0, -1], [-1, 0]]

const bytes = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n')

function print() {
  for (let r = 0; r < H; r++) {
    for (let c = 0; c < W; c++) {
      if (corrupted.has(r + ',' + c))
        process.stdout.write('#')
      else
        process.stdout.write('.')
    }
    process.stdout.write('\n')
  }
}

// print();

function find(corrupted) {
  const Q = new Heap((a, b) => a[1] <= b[1])
  const visited = new Map
  Q.insert([start, 0])

  while (Q.peak()) {
    const [[r, c], d] = Q.remove()

    if (r == target[0] && c == target[1]) {
      return d
    }

    if (!visited.has(r + ',' + c) || visited.get(r + ',' + c) > d) {
      visited.set(r + ',' + c, d)

      for (const [dr, dc] of moves) {
        let nr = r + dr
        let nc = c + dc
        if (in_bounds(nr, nc) && !corrupted.has(nr + ',' + nc))
          Q.insert([[nr, nc], d + 1])
      }
    }
  }
}

const corrupted = new Set(bytes.slice(0, 1024))
console.log( find(corrupted) )

for (let i = 1024; i <= bytes.length; i++) {
  corrupted.add(bytes[i])
  const d = find(corrupted)
  if (!d) {
    console.log(i, bytes[i])
    break
  }
}
