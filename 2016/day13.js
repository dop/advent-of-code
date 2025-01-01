const assert = require('assert')
const {Heap} = require('../heap')

const input = 1352

// Find x*x + 3*x + 2*x*y + y + y*y.
// Add the office designer's favorite number (your puzzle input).
// Find the binary representation of that sum; count the number of bits that are 1.
//   If the number of bits that are 1 is even, it's an open space.
//   If the number of bits that are 1 is odd, it's a wall.

let maze = new Map

function is_open(r, c) {
  let k = [r,c]+''
  if (maze.has(k))
    return maze.get(k)
  let a = c*c + 3*c + 2*c*r + r + r*r + input
  let parity = 0
  for (let i = 1; i <= a; i *= 2) {
    if (a & i)
      parity++
  }
  let result = parity % 2 == 0
  maze.set(k, result)
  return result
}

function print(W, H) {
  for (let r = 0; r < H; r++) {
    for (let c = 0; c < W; c++) {
      if (r == 39 && c == 31)
        process.stdout.write('X')
      else if (is_open(r, c))
        process.stdout.write('.')
      else
        process.stdout.write('#')
    }
    process.stdout.write('\n')
  }
}

print(35, 44)

let in_bounds = (r, c) => 0 <= r && 0 <= c
let moves = [[0, 1], [1, 0], [0, -1], [-1, 0]]

function dijkstra(start, target) {
  let D = new Map
  let V = new Set
  let H = new Heap((a, b) => a[0] <= b[0])
  H.insert([0, start])
  while (H.peak()) {
    let [d, rc] = H.remove()
    let k = rc+''
    V.add(k)
    D.set(k, Math.min(d, D.get(k) ?? Infinity))
    if (target[0] == rc[0] && target[1] == rc[1]) {
      //
    } else {
      for (let [dr, dc] of moves) {
        let nr = rc[0]+dr
        let nc = rc[1]+dc
        if (!V.has([nr,nc]+'') && in_bounds(nr,nc) && is_open(nr, nc))
          H.insert([d+1,[nr,nc]])
      }
    }
  }
  return D
}

let start = [1,1]
let goal = [39,31]
let D = dijkstra(start, goal)

console.log( D.get(goal+'') )

console.log( D.values().reduce((count, d) => count + (d <= 50), 0) )
