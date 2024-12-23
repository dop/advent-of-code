const assert = require('assert')
const fs = require('fs')

let maze = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n').map(row => row.split(''))

const H = maze.length
const W = maze[0].length

const of_str = (str) => str.split(',').map(Number)
const moves = [[0, 1], [1, 0], [0, -1], [-1, 0]]

let start = [0, 0]
let target = [W-1, H-1]

for (let r = 0; r < H; r++) {
  for (let c = 0; c < W; c++) {
    if (maze[r][c] == 'S')
      start = [r, c]
    else if (maze[r][c] == 'E')
      target = [r, c]
  }
}

maze[start[0]][start[1]] = '.'
maze[target[0]][target[1]] = '.'

console.log(start, target)

function print() {
  for (let r = 0; r < H; r++) {
    for (let c = 0; c < W; c++) {
      if (start[0] == r && start[1] == c)
        process.stdout.write('S')
      else if (target[0] == r && target[1] == c)
        process.stdout.write('E')
      else
        process.stdout.write(maze[r][c])
    }
    process.stdout.write('\n')
  }
}

// print()

let dists = new Map([[start+'', 0]])

let pos = [...start]
let dist = 0

while (pos+'' != target) {
  // let [r, c] = pos
  for (let [dr, dc] of moves) {
    let nr = pos[0] + dr
    let nc = pos[1] + dc
    if (maze[nr][nc] != '#' && !dists.has([nr, nc] + '')) {
      dists.set([nr, nc]+'', ++dist)
      pos = [nr, nc]
      break
    }
  }
}

let count = 0
for (let [pos, dist] of dists) {
  let [r, c] = of_str(pos)
  for (let [dr, dc] of moves) {
    let w = [r + dr, c + dc]
    let p = [r + dr*2, c + dc*2]
    if (dists.has(p+'') && maze[w[0]][w[1]] == '#') {
      let d = dists.get(p+'')
      if (d > dist && d - dist >= 102)
        count++
    }
  }
}

console.log(count)

// 1384 too high // 1384
// 6879 too high

let count2 = 0

dists = Array.from(dists)

for (let i = 0; i < dists.length; i++) {
  let [pos1, dist1] = dists[i]
  let [r, c] = of_str(pos1)
  for (let j = 0; j < dists.length; j++) {
    if (i == j)
      continue

    let [pos2, dist2] = dists[j]

    // console.log(pos1, dist1, pos2, dist2)

    if (dist1 >= dist2)
      continue

    let [rr, cc] = of_str(pos2)
    let ps = Math.abs(rr - r) + Math.abs(cc - c)

    if (ps > 20)
      continue

    if ((dist2 - dist1) >= (100 + ps))
      count2++
  }
}

console.log(count2)
