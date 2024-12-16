const assert = require('assert')
const fs = require('fs')
const {Heap} = require('./heap')

let grid = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n')

// r, c, direction, cost
let start = [0, 0, '>', 0, 0]
let goal = [0, 0]
let visited = new Set

const to_str = (x) => x.join(',')
const of_str = (x) => {
  const [r, c, dir] = x.split(',')
  return [Number(r), Number(c), dir]
}

for (let r = 0; r < grid.length; r++) {
  for (let c = 0; c < grid[r].length; c++) {
    if (grid[r][c] == 'S')
      start = [r, c, '>', 0]
    else if (grid[r][c] == 'E')
      goal= [r, c]
  }
}

function print() {
  for (let r = 0; r < grid.length; r++) {
    for (let c =0; c < grid[r].length; c++) {
      if (r == start[0] && c == start[1])
        process.stdout.write('S')
      else if (r == goal[0] && c == goal[1])
        process.stdout.write('E')
      else
        process.stdout.write(grid[r][c])
    }
    process.stdout.write('\n')
  }
}

print()

function delta(dir) {
  switch (dir) {
  case '^':
    return [-1, 0]
  case 'v':
    return [1, 0]
  case '>':
    return [0, 1]
  case '<':
    return [0, -1]
  }
}

function rotate(dir) {
  switch (dir) {
  case '^':
  case 'v':
    return ['<', '>']
  case '>':
  case '<':
    return ['^', 'v']
  }
}

function find() {
  let tiles = new Set
  let best = Infinity
  let visited = new Map
  let Q = new Heap((a, b) => a[3] < b[3])
  Q.insert([...start, new Set([start.slice(0, 2).join(',')])])
  while (Q.peak()) {
    let [r, c, dir, cost, path] = Q.remove()
    let k = [r, c, dir].join(',')
    if (visited.has(k) && visited.get(k) < cost)
      continue
    visited.set([r, c, dir].join(','), cost)

    if (r == goal[0] && c == goal[1]) {
      if (best > cost) {
        best = cost
        tiles = path
      } else if (best == cost) {
        tiles = tiles.union(path)
      }
    } else {
      if (grid[r][c] != '#') {
        let [dr, dc] = delta(dir)
        let tile = [r + dr, c + dc].join(',')
        Q.insert([r + dr, c + dc, dir, cost + 1, path.union(new Set([tile]))])
        for (let ndir of rotate(dir)) {
          Q.insert([r, c, ndir, cost + 1000, path])
        }
      }
    }
  }

  return [best, tiles]
}

let [answer, tiles] = find()

console.log( answer )
console.log( tiles.size )

