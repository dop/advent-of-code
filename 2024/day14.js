const assert = require('assert')
const fs = require('fs')

let robots = fs.readFileSync(0, {encoding: 'utf-8'}).trim()
  .split('\n')
  .map(robot => robot.match(/-?\d+/g).map(Number))

const w = 101
const h = 103

// console.log(robots)

function positions(iteration) {
  return robots.map(([px, py, vx, vy]) => {
    let c = (px + vx * iteration) % w
    let r = (py + vy * iteration) % h

    if (c < 0)
      c = w + c
    if (r < 0)
      r = h + r

    return [r, c]
  })
}

let q = [0, 0, 0, 0]

let mw = Math.floor(w / 2)
let mh = Math.floor(h / 2)

for ([r, c] of positions(100)) {
  if (r < mh && c < mw)
    q[0] += 1
  else if (r > mh && c < mw)
    q[1] += 1
  else if (r < mh && c > mw)
    q[2] += 1
  else if (r > mh && c > mw)
    q[3] += 1
}

console.log(q, q.reduce((acc, x) => acc * x, 1))

// 215476074

// Idea for part 2 is that if most of the robots are close to center
// to form a picture of a xmas tree, then average of distances will be
// smallest.
let best_avg = Infinity
let best_i = 0
for ( let i = 0; i < 100000; i++ ) {
  const dists = positions(i).map(([r, c]) => Math.sqrt(Math.pow((h / 2) - r, 2) + Math.pow((w / 2) - c, 2)) )
  const avg = dists.reduce((sum, d) => sum + d, 0) / dists.length
  if (avg < best_avg) {
    best_avg = avg
    best_i = i
  }
}

function print_iteration(iteration) {
  const s = new Set(positions(iteration).map(p => p.join(',')))
  for ( let r = 0; r < h; r++ ) {
    for ( let c = 0; c < w; c++ ) {
      if (s.has(r + ',' + c))
        process.stdout.write('X')
      else
        process.stdout.write('.')
    }
    process.stdout.write('\n')
  }
}

print_iteration(best_i)
