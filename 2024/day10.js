const assert = require('assert')
const fs = require('fs')

const landscape = fs.readFileSync(0, {encoding:'utf-8'}).trim()
  .split('\n')
  .map(line => line.split('').map(Number))

const starts = []

for (let r = 0; r < landscape.length; r++) {
  for (let c = 0; c < landscape[r].length; c++) {
    if (landscape[r][c] === 0)
      starts.push([r, c])
  }
}

// console.log(starts)

let score = 0

function in_bounds(r, c) {
  return r >= 0 && r < landscape.length && c >= 0 && c < landscape[r].length;
}

const moves = [[1, 0], [0, 1], [-1, 0], [0, -1]]

function count_score(start) {
  const peaks = new Set
  const visits = new Set
  const queue = [start]

  while (queue.length) {
    const [r, c] = queue.shift()
    // console.log(r, c, landscape[r][c])
    visits.add(r + ',' + c)
    for (const [dr, dc] of moves) {
      const nr = r + dr
      const nc = c + dc
      // console.log('next?', nr, nc)
      if (in_bounds(nr, nc) && landscape[r][c] === landscape[nr][nc] - 1 && !visits.has(nr + ',' + nc)) {
        if (landscape[nr][nc] === 9)
          peaks.add(nr + ',' + nc)
        else
          queue.push([nr, nc])
      }
    }
  }

  return peaks.size
}

console.log( starts.reduce((sum, p) => sum + count_score(p), 0) )

function count_rating(start) {
  const queue = [[...start, new Set(start.join(','))]]
  let rating = 0

  while (queue.length) {
    const [r, c, visits] = queue.shift()
    for (const [dr, dc] of moves) {
      const nr = r + dr
      const nc = c + dc
      if (in_bounds(nr, nc) && landscape[r][c] === landscape[nr][nc] - 1 && !visits.has(nr + ',' + nc)) {
        if (landscape[nr][nc] === 9) {
          rating++
        } else {
          const nvisits = new Set(visits)
          nvisits.add(nr + ',' + nc)
          queue.push([nr, nc, nvisits])
        }
      }
    }
  }

  return rating
}

console.log( starts.reduce((sum, p) => sum + count_rating(p), 0) )
