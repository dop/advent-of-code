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
  return r >= 0 && r < landscape.length && c >= 0 && c < landscape[r].length
}

const moves = [[1, 0], [0, 1], [-1, 0], [0, -1]]

// Counting score and rating with the same BFS works because all paths
// to the same peak have same length.
function count_score_and_rating(start) {
  const peaks = new Set
  const visits = new Set
  const queue = [start]
  let rating = 0

  while (queue.length) {
    const [r, c] = queue.shift()
    visits.add(r + ',' + c)
    for (const [dr, dc] of moves) {
      const nr = r + dr
      const nc = c + dc
      if (in_bounds(nr, nc) && landscape[r][c] === landscape[nr][nc] - 1 && !visits.has(nr + ',' + nc)) {
        if (landscape[nr][nc] === 9) {
          rating++
          peaks.add(nr + ',' + nc)
        } else {
          queue.push([nr, nc])
        }
      }
    }
  }

  return [peaks.size, rating]
}

console.log(
  starts.map(count_score_and_rating)
    .reduce(([sum1, sum2], [score, rating]) => [sum1 + score, sum2 + rating], [0, 0])
)

// Following code is not longer necessary ...

/*
console.log( starts.reduce((sum, p) => sum + count_score(p)[0], 0) )
console.log( starts.reduce((sum, p) => sum + count_score(p)[1], 0) )

function count_rating(start) {
  const queue = [[...start, new Set([start.join(',')])]]
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
*/
