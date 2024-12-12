const fs = require('fs')
const assert = require('assert')

const garden = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n')

// console.log(garden)

let taken = new Set
let regions = []

let available = new Set
let cell_to_str = (rc) => rc.join(',')
let str_to_cell = (str) => str.split(',').map(Number)

const in_bounds = (r, c) => 0 <= r && r < garden.length && 0 <= c && c < garden[0].length

for (let r = 0; r < garden.length; r++) {
  for (let c = 0; c < garden[r].length; c++) {
    available.add(cell_to_str([r, c]))
  }
}

// console.log(available)

const moves = [[1, 0], [0, 1], [-1, 0], [0, -1]]

for (let cell of available) {
  let queue = [str_to_cell(cell)]
  let region = new Set([cell])

  while (queue.length) {
    const [r, c] = queue.shift()
    for (let move of moves) {
      const nr = r + move[0]
      const nc = c + move[1]
      const ncell = [nr, nc]
      // console.log([r, c], cell_to_str(ncell))
      if (available.has(cell_to_str(ncell)) && in_bounds(nr, nc) && garden[r][c] === garden[nr][nc]) {
        available.delete(cell_to_str(ncell))
        region.add(cell_to_str(ncell))
        queue.push(ncell)
      }
    }
  }

  regions.push(region)
}

// console.log(regions)

const h = garden.length
const w = garden[0].length

function perimeter(region) {
  let p = 0
  for (let r = -1; r < h + 1; r++) {
    for (let c = -1; c < w + 1; c++) {
      if (region.has(cell_to_str([r, c])) && !region.has(cell_to_str([r, c+1])))
        p += 1
      if (region.has(cell_to_str([r, c])) && !region.has(cell_to_str([r, c-1])))
        p += 1
      if (region.has(cell_to_str([r, c])) && !region.has(cell_to_str([r-1, c])))
        p +=1
      if (region.has(cell_to_str([r, c])) && !region.has(cell_to_str([r+1, c])))
        p +=1
    }
  }
  return p
}

function sides(region) {
  let corners = 0

  for (let r = -1; r < h + 1; r++) {
    for (let c = -1; c < w + 1; c++) {
      let x = region.has(cell_to_str([r - 1, c - 1]))
      let y = region.has(cell_to_str([r - 1, c]))
      let z = region.has(cell_to_str([r, c - 1]))
      let w = region.has(cell_to_str([r, c]))

      switch (x + y + z + w) {
      case 3:
      case 1:
        corners += 1
        break
      case 2:
        if ((x && w) || (y && z))
          corners += 2
        break
      }
    }
  }

  return corners
}

console.log(
  regions.reduce(([sum1, sum2], r) => [sum1 + r.size * perimeter(r), sum2 + r.size * sides(r)], [0, 0])
)
