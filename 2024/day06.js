const fs = require('fs')

let grid = fs.readFileSync(0, {encoding:'utf-8'}).trim().split('\n').map(row => row.split(''))

let start_pos = [0, 0, '^']

start_position_search: {
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[r].length; c++) {
      if (grid[r][c] === '^') {
        start_pos = [r, c, '^']
        grid[r][c] = '.'
        break start_position_search
      }
    }
  }
}

const pos_to_str = (pos) => pos.join(',')

const pos_from_str = (str) => {
  const [r, c, dir] = str.split(',')
  return [parseFloat(r), parseFloat(c), dir]
}

console.log({start_pos})

function next_pos([r, c, dir]) {
  switch (dir) {
  case '^':
    return [r-1, c]
  case 'v':
    return [r+1, c]
  case '<':
    return [r, c-1]
  case '>':
    return [r, c+1]
  }
}

function turn([r, c, dir]) {
  return [r, c, {'^': '>', '>': 'v', 'v': '<', '<': '^'}[dir]]
}

function next([r, c, dir]) {
  const [nr, nc] = next_pos([r, c, dir]);
  if (nr >= 0 && nr < grid.length && nc >= 0 && nc < grid[nr].length) {
    if (grid[nr][nc] === '#') {
      return turn([r, c, dir])
    }
    return [nr, nc, dir]
  }
}

let visits = new Set()
let pos = start_pos
while (pos = next(pos)) {
  visits.add(pos.slice(0, 2).join(','));
}
console.log({visits: visits.size })

function is_loop(pos) {
  const trace = new Set(pos)
  while (pos = next(pos)) {
    if (trace.has(pos_to_str(pos))) {
      return true;
    }
    trace.add(pos_to_str(pos))
  }
  return false;
}

function count_loops() {
  let loops = 0
  for (const pos of visits) {
    const [r, c] = pos.split(',').map(parseFloat)
    grid[r][c] = '#'
    if (is_loop(start_pos)) {
      loops++
    }
    grid[r][c] = '.'
  }
  console.log({loops})
}

count_loops()
