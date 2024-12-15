const assert = require('assert')
const fs = require('fs')

let [warehouse, moves] = fs.readFileSync(0, {encoding: 'utf-8'})
  .trim().split('\n\n')

warehouse = warehouse.split('\n').map(line => line.split(''))
moves = moves.split('\n').join('')

const in_bounds = (r, c) => 0 <= r && r < warehouse.length && 0 <= c && c < warehouse[r].length

find_robot: {
  for ( let r = 0; r < warehouse.length; r++ ) {
    for ( let c = 0; c < warehouse[r].length; c++ ) {
      if (warehouse[r][c] == '@') {
        robot = [r, c]
        warehouse[r][c] = '.'
        break find_robot
      }
    }
  }
}

function print() {
  for ( let r = 0; r < warehouse.length; r++ ) {
    for ( let c = 0; c < warehouse[r].length; c++ ) {
      if (r == robot[0] && c == robot[1])
        process.stdout.write('@')
      else
        process.stdout.write(warehouse[r][c])
    }
    process.stdout.write('\n')
  }
}

print()

for ( let dir of moves ) {
  let dr, dc
  let [r, c] = robot

  switch (dir) {
  case '^':
    dr = -1
    dc = 0
    break
  case 'v':
    dr = 1
    dc = 0
    break
  case '>':
    dr = 0
    dc = 1
    break
  case '<':
    dr = 0
    dc = -1
    break
  }

  if (warehouse[r + dr][c + dc] == '.') {
    robot = [r + dr, c + dc]
  }
  else if (warehouse[r + dr][c + dc] == 'O') {
    let nr = r + dr
    let nc = c + dc
    while (warehouse[nr][nc] == 'O') {
      nr += dr
      nc += dc
    }
    if (warehouse[nr][nc] == '.') {
      warehouse[r + dr][c + dc] = '.'
      warehouse[nr][nc] = 'O'
      robot = [r + dr, c + dc]
    }
  }
}

print()

let gps = 0
for ( let r = 0; r < warehouse.length; r++ ) {
  for ( let c = 0; c < warehouse[r].length; c++ ) {
    if (warehouse[r][c] == 'O') {
      gps += r * 100 + c
    }
  }
}
console.log(gps)
