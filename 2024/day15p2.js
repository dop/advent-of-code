const assert = require('assert')
const fs = require('fs')

let [warehouse_small, moves] = fs.readFileSync('day15.txt', {encoding: 'utf-8'})
  .trim().split('\n\n')

warehouse_small = warehouse_small.split('\n').map(line => line.split(''))
moves = moves.split('\n').join('')

let warehouse = []

prepare: {
  for ( let r = 0; r < warehouse_small.length; r++ ) {
    warehouse[r] = []
    for ( let c = 0; c < warehouse_small[r].length; c++ ) {
      if (warehouse_small[r][c] == '.') {
        warehouse[r].push('.', '.')
      } else if (warehouse_small[r][c] == '#') {
        warehouse[r].push('#', '#')
      } else if (warehouse_small[r][c] == '@') {
        robot = [r, c * 2]
        warehouse[r].push('.', '.')
      } else if (warehouse_small[r][c] == 'O') {
        warehouse[r].push('[', ']')
      }
    }
  }
}

let initial = structuredClone(warehouse)

function print(wh = warehouse, rob = robot) {
  for ( let r = 0; r < wh.length; r++ ) {
    for ( let c = 0; c < wh[r].length; c++ ) {
      if (r == rob[0] && c == rob[1])
        process.stdout.write('@')
      else
        process.stdout.write(wh[r][c])
    }
    process.stdout.write('\n')
  }
}

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

print()

const to_str = (x) => String(x)
const of_str = (x) => x.split(',').map(Number)

for ( let i = 0; i < moves.length; i++ ) {
  let dir = moves[i]
  let [r, c] = robot
  let [dr, dc] = delta(dir)
  let obstacle = warehouse[r+dr][c+dc]

  if (obstacle == '#')
    continue

  if (obstacle == '.') {
    robot = [r+dr, c+dc]
    continue
  }

  if (dir == '>' || dir == '<') {
    // Moving left or right requires jumping by 2 until wall "#" or
    // empty space "."
    let nc = c + dc
    while (obstacle == '[' || obstacle == ']') {
      nc += dc * 2
      obstacle = warehouse[r][nc]
      //console.log('forward', {c, nc, obstacle})
    }
    if (obstacle == '.') {
      while (nc != c + dc) {
        warehouse[r][nc] = dir == '>' ? ']' : '['
        warehouse[r][nc - dc] = dir == '>' ? '[' : ']'
        nc -= dc * 2
        obstacle = warehouse[r][nc]
      }
      warehouse[r][nc] = '.'
      robot = [r, c+dc]
    }
  }
  else if (dir == '^' || dir == 'v') {
    // Moving up or down requires checking if box moves one or more
    // boxes recursively up/down. Boxes can be moved only if there
    // is empty space below or above. If there is even a single
    // wall, nothing can be moved.

    let nr = r + dr
    let boxes = []

    if (warehouse[nr][c] == '[') boxes = [new Set([to_str([nr, c])])]
    if (warehouse[nr][c] == ']') boxes = [new Set([to_str([nr, c-1])])]

    if (boxes.length) {
      search: {
        while (true) {
          let row = new Set
          for (let [br, bc] of Array.from(boxes[0]).map(of_str)) {
            if (warehouse[br+dr][bc] == '[') {
              row.add(to_str([br+dr, bc]))
            } else {
              if (warehouse[br+dr][bc] == '#' || warehouse[br+dr][bc+1] == '#') {
                break search
              }
              if (warehouse[br+dr][bc] == ']') {
                row.add(to_str([br+dr, bc-1]))
              }
              if (warehouse[br+dr][bc+1] == '[') {
                row.add(to_str([br+dr, bc+1]))
              }
            }
          }
          if (row.size) {
            boxes.unshift(row)
          } else {
            robot = [nr, c]
            while (boxes.length) {
              let row = boxes.shift()
              for (let [br,bc] of Array.from(row).map(of_str)) {
                warehouse[br+dr][bc] = warehouse[br][bc]
                warehouse[br+dr][bc+1] = warehouse[br][bc+1]
                warehouse[br][bc] = '.'
                warehouse[br][bc+1] = '.'
              }
            }
            break
          }
        }
      }
    } else {
      robot = [nr, c]
    }
  }
}

print()

for ( let r = 0; r < warehouse.length; r++ ) {
  for ( let c = 0; c < warehouse[r].length; c++ ) {
    if (warehouse[r][c] == '#') {
      assert.equal('#', initial[r][c])
    }
  }
}

function box_count(warehouse) {
  let count = 0
  for ( let r = 0; r < warehouse.length; r++ ) {
    for ( let c = 0; c < warehouse[r].length; c++ ) {
      if (warehouse[r][c] == '[') {
        count += 1
      }
    }
  }
  return count
}

assert.equal(box_count(initial), box_count(warehouse))

let gps = 0
for ( let r = 0; r < warehouse.length; r++ ) {
  for ( let c = 0; c < warehouse[r].length; c++ ) {
    if (warehouse[r][c] == '[') {
      gps += r * 100 + c
    }
  }
}
console.log(gps)

// too high 1459558
//          1446175
