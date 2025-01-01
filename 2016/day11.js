const assert = require('assert')
const fs = require('fs')

let floors = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')[0].split('\n')
let puzzle = {}

let i = 0
for (let i = 0; i < floors.length; i++) {
  let matches = floors[i].match(/[a-z-]+ (generator|microchip)/g)
  if (matches) {
    for (let item of matches) {
      let [thing, type] = item.split(' ')
      puzzle[thing.substr(0,2)+type[0]] = i
    }
  }
}

let total = 0
let layout = []
let things = Object.keys(puzzle)

for (let [thing, floor] of Object.entries(puzzle)) {
  layout[floor] ??= new Set
  layout[floor].add(thing)
  total++
}

layout[3] = new Set

let is_valid_floor = items => {
  let gs = new Set
  let ms = []
  for (let item of items) {
    if (item[2] == 'g')
      gs.add(item.substr(0,2))
    else
      ms.push(item.substr(0,2))
  }
  return ms.every(m => gs.has(m)) || gs.size == 0
}

assert(is_valid_floor(['stg', 'stm']))
assert(is_valid_floor(['stg', 'stm', 'xxg', 'xxm']))
assert(is_valid_floor(['stg', 'stm', 'xxg', 'xxm', 'yyg']))
assert(!is_valid_floor(['stg', 'stm', 'plm']))
assert(is_valid_floor(['stm', 'plm']))
assert(is_valid_floor(['stg', 'plg']))

let layout_hash = (floor_i, layout) => {
  let hash = [floor_i]
  for (let floor of layout)
    hash.push(Array.from(floor).map(item => item[2]).sort().join(''))
  return hash.join(',')
}

let layout_str = layout => {
  let str = []
  for (let floor of Object.values(layout)) {
    str.push(Array.from(floor).join(''))
  }
  return str.join('|')
}

function next_moves(cf, nf, layout) {
  let moves = []
  let items = Array.from(layout[cf])
  if (-1 < nf && nf < 4) {
    for (let j = 0; j < items.length; j++) {
      for (let k = j; k < items.length; k++) {
        let item1 = items[j]
        let item2 = items[k]
        let f1 = new Set(items)
        let f2 = new Set(layout[nf])
        f1.delete(item1)
        f1.delete(item2)
        f2.add(item1)
        f2.add(item2)
        let new_layout = layout.slice(0)
        new_layout[cf] = f1
        new_layout[nf] = f2
        if (is_valid_floor(f1) && is_valid_floor(f2)) {
          moves.push(new_layout)
        }
      }
    }
  }
  return moves
}

let V = new Set

function bfs(initial_layout) {
  let Q = [[0, 0, initial_layout]]
  let best = Infinity

  while (Q.length) {
    let [i, cf, layout] = Q.shift()
    let hash = layout_hash(cf, layout)

    if (V.has(hash))
      continue
    V.add(hash)

    if (cf == 3 && layout[3].size == total) {
      if (best > i) {
        best = i
      }
      continue
    }

    let nf = cf+1
    if (nf <= 3) {
      for (let new_layout of next_moves(cf, nf, layout)) {
        Q.push([i+1, nf, new_layout])
      }
    }
    let pf = cf-1
    if (pf >= 0) {
      for (let new_layout of next_moves(cf, pf, layout)) {
        Q.push([i+1, pf, new_layout])
      }
    }
  }

  return best
}

console.log( bfs(layout) )

for (let item of ['elg', 'elm', 'dig', 'dim'])
  layout[0].add(item)
total += 4

console.log( bfs(layout) )
