const assert = require('assert')
const fs = require('fs')

// In summary, there are the following keypads:
// - One directional keypad that you are using.
// - Two directional keypads that robots are using.
// - One numeric keypad (on a door) that a robot is using.

let of_str = (x) => x.split(',').map(Number)
let codes = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n')

// console.log(codes)

let keypad = [ '789', '456', '123', ' 0A' ]
let arrows = [ ' ^A', '<v>' ]

let keypad_coord = {}
let coord_keypad = {}

for (let r = 0; r < keypad.length; r++) {
  for (let c = 0; c < keypad[r].length; c++) {
    keypad_coord[keypad[r][c]] = [r,c]+''
    coord_keypad[[r,c]+''] = keypad[r][c]
  }
}

// console.log(keypad_coord)
// console.log(coord_keypad)

let arrow_coord = {};
let coord_arrow = {};

for (let r = 0; r < arrows.length; r++) {
  for (let c = 0; c < arrows[r].length; c++) {
    arrow_coord[arrows[r][c]] = [r,c]+''
    coord_arrow[[r,c]+''] = arrows[r][c]
  }
}

// console.log(arrow_coord)
// console.log(coord_arrow)

const moves = {
  '^': [-1, 0],
  'v': [1, 0],
  '>': [0, 1],
  '<': [0, -1],
}

const size_cache = new Map

function all_paths(current_key, target_key, coord_of_key, key_of_coord) {
  let paths = []
  let [r, c] = of_str(coord_of_key[current_key])
  let [rr, cc] = of_str(coord_of_key[target_key])
  let Q = [[r, c, ['']]]
  while (Q.length) {
    let [r, c, codes] = Q.shift()
    if (rr - r < 0 && key_of_coord[[r-1,c]+''] != ' ') {
      Q.push([r-1, c, codes.map(code => code + '^')])
    }
    if (rr - r > 0 && key_of_coord[[r+1,c]+''] != ' ') {
      Q.push([r+1, c, codes.map(code => code + 'v')])
    }
    if (cc - c < 0 && key_of_coord[[r,c-1]+''] != ' ') {
      Q.push([r, c-1, codes.map(code => code + '<')])
    }
    if (cc - c > 0 && key_of_coord[[r,c+1]+''] != ' ') {
      Q.push([r, c+1, codes.map(code => code + '>')])
    }
    if (rr - r == 0 && cc - c == 0) {
      for (let j = 0; j < codes.length; j++)
        paths.push(codes[j] + 'A')
    }
  }
  return paths
}

function encode_keys(code) {
  let current_key = 'A'
  let new_paths = ['']
  for (let target_key of code) {
    let subpaths = all_paths(current_key, target_key, keypad_coord, coord_keypad)
    new_paths = new_paths.flatMap(path => subpaths.map(subpath => path + subpath))
    current_key = target_key
  }
  return new_paths
}

function min(arr) {
  let best = Infinity
  for (let i = 0; i < arr.length; i++)
    best = Math.min(arr[i], best)
  return best
}

function encode_arrows(i, code) {
  if (size_cache.has(i + code)) {
    return size_cache.get(i + code)
  }
  let current_key = 'A'
  let size = 0
  let new_codes = ['']
  for (let target_key of code) {
    let subpaths = all_paths(current_key, target_key, arrow_coord, coord_arrow)
    if (i > 0) {
      size += min(subpaths.map(subpath => encode_arrows(i-1, subpath)))
    }
    else {
      size += min(subpaths.map(subpath => subpath.length))
    }
    current_key = target_key
  }
  size_cache.set(i + code, size)
  return size
}

let answer = 0
for (let code of codes) {
  let options = encode_keys(code)
  let sizes = options.map(option => encode_arrows(1, option))
  answer += parseInt(code) * min(sizes)
}
console.log(answer)

let answer2 = 0
for (let code of codes) {
  let options = encode_keys(code)
  let sizes = options.map(option => encode_arrows(24, option))
  answer2 += parseInt(code) * min(sizes)
}
console.log(answer2)
