const assert = require('assert')
const fs = require('fs')

let input = fs.readFileSync(0, {encoding: 'utf-8'})
  .trim()
  .split('\n\n')[0]
  .split('\n')

const of_str = (rc) => rc.split(',').map(Number)

let W = 50
let H = 6
let screen = new Set

for (let line of input) {
  if (line.startsWith('rect')) {
    let [w, h] = line.match(/\d+/g).map(Number)
    for (let r = 0; r < h; r++) {
      for (let c = 0; c < w; c++) {
        screen.add([r,c]+'')
      }
    }
  }
  else if (line.startsWith('rotate row')) {
    let [r, amount] = line.match(/\d+/g).map(Number)
    screen = Array.from(screen).map(of_str)
    for (let i = 0; i < screen.length; i++) {
      if (screen[i][0] == r) {
        screen[i][1] = (screen[i][1] + amount) % W
      }
    }
    screen = new Set(screen.map(rc => rc+''))
  }
  else if (line.startsWith('rotate column')) {
    let [c, amount] = line.match(/\d+/g).map(Number)
    screen = Array.from(screen).map(of_str)
    for (let i = 0; i < screen.length; i++) {
      if (screen[i][1] == c) {
        screen[i][0] = (screen[i][0] + amount) % H
      }
    }
    screen = new Set(screen.map(rc => rc+''))
  }
}
console.log(screen.size)

for (let r = 0; r < H; r++) {
  for (let c = 0; c < W; c++) {
    if (screen.has([r,c]+''))
      process.stdout.write('#')
    else
      process.stdout.write(' ')
  }
  process.stdout.write('\n')
}

// EFEYKFRFIJ
