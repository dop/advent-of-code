const assert = require('assert')
const fs = require('fs')

const stones = fs.readFileSync(0, {encoding:'utf-8'}).trim()
  .split(' ').map(Number)

console.log({stones})

const cache = {}

function blink(stone, i) {
  if (cache[stone + ',' + i])
    return cache[stone + ',' + i]
  // console.log('blink', stone, i)
  let count = 0
  let result
  if (i === 0)
    result = 1
  else if (stone === 0)
    result = blink(1, i - 1)
  else {
    const str = stone.toString()
    if (str.length % 2 === 0) {
      result = blink(Number(str.substring(0, str.length / 2)), i - 1) +
        blink(Number(str.substring(str.length / 2)), i - 1)
    } else {
      result = blink(stone * 2024, i - 1)
    }
  }
  cache[stone + ',' + i] = result
  return result
}

console.log( stones.reduce((sum, stone) => sum + blink(stone, 75), 0) )
