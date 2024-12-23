const assert = require('assert')
const fs = require('fs')

let [patterns, towels] = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')

patterns = patterns.split(', ')
towels = towels.split('\n')

let rx = new RegExp('^(' + patterns.join('|') + ')+$')

console.log( towels.reduce((sum, towel) => sum + rx.test(towel), 0) )

let cache = new Map

function count_possible(towel) {
  if (cache.has(towel))
    return cache.get(towel)

  let count = 0
  for (let pattern of patterns) {
    if (pattern == towel)
      count += 1
    else if (towel.startsWith(pattern))
      count += count_possible(towel.substring(pattern.length))
  }

  cache.set(towel, count)
  return count
}

console.log( towels.reduce((sum, towel) => sum + count_possible(towel), 0) )
