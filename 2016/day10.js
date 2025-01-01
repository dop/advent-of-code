const assert = require('assert')
const fs = require('fs')

let instructions = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')[0].split('\n')

let val = str => Number(str.split(' ')[1])

let bots = {}
let dirs = {}
for (let instruction of instructions) {
  let nums = instruction.match(/(bot|output|value) \d+/g)
  if (nums.length == 2) {
    bots[nums[1]] ??= []
    bots[nums[1]].push(val(nums[0]))
  } else if (nums.length == 3) {
    dirs[nums[0]] = nums.slice(1)
  } else {
    assert.fail(nums.length)
  }
}

let Q = ['bot 17']
while (Q.length) {
  let bot = Q.shift()
  let [a, b] = bots[bot].sort((a, b) => a - b)
  let [dir1, dir2] = dirs[bot]
  bots[dir1] ??= []
  bots[dir2] ??= []
  bots[dir1].push(a)
  bots[dir2].push(b)
  if (bots[dir1].length == 2)
    Q.push(dir1)
  if (bots[dir2].length == 2)
    Q.push(dir2)
}

console.log(
  Object.entries(bots)
    .find(([bot, values]) => values.includes(61) && values.includes(17))[0]
    .split(' ')[1]
)

console.log(
  bots['output 0'][0] * bots['output 1'][0] * bots['output 2'][0],
)
