const fs = require('fs')

const lines = fs.readFileSync(0, {encoding:'utf-8'}).trim().split('\n')

function* diffs(nums) {
  for (let i = 0; i < nums.length - 1; i++) {
    yield nums[i] - nums[i+1]
  }
}

function parseFloats(line) {
  return line.split(' ').map(parseFloat);
}

function issues(series) {
  let pos = 0
  let neg = 0
  let bad = 0

  for (let i = 0; i < series.length - 1; i++) {
    const diff = series[i] - series[i+1]
    if (diff < 0)
      neg++
    if (diff > 0)
      pos++
    if (diff > 3 || diff < -3 || diff === 0)
      bad++
  }

  return bad + Math.min(pos, neg)
}

function without(arr, i) {
  return [...arr.slice(0, i), ...arr.slice(i+1)]
}

async function main() {
  let rez = 0
  for (const line of lines) {
    const numbers = parseFloats(line)
    // console.log(numbers)
    if (issues(numbers) === 0) {
      rez++
      continue
    }
    for (let i = 0; i < numbers.length; i++) {
      if (issues(without(numbers, i)) === 0) {
        rez++
        break
      }
    }
  }
  console.log(rez)
}

main()

// 725 too high
// 716 too low
// 722 too high

// 717 ok
