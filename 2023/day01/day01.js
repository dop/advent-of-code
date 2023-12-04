fs = require('fs');

convert = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,
}

const str_reverse = (str) => str.split('').reverse().join('')

function solve(lines) {

  let result = []

  for (let i = 0; i < lines.length; i++) {

    let first = lines[i].match(/([0-9]|one|two|three|four|five|six|seven|eight|nine)/g)
      .map((digit) => convert[digit] ?? digit - '0')[0]

    let last = lines[i].split('').reverse().join('')
        .match(/([0-9]|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno)/g)
        .map((digit) => convert[str_reverse(digit)] ?? digit - '0')[0]

    result.push( first * 10 + last )

  }

  return result.reduce((acc, n) => acc + n, 0)
}

console.log(solve(fs.readFileSync(0).toString().trim().split('\n')))
