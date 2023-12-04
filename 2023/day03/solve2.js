str_reverse = (str) => str.split('').reverse().join('')

split_trim = (sep, str) => str.split(sep).map(s => s.trim())

is_symbol = (x) => x && x !== '.' && (x < '0' || x > '9')

is_gear = (x) => x === '*';

function solve(lines) {

  let result = new Map()

  for ( let i = 0; i < lines.length; i++ ) {

    let number = ''
    let gears = new Set()

    for ( let j = 0; j < lines[i].length; j++ ) {

      if ( lines[i][j] >= '0' && lines[i][j] <= '9' ) {

        if (!number)
          for ( let k = i - 1; k < i + 2; k++ )
            if (is_gear(lines[k]?.[j - 1])) {
              gears.add(k + ',' + (j - 1))
            }

        for ( let k = i - 1; k < i + 2; k++ )
          if (is_gear(lines[k]?.[j])) {
            gears.add(k + ',' + j)
          }

        number += lines[i][j]

      } else if (number) {

        for ( let k = i - 1; k < i + 2; k++ ) {
          if (is_gear(lines[k]?.[j]))
            gears.add(k + ',' + j)
        }

        // console.log(number, adjacent);
        for ( let gear of gears ) {
          result.set(gear, [ number, ...(result.get(gear) ?? []) ])
        }

        number = ''
        gears = new Set()
      }

    }

    if (number) {
      for ( let gear of gears ) {
        result.set(gear, [ number, ...(result.get(gear) ?? []) ])
      }

      number = ''
    }

  }

  return Array.from(result.values())
    .filter((numbers) => numbers.length === 2)
    .reduce((acc, [a, b]) => acc + a * b, 0)
}

console.debug(solve(require('fs').readFileSync(0).toString().trim().split('\n')))
