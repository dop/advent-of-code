str_reverse = (str) => str.split('').reverse().join('')

split_trim = (sep, str) => str.split(sep).map(s => s.trim())

is_symbol = (x) => {
  return x && x !== '.' && (x < '0' || x > '9');
}

function solve(lines) {

  let result = []

  for ( let i = 0; i < lines.length; i++ ) {

    let number = ''
    let adjacent = false

    for ( let j = 0; j < lines[i].length; j++ ) {

      if ( lines[i][j] >= '0' && lines[i][j] <= '9' ) {

        if (!number)
          for ( let k = i - 1; k < i + 2; k++ )
            if (is_symbol(lines[k]?.[j - 1]))
              adjacent = true

        for ( let k = i - 1; k < i + 2; k++ )
          if (is_symbol(lines[k]?.[j]))
            adjacent = true

        number += lines[i][j]

      } else if (number) {

        for ( let k = i - 1; k < i + 2; k++ ) {
          if (is_symbol(lines[k]?.[j]))
            adjacent = true
        }

        // console.log(number, adjacent);
        if (adjacent) {
          result.push(number)
        }

        // process.exit(1)

        number = ''
        adjacent = false
      }

    }

    if (number) {
      if (adjacent) {
        result.push(number)
      }

      number = ''
    }

  }

  return result.map(Number).reduce((acc, n) => acc + n, 0)
}

console.debug(solve(require('fs').readFileSync(0).toString().trim().split('\n')))
