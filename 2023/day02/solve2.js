str_reverse = (str) => str.split('').reverse().join('')

split_trim = (sep, str) => str.split(sep).map(s => s.trim())

function fit(sets, max) {
  for ( let set of split_trim(';', sets) ) {
    for ( let [count, color] of split_trim(',', set).map(cube => cube.split(' ')) ) {
      if (Number(count) > max[color]) {
        max[color] = Number(count);
      }
    }
  }
}

function solve(lines) {

  let result = 0

  for (let i = 0; i < lines.length; i++) {

    const [game, sets] = lines[i].split(':')
    id = Number(game.match(/[0-9]+/)[0])
    max = { red: 0, green: 0, blue: 0 }

    fit(sets, max)

    // console.log(id, max, max.red * max.green * max.blue)

    result += max.red * max.green * max.blue

  }

  return result
}

console.debug(solve(require('fs').readFileSync(0).toString().trim().split('\n')))
