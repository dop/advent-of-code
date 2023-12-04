str_reverse = (str) => str.split('').reverse().join('')

split_trim = (sep, str) => str.split(sep).map(s => s.trim())

max = {
  red: 12,
  green: 13,
  blue: 14,
}

function fits(sets) {
  for ( let set of split_trim(';', sets) ) {
    // console.log(set);
    for ( let [count, color] of split_trim(',', set).map(cube => cube.split(' ')) ) {
      // console.log(count, color, max[color])
      if (count > max[color]) {
        return false;
      }
    }
  }
  return true;
}

function solve(lines) {

  let result = 0

  for (let i = 0; i < lines.length; i++) {

    const [game, sets] = lines[i].split(':')

    if (fits(sets)) {
      result += Number(game.match(/[0-9]+/)[0])
    }

  }

  return result
}

console.debug(solve(require('fs').readFileSync(0).toString().trim().split('\n')))
