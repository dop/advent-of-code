const fs = require('fs')

const heystack = fs.readFileSync(0, {encoding:'utf-8'}).trim().split('\n')

const p1 = ['XMAS']
const p2 = ['SAMX']
const p3 = 'XMAS'.split('')
const p4 = 'SAMX'.split('')
const p5 = ['X...', '.M..', '..A.', '...S']
const p6 = ['S...', '.A..', '..M.', '...X']
const p7 = ['...S', '..A.', '.M..', 'X...']
const p8 = ['...X', '..M.', '.A..', 'S...']

const ps = [p1,p2,p3,p4,p5,p6,p7,p8]

console.log(heystack.join('\n'))

function matchpos(p, r, c) {
  try {
    for (let pr = 0; pr < p.length; pr++) {
      for (let pc = 0; pc < p[pr].length; pc++) {
        // console.log(`${r+pr}:${c+pc}`, pr+':'+pc, heystack[r+pr][c+pc], '=', p[pr][pc])
        if (p[pr][pc] !== '.' && heystack[r+pr][c+pc] !== p[pr][pc]) {
          return false
        }
      }
    }
    return true;
  } catch (e) {
    return false
  }
}

function count_matches(p) {
  let count = 0
  for (let r = 0; r < heystack.length; r++) {
    for (let c = 0; c < heystack[r].length; c++) {
      if (matchpos(p, r, c))
        count++
    }
  }
  return count
}

const px1 = ['M.S', '.A.', 'M.S']
const px2 = ['S.S', '.A.', 'M.M']
const px3 = ['S.M', '.A.', 'S.M']
const px4 = ['M.M', '.A.', 'S.S']
const px = [px1, px2, px3, px4]

console.log( 'count 1', ps.reduce((acc, p) => acc + count_matches(p), 0) )
console.log( 'count 2', px.reduce((acc, p) => acc + count_matches(p), 0) )
