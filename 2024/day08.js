const fs = require('fs')

let lines = fs.readFileSync(0, {encoding:'utf-8'})
  .trim().split('\n')

// lines.forEach((line) => process.stdout.write(line + '\n'))

const m = new Map()

for (let r = 0; r < lines.length; r++) {
  for (let c = 0; c < lines[r].length; c++) {
    const l = lines[r][c]
    if (l != '.') {
      if (!m.get(l))
        m.set(l, new Set())
      const p = r + ',' + c
      m.get(l).add(p)
    }
  }
}

const a = new Set()
const a2 = new Set()

const in_bounds = (r, c) => 0 <= r && r < lines.length && 0 <= c && c < lines[0].length

for (const [l, s] of m) {
  // console.log(l)
  const ps = Array.from(s)
  for (let i = 0; i < ps.length - 1; i++) {
    for (let j = i + 1; j < ps.length; j++) {
      const pi = ps[i].split(',').map(Number)
      const pj = ps[j].split(',').map(Number)

      const dr = pi[0] - pj[0]
      const dc = pi[1] - pj[1]

      let r1 = pi[0] + dr
      let c1 = pi[1] + dc
      if (in_bounds(r1, c1))
        a.add(r1 + ',' + c1)

      a2.add(pi.join(','))
      while (in_bounds(r1, c1)) {
        a2.add(r1 + ',' + c1)
        r1 += dr
        c1 += dc
      }

      let r2 = pj[0] - dr
      let c2 = pj[1] - dc
      if (in_bounds(r2, c2))
        a.add(r2 + ',' + c2)

      a2.add(pj.join(','))
      while (in_bounds(r2, c2)) {
        a2.add(r2 + ',' + c2)
        r2 -= dr
        c2 -= dc
      }
    }
  }
}

console.log(a.size)
console.log(a2.size)
