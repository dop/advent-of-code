const assert = require('assert')
const fs = require('fs')

const arcades = fs.readFileSync(0, {encoding: 'utf-8'}).trim()
  .split('\n\n')
  .map(arcade => arcade.match(/\d+/g).map(Number))

let result1 = 0

for (let [ax, ay, bx, by, px, py] of arcades) {
  let tokens = Infinity
  for (let i = 0; i <= 100; i++) {
    for (let j = 0; j <= 100; j++) {
      if (px == ax * i + bx * j && py == ay * i + by * j) {
        tokens = Math.min(tokens, i * 3 + j)
      }
    }
  }
  if (tokens != Infinity)
    result1 += tokens
}

console.log(result1)

// assert.equal( result1, 37686 )

let result2 = 0

let zip = (f, a, b) => a.map((x, i) => f(x, b[i]))
let minus = (a, b) => zip((a, b) => a - b, a, b)
let div = (a, x) => a.map(a => a / x)


for (let [ax, ay, bx, by, px, py] of arcades) {
  px += 10000000000000
  py += 10000000000000

  // const a = minus(
  //   [ay / ay, by / ay, py / ay],
  //   [ax / ax, bx / ax, px / ax],
  // );
  // const b = minus(
  //   [ay / by, by / by, py / by],
  //   [ax / bx, bx / bx, px / bx],
  // );
  // const i = Math.round(div(b, b[0])[2])
  // const j = Math.round(div(a, a[1])[2])

  // Simpler solution because numbers will still fit in js number.
  //
  // L1 : ax * i + bx * j = px |* ay
  // L2 : ay * i + by * j = py |* ax
  //
  // then L1 - L2 to get rid of i. To calculate j
  //
  // (bx - by) * j = (px * ay) - (py * ax)
  // j = (px * ay) - (py * ax) / (bx - by)
  //
  // Same can be applied for i.
  const j = Math.round( (px * ay - py * ax) / (bx * ay - by * ax) )
  const i = Math.round( (px * by - py * bx) / (ax * by - ay * bx) )

  if (ax * i + bx * j === px && ay * i + by * j === py) {
    result2 += 3 * i + j
  }
}

console.log(result2)

// assert.equal( result2, 77204516023437 )
