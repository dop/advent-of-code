const fs = require('fs')

let [order, manuals] = fs.readFileSync(0, {encoding:'utf-8'}).trim().split('\n\n')

order = new Set(order.split('\n'))
manuals = manuals.split('\n').map(line => line.split(','))

console.log(order)
console.log(manuals)

let good = []
let bad = []

function is_ordered(pages) {
  for (let i = 0; i < pages.length - 1; i++){
    for (let j = i + 1; j < pages.length; j++) {
      if (!order.has(pages[i] + '|' + pages[j])) {
        return false;
      }
    }
  }
  return true;
}

for (const pages of manuals) {
  if (is_ordered(pages)) {
    good.push(pages)
  } else {
    bad.push(pages)
  }
}

console.log(good.reduce((sum, pages) => sum + Number(pages[Math.floor(pages.length / 2)]), 0))

console.log(
  bad
    .map(pages => pages.toSorted((a, b) => {
      if (order.has(a + '|' + b)) {
        return -1
      } else if (order.has(b + '|' + a)) {
        return 1
      } else {
        return 0
      }
    }))
    .reduce((sum, pages) => sum + Number(pages[Math.floor(pages.length / 2)]), 0)
)
