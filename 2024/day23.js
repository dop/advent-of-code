const assert = require('assert')
const fs = require('fs')

let connections = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n')

let computers = new Set
let network = new Set

for (let connection of connections) {
  let [a, b] = connection.split('-')
  computers.add(a)
  computers.add(b)
  network.add(a + b)
  network.add(b + a)
}

computers = Array.from(computers)

let count = 0
for (let i = 0; i < computers.length - 2; i++) {
  for (let j = i+1; j < computers.length - 1; j++) {
    for (let k = j+1; k < computers.length; k++) {
      let a = computers[i], b = computers[j], c = computers[k]
      if ((a[0] == 't' || b[0] == 't' || c[0] == 't') && network.has(a+b) && network.has(b+c) && network.has(a+c))
        count++
    }
  }
}

console.log(count)

let lans = computers.map((computer) => [computer])

for (let i = 0; i < computers.length; i++) {
  for (let j = 0; j < lans.length; j++) {
    if (lans[j].every(computer => computer == computers[i] || network.has(computer + computers[i]))) {
      if (lans[j].indexOf(computers[i]) == -1)
        lans[j].push(computers[i])
    }
  }
}

console.log(
  lans
    .sort((a, b) => b.length - a.length)[0]
    .sort((a, b) => a == b ? 0 : a < b ? -1 : 1)+''
)
