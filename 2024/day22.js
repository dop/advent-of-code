const assert = require('assert')
const fs = require('fs')

let secrets = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n').map(BigInt)

// console.log(input)

function mix(secret, value) {
  return BigInt(secret) ^ BigInt(value)
}

function prune(secret) {
  return secret % BigInt(16777216)
}

function next_secret(secret) {
  secret = prune(mix(secret, secret * BigInt(64)))
  secret = prune(mix(secret, secret / BigInt(32)))
  secret = prune(mix(secret, secret * BigInt(2048)))
  return secret
}

let secrets1 = secrets.slice(0)
for (let i = 0; i < 2000; i++) {
  for (let j = 0; j < secrets.length; j++) {
    secrets1[j] = next_secret(secrets1[j])
  }
}
console.log(secrets1.reduce((sum, n) => sum + n, BigInt(0)))

function price(secret) {
  return Number(secret % BigInt(10))
}

let hash = (w) => {
  let k = 0
  for (let i = 0; i < w.length; i++) {
    k *= 19
    k += w[i] + 9
  }
  return k
}

let seqs = new Int16Array(19**4)
let bananas = new Int32Array(19**4)
let secrets2 = secrets.slice(0)
let diffs = new Int8Array(2000)
for (let j = 0; j < secrets.length; j++) {
  for (let i = 0; i < 2000; i++) {
    let next = next_secret(secrets2[j])
    diffs[i] = price(next) - price(secrets2[j])
    secrets2[j] = next
    if (i > 2) {
      // let k = hash(diffs.slice(i-3, i+1))
      let k = 0
      for (let l = i-3; l < i+1; l++) {
        k *= 19
        k += diffs[l] + 9
      }
      if (seqs[k] != j) {
        seqs[k] = j
        bananas[k] += price(next)
      }
    }
  }
}

console.log( bananas.reduce((a, b) => Math.max(a, b), 0) )
