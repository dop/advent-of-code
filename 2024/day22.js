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

let seqs = {}
let diffs = {}
let bananas = new Map
let secrets2 = secrets.slice(0)
for (let j = 0; j < secrets.length; j++) {
  diffs[j] = []
  seqs[j] = new Set
  for (let i = 0; i < 2000; i++) {
    let next = next_secret(secrets2[j])
    diffs[j].push(price(next) - price(secrets2[j]))
    secrets2[j] = next
    if (i > 2) {
      let k = diffs[j].slice(-4)+''
      if (!seqs[j].has(k)) {
        seqs[j].add(k)
        bananas.set(k, (bananas.get(k) ?? 0) + price(next))
      }
    }
  }
}

console.log( bananas.values().reduce((a, b) => Math.max(a, b), 0) )
