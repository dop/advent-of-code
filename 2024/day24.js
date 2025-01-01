const assert = require('assert')
const fs = require('fs')

let [gates, wiring] = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')

gates = gates.split('\n').map(gate => {
  const [name, value] = gate.split(': ')
  return [name, BigInt(value)]
})

let zs = []

wiring = wiring.split('\n').map(wire => {
  const [a, op, b, , out] = wire.split(' ')
  if (out[0] == 'z')
    zs.push(out)
  return [out, [op, a, b]]
})

let code = new Map([...gates, ...wiring])

function apply(op, a, b, print = 0, cache, visited) {
  let A = resolve(a, print, cache, visited)
  let B = resolve(b, print, cache, visited)
  switch (op) {
  case 'OR':
    return A | B
  case 'AND':
    return A & B
  case 'XOR':
    return A ^ B
  }
}

function resolve(name, print = 0, cache = new Map, visited = new Set) {
  if (cache.has(name))
    return cache.get(name)
  // if (visited.has(name))
  //   throw new Error(`loops on ${name}`)
  // visited.add(name)
  let value = code.get(name)
  if (typeof value == 'number' || typeof value === 'bigint') {
    // if (print)
    //   console.log('  '.repeat(print-1), name, '=', value)
  } else {
    const [op, a, b] = value
    if (print) {
      console.log('  '.repeat(print-1), name, '=', a, op, b)
    }
    value = apply(op, a, b, print && print + 1, cache, visited)
  }
  cache.set(name, value)
  return value
}

console.log(add_up() + '') // 51657025112326

// ------------ part 2 ---------------

function bit_name(var_name, bit) {
  return bit.toString().padStart(3, var_name + '0')
}

function set_bit(var_name, bit, value) {
  code.set(bit_name(var_name, bit), value)
}

function swap(a, b) {
  let tmp = code.get(a)
  code.set(a, code.get(b))
  code.set(b, tmp)
}

function reset() {
  for (let i = 0; i < 45; i++) {
    set_bit('x', i, 0)
    set_bit('y', i, 0)
  }
}

function set_input(x, y) {
  for (let i = 0; i < 45; i++) {
    let val1 = BigInt(Boolean(x & (BigInt(1) << BigInt(i))))
    let val2 = BigInt(Boolean(y & (BigInt(1) << BigInt(i))))
    set_bit('x', i, val1)
    set_bit('y', i, val2)
  }
}

function add_up() {
  let answer = BigInt(0)
  for (let i = 45; i >= 0; i--) {
    answer *= BigInt(2)
    answer += BigInt(resolve(bit_name('z', i)))
  }
  return answer
}

function add(x, y) {
  set_input(x, y)
  return add_up()
}

function bad_bits() {
  let bits = new Set
  for ( let i = 0; i < 45; i++ ) {
    let bit = BigInt(1) << BigInt(i)
    let result = add(bit, bit)
    let expected = bit+bit
    if (result != expected) {
      bits.add(i)
    }
  }
  for ( let i = 0; i < 45; i++ ) {
    let bit = BigInt(1) << BigInt(i)
    let result = add(bit, BigInt(0))
    if (result != bit) {
      bits.add(i)
    }
  }
  for ( let i = 0; i < 45; i++ ) {
    let bit = BigInt(1) << BigInt(i)
    let result = add(BigInt(0), bit)
    if (result != bit) {
      bits.add(i)
    }
  }
  return bits
}

// Hand picked for the output produced below.
let swaps = [
  ['z05', 'hdt'],
  ['z09', 'gbf'],
  ['mht', 'jgt'],
  ['z30', 'nbf'],
]

for (let [a, b] of swaps)
  swap(a, b)

let cache = new Map
let bb = bad_bits()
if (bb.size > 0) {
  console.log( bb )
  let bba = Array.from(bb)
  for (let i = 0; i < 45; i++) {
    resolve((i+'').padStart(3, 'z0'), i >= bba[0] ? 1 : 0, cache)
  }
}

console.log( swaps.flat().sort()+'' )
