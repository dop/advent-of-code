const assert = require('assert')
const fs = require('fs')

let spec = fs.readFileSync(0, {encoding:'utf-8'}).trim()
  .split('\n')[0]
  .split('').map(Number)

;(function part1() {
  let size = 0

  for (let i = 0; i < spec.length; i++) {
    size += spec[i]
  }

  let disk = new Array(size)

  for (let i = 0, j = 0, id = 0; i < spec.length; i += 2) {
    for (let k = 0; k < spec[i]; k++) {
      disk[j++] = id
    }
    id++
    if (spec[i+1])
      for (let k = 0; k < spec[i+1]; k++) {
        disk[j++] = '.'
      }
  }

  // console.log(disk.join(''))

  let i = 0;
  let j = disk.length - 1;
  while (i < j) {
    while (i < disk.length && disk[i] !== '.')
      i += 1
    while (j > 0 && disk[j] === '.')
      j -= 1

    if (i >= j)
      break

    disk[i] = disk[j]
    disk[j] = '.'
  }

  // console.log(disk.join(''))

  let checksum = 0
  for ( let i = 0; i < disk.length && disk[i] != '.'; i++ ) {
    checksum += disk[i] * i
  }

  console.log(checksum)
})()

;(function part2() {
  let disk = []

  function str(disk) {
    return disk.map(block => {
      if (block.file)
        return block.id.toString(36).repeat(block.file)
      return '.'.repeat(block.free)
    }).join('')
  }

  for (let i = 0, id = 0; i < spec.length; i += 2) {
    disk.push({file: spec[i], id: id++})
    spec[i+1] && disk.push({free: spec[i+1]})
  }

  // console.log(str(disk))
  for (let j = disk.length-1; j > 0; j--) {
    for (let i = 0; i < j; i++) {
      if (disk[i].free >= disk[j].file) {
        const free = disk[i].free - disk[j].file
        disk[i] = { ...disk[j] }
        disk[j] = { free: disk[j].file }
        if (free > 0) {
          j+=1
          disk.splice(i+1, 0, { free })
        }
        // console.log(str(disk))
        break;
      }
    }
  }
  // console.log(str(disk))

  let checksum = 0
  for (let i = 0, j = 0; i < disk.length; i++) {
    if (disk[i].file)
      for (let k = 0; k < disk[i].file; k++, j++) {
        checksum += disk[i].id * j
      }
    if (disk[i].free)
      j += disk[i].free
  }

  console.log(checksum)
})()

// too low 85588967694
// too low 85738318393

// too high 6379497256282
//          6335972980679
