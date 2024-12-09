const fs = require('fs')

let spec = fs.readFileSync(0, {encoding:'utf-8'}).trim()
  .split('').map(Number)

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
  for (let k = 0; k < spec[i+1]; k++) {
    disk[j++] = '.'
  }
}

let i = 0;
let j = disk.length - 1;
while (i < j) {
  while (disk[i] != '.')
    i += 1
  while (disk[j] == '.')
    j -= 1

  if (i >= j)
    break

  // console.log(disk.join(''))

  disk[i] = disk[j]
  disk[j] = '.'
}

// console.log(disk.join(''))

let checksum = 0
for ( let i = 0; i < disk.length && disk[i] != '.'; i++ ) {
  checksum += disk[i] * i
}

console.log(checksum)

// lines.forEach((line) => process.stdout.write(line + '\n'))
