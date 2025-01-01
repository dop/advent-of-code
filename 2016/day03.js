// --- Day 3: Squares With Three Sides ---

// Now that you can think clearly, you move deeper into the labyrinth
// of hallways and office furniture that makes up this part of Easter
// Bunny HQ. This must be a graphic design department; the walls are
// covered in specifications for triangles.

// Or are they?

// The design document gives the side lengths of each triangle it
// describes, but... 5 10 25? Some of these aren't triangles. You
// can't help but mark the impossible ones.

// In a valid triangle, the sum of any two sides must be larger than
// the remaining side. For example, the "triangle" given above is
// impossible, because 5 + 10 is not larger than 25.

// In your puzzle input, how many of the listed triangles are
// possible?

const assert = require('assert')
const fs = require('fs')

let triangles = fs.readFileSync(0, {encoding: 'utf-8'})
  .trim()
  .split('\n')
  .map(line => line.trim().split(/\s+/).map(Number))

let count = 0
let sorted = triangles.map(triangle => triangle.toSorted((a, b) => a - b))
for (let [a, b, c] of sorted) {
  if (a + b > c)
    count++
}
console.log(count)


// --- Part Two ---

// Now that you've helpfully marked up their design documents, it
// occurs to you that triangles are specified in groups of three
// vertically. Each set of three numbers in a column specifies a
// triangle. Rows are unrelated.

// For example, given the following specification, numbers with the
// same hundreds digit would be part of the same triangle:

// 101 301 501
// 102 302 502
// 103 303 503
// 201 401 601
// 202 402 602
// 203 403 603

// In your puzzle input, and instead reading by columns, how many of
// the listed triangles are possible?

let count2 = 0
for (let r = 0; r < triangles.length; r += 3) {
  for (let c = 0; c < 3; c++) {
    let [x, y, z] = [ triangles[r][c], triangles[r+1][c], triangles[r+2][c] ].sort((a, b) => a - b)
    if (x + y > z)
      count2++
  }
}
console.log(count2)
