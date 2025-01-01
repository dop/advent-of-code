const assert = require('assert')
const fs = require('fs')

let input = fs.readFileSync(0, {encoding: 'utf-8'}).trim().split('\n\n')[0]

let length = 0
for (let i = 0; i < input.length;) {
  if (input[i] == '(') {
    i++ // skip (
    let marker_length = ''
    let marker_repeat = ''
    while ('0' <= input[i] && input[i] <= '9')
      marker_length += input[i++]
    i++ // skip x
    while ('0' <= input[i] && input[i] <= '9')
      marker_repeat += input[i++]
    i++ // skip )
    length += Number(marker_length) * Number(marker_repeat)
    i += Number(marker_length)
  } else {
    // console.log(input[i])
    length++
    i++
  }
}
console.log(length)

function compute_length(input) {
  let length = 0
  for (let i = 0; i < input.length;) {
    if (input[i] == '(') {
      i++ // skip (
      let marker_length = ''
      let marker_repeat = ''
      while ('0' <= input[i] && input[i] <= '9')
        marker_length += input[i++]
      i++ // skip x
      while ('0' <= input[i] && input[i] <= '9')
        marker_repeat += input[i++]
      i++ // skip )
      length += compute_length(input.substr(i, marker_length)) * Number(marker_repeat)
      i += Number(marker_length)
    } else {
      // console.log(input[i])
      length++
      i++
    }
  }
  return length
}
console.log(compute_length(input))
