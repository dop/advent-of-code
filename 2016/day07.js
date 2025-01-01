// --- Day 7: Internet Protocol Version 7 ---

// While snooping around the local network of EBHQ, you compile a list
// of IP addresses (they're IPv7, of course; IPv6 is much too
// limited). You'd like to figure out which IPs support TLS
// (transport-layer snooping).

// An IP supports TLS if it has an Autonomous Bridge Bypass
// Annotation, or ABBA. An ABBA is any four-character sequence which
// consists of a pair of two different characters followed by the
// reverse of that pair, such as xyyx or abba. However, the IP also
// must not have an ABBA within any hypernet sequences, which are
// contained by square brackets.

// For example:

// abba[mnop]qrst supports TLS (abba outside square brackets).
// abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
// aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
// ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).
// How many IPs in your puzzle input support TLS?

const assert = require('assert')
const fs = require('fs')

let ips = fs.readFileSync(0, {encoding: 'utf-8'})
  .trim()
  .split('\n\n')[0]
  .split('\n')
  .map(ip => ip.split(/[\[\]]/))

function find_abba(seq) {
  for (let i = 0; i < seq.length - 3; i++) {
    if (seq[i] != seq[i+1] && seq[i] == seq[i+3] && seq[i+1] == seq[i+2])
      return true
  }
  return false
}

let count = 0
for (let ip of ips) {
  let hyseqs = []
  let seqs = []
  for (let i = 0; i < ip.length; i++) {
    if (i % 2 == 0)
      seqs.push(ip[i])
    else
      hyseqs.push(ip[i])
  }
  let ok = false
  for (let seq of seqs) {
    if (find_abba(seq)) {
      ok = true
      break
    }
  }
  if (ok) {
    for (let hyseq of hyseqs) {
      if (find_abba(hyseq)) {
        ok = false
        break
      }
    }
    if (ok) {
      count++
    }
  }
}
console.log(count)

// 191 too high

// --- Part Two ---

// You would also like to know which IPs support SSL (super-secret
// listening).

// An IP supports SSL if it has an Area-Broadcast Accessor, or ABA,
// anywhere in the supernet sequences (outside any square bracketed
// sections), and a corresponding Byte Allocation Block, or BAB,
// anywhere in the hypernet sequences. An ABA is any three-character
// sequence which consists of the same character twice with a
// different character between them, such as xyx or aba. A
// corresponding BAB is the same characters but in reversed positions:
// yxy and bab, respectively.

// For example:

// aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
// xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
// aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because the interior character must be different).
// zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).

// How many IPs in your puzzle input support SSL?

count = 0
for (let ip of ips) {
  let hyseqs = []
  let seqs = []
  for (let i = 0; i < ip.length; i++) {
    if (i % 2 == 0)
      seqs.push(ip[i])
    else
      hyseqs.push(ip[i])
  }
  let abas = []
  for (let seq of seqs) {
    for (let i = 0; i < seq.length-2; i++) {
      if (seq[i] != seq[i+1] && seq[i] == seq[i+2])
        abas.push(seq.substr(i, 3))
    }
  }
  search: {
    for (let aba of abas) {
      for (let hyseq of hyseqs) {
        for (let i = 0; i < hyseq.length-2; i++) {
          if (hyseq[i] == aba[1] && hyseq[i+1] == aba[0] && hyseq[i+2] == aba[1]) {
            count++
            break search
          }
        }
      }
    }
  }
}
console.log(count)
