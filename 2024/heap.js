const assert = require('assert')

function swap(array, i, j) {
  let tmp = array[i]
  array[i] = array[j]
  array[j] = tmp;
}

class Heap {
  constructor(compare) {
    this.compare = compare;
    this.heap = []
    this.size = 0
  }

  insert(element) {
    let ei = this.size++
    this.heap[ei] = element
    if (ei > 0) {
      let pi = Math.floor((this.size - 1) / 2)
      while (ei > 0 && ei != pi && !this.compare(this.heap[pi], this.heap[ei])) {
        swap(this.heap, pi, ei)
        ei = pi
        pi = Math.floor((ei - 1) / 2)
      }
    }
  }

  remove() {
    if (this.size) {
      let top = this.heap[0]
      swap(this.heap, 0, --this.size)
      this.heap.pop()
      let i = 0
      while (true) {
        let j = i * 2 + 1
        let k = i * 2 + 2
        if (j < this.size && !this.compare(this.heap[i], this.heap[j])) {
          swap(this.heap, i, j)
          i = j
          continue
        }
        if (k < this.size && !this.compare(this.heap[i], this.heap[k])) {
          swap(this.heap, i, k)
          i = k
          continue
        }
        break
      }
      return top
    }
  }

  peak() {
    return this.heap[0]
  }
}

class MaxHeap extends Heap {
  constructor() {
    super((a, b) => a >= b)
  }
}

class MinHeap extends Heap {
  constructor() {
    super((a, b) => a <= b)
  }
}

function test() {
  let heap = new MaxHeap

  heap.insert(4)
  heap.insert(2)
  heap.insert(3)
  heap.insert(1)
  heap.insert(0)

  assert.equal( heap.peak(), 4 )
  assert.equal( heap.remove(), 4 )
  assert.equal( heap.peak(), 3 )

  let heap2 = new MinHeap

  heap2.insert(4)
  heap2.insert(2)
  heap2.insert(1)
  heap2.insert(0)
  heap2.insert(3)

  assert.equal( heap2.peak(), 0 )

  assert.equal( heap2.remove(), 0 )

  assert.equal( heap2.peak(), 1 )

  let heap3 = new MaxHeap

  heap3.insert(0)
  heap3.insert(1)
  heap3.insert(0)
  heap3.insert(0)
  heap3.insert(1)

  assert.equal( heap3.remove(), 1 )
  assert.equal( heap3.remove(), 1 )
  assert.equal( heap3.remove(), 0 )
  assert.equal( heap3.remove(), 0 )
  assert.equal( heap3.remove(), 0 )
  assert.equal( heap3.remove(), undefined )

  heap3.insert(0)
  heap3.insert(1)

  let heap4 = new MinHeap

  heap4.insert(1)
  heap4.insert(1)
  heap4.insert(0)
  heap4.insert(1)
  heap4.insert(0)

  assert.equal( heap4.remove(), 0 )
  assert.equal( heap4.remove(), 0 )
  assert.equal( heap4.remove(), 1 )
  assert.equal( heap4.remove(), 1 )
  assert.equal( heap4.remove(), 1 )
  assert.equal( heap4.remove(), undefined )
}

test()

module.exports = {
  Heap,
  MaxHeap,
  MinHeap,
}
