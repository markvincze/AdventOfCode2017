open System.Collections.Generic

// let startA = 65UL
let startA = 703UL
let factorA = 16807UL
// let startB = 8921UL
let startB = 516UL
let factorB = 48271UL
let divider = 2147483647UL

let match16bit a b = (a &&& 0xffffUL) = (b &&& 0xffffUL)

let rec countMatches prevA prevB iterations acc =
    if iterations = 0
    then acc
    else
        let nextA = (prevA * factorA) % divider
        let nextB = (prevB * factorB) % divider

        if match16bit nextA nextB
        then countMatches nextA nextB (iterations - 1) (acc + 1)
        else countMatches nextA nextB (iterations - 1) acc

// countMatches startA startB 40_000_000 0

let rec procQueues (queueA : Queue<uint64>) (queueB : Queue<uint64>) acc count =
    if queueA.Count = 0 || queueB.Count = 0
    then (acc, count)
    else
        let numA = queueA.Dequeue()
        let numB = queueB.Dequeue()

        if match16bit numA numB
        then procQueues queueA queueB (acc + 1) (count + 1)
        else procQueues queueA queueB acc (count + 1)


let rec countMatches2 prevA prevB (queueA : Queue<uint64>) (queueB : Queue<uint64>) iterations judgedCount acc =
    if judgedCount >= iterations
    then acc
    else
        let nextA = (prevA * factorA) % divider
        let nextB = (prevB * factorB) % divider

        if nextA % 4UL = 0UL
        then queueA.Enqueue(nextA)
        else ()

        if nextB % 8UL = 0UL
        then queueB.Enqueue(nextB)
        else ()

        let (matches, count) = procQueues queueA queueB 0 0

        countMatches2 nextA nextB queueA queueB iterations (judgedCount + count) (acc + matches)

countMatches2 startA startB (new Queue<uint64>()) (new Queue<uint64>()) 5_000_000 0 0