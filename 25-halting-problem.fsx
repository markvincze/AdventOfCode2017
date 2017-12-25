open System
open System.IO

type State = | A | B | C | D | E | F

type MoveDir = | Left | Right

type Operation = {
    WriteValue : int
    MoveDir : MoveDir
    NextState : State
}

type Instruction = {
    Operation0 : Operation
    Operation1 : Operation
}

type Program = {
    InitialState : State
    ChecksumSteps : int
    Instructions : Map<State, Instruction>
}

let program = {
    InitialState = A
    ChecksumSteps = 12261543
    Instructions = Map.empty<State, Instruction>
                       .Add(A, { Operation0 = { WriteValue = 1; MoveDir = Right; NextState = B }
                                 Operation1 = { WriteValue = 0; MoveDir = Left; NextState = C } })
                       .Add(B, { Operation0 = { WriteValue = 1; MoveDir = Left; NextState = A }
                                 Operation1 = { WriteValue = 1; MoveDir = Right; NextState = C } })
                       .Add(C, { Operation0 = { WriteValue = 1; MoveDir = Right; NextState = A }
                                 Operation1 = { WriteValue = 0; MoveDir = Left; NextState = D } })
                       .Add(D, { Operation0 = { WriteValue = 1; MoveDir = Left; NextState = E }
                                 Operation1 = { WriteValue = 1; MoveDir = Left; NextState = C } })
                       .Add(E, { Operation0 = { WriteValue = 1; MoveDir = Right; NextState = F }
                                 Operation1 = { WriteValue = 1; MoveDir = Right; NextState = A } })
                       .Add(F, { Operation0 = { WriteValue = 1; MoveDir = Right; NextState = A }
                                 Operation1 = { WriteValue = 1; MoveDir = Right; NextState = E } })
}

type MemoryBlock = {
    mutable Value : int
    mutable Left : MemoryBlock option
    mutable Right : MemoryBlock option
}

let memory = {
    Value = 0
    Left = None
    Right = None
}

let rec proc program memory state cnt =
    if cnt = 0
    then memory
    else let instruction = Map.find state program.Instructions
         let operation = if memory.Value = 0
                         then instruction.Operation0
                         else instruction.Operation1
         memory.Value <- operation.WriteValue

         let nextMemory =
             match operation.MoveDir with
             | Left -> match memory.Left with
                       | Some m -> m
                       | None -> let m = { Value = 0; Left = None; Right = Some memory }
                                 memory.Left <- Some m
                                 m
             | Right -> match memory.Right with
                        | Some m -> m
                        | None -> let m = { Value = 0; Right = None; Left = Some memory }
                                  memory.Right <- Some m
                                  m

         proc program nextMemory operation.NextState (cnt - 1)

let checksum memory =
    let rec countLeft memory acc =
         match memory with
         | Some memory -> countLeft memory.Left (acc + memory.Value)
         | None -> acc

    let rec countRight memory acc =
         match memory with
         | Some memory -> countRight memory.Right (acc + memory.Value)
         | None -> acc

    (countLeft memory.Left 0) + (countRight memory.Right 0) + memory.Value

proc program memory program.InitialState program.ChecksumSteps

let sol = checksum memory
