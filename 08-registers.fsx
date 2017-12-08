open System

let input = [|
    "b inc 5 if a > 1"
    "a inc 1 if b < 5"
    "c dec -10 if a >= 1"
    "c inc -20 if c == 10"
|]

type Operation =
| Inc
| Dec

type ConditionType =
| Smaller
| SmallerOrEqual
| Greater
| GreaterOrEqual
| Equal
| NotEqual

type Condition = {
    Type : ConditionType
    Register : string
    Value : int
}

type Instruction = {
    Register : string
    Operation : Operation
    Value : int
    Condition : Condition
}

let parseCondition reg conditionType value =
    {
        Register = reg
        Type = match conditionType with
               | "<" -> Smaller
               | "<=" -> SmallerOrEqual
               | ">" -> Greater
               | ">=" -> GreaterOrEqual
               | "==" -> Equal
               | "!=" -> NotEqual
               | _ -> failwith "Invalid condition literal"
        Value = Int32.Parse value
    }

let parseInstruction (line : string) =
    let tokens = line.Split [| ' ' |]

    {
        Register = tokens.[0]
        Operation = if tokens.[1] = "inc" then Inc else Dec
        Value = Int32.Parse tokens.[2]
        Condition = parseCondition tokens.[4] tokens.[5] tokens.[6]
    }

let evaluate (condition : Condition) registers =
    let registerValue = Map.tryFind condition.Register registers
                        |> Option.defaultValue 0

    match condition.Type with
    | Smaller -> registerValue < condition.Value
    | SmallerOrEqual -> registerValue <= condition.Value
    | Greater -> registerValue > condition.Value
    | GreaterOrEqual -> registerValue >= condition.Value
    | Equal -> registerValue = condition.Value
    | NotEqual -> registerValue <> condition.Value

let updateRegister reg value (registers : Map<string, int>) highest =
    Map.add reg value registers, if highest > value then highest else value

let executeInstruction instruction registers highest =
    let registerValue = Map.tryFind instruction.Register registers
                        |> Option.defaultValue 0

    match instruction.Operation with
    | Inc -> updateRegister instruction.Register (registerValue + instruction.Value) registers highest
    | Dec -> updateRegister instruction.Register (registerValue - instruction.Value) registers highest

let processInstruction instruction registers highest =
    if evaluate instruction.Condition registers
    then executeInstruction instruction registers highest
    else registers, highest

let rec processInstructions ((registers : Map<string, int>), highest) instructions  =
    match instructions with
    | h :: t -> processInstructions (processInstruction h registers highest) t
    | [] -> registers, highest

[<EntryPoint>]
let main argv =
    let registers =
        input
        |> Seq.map parseInstruction
        |> Seq.toList
        |> processInstructions (Map.empty<string, int>, 0)

    let finalMaxValue =
        registers
        |> fst
        |> Seq.maxBy (fun r -> r.Value)

    let maxValueInProcess = registers |> snd

    printfn "Final max value: %d" finalMaxValue.Value
    printfn "Max value in process: %d" maxValueInProcess
    0
