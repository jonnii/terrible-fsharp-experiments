module ElevatorLib.Types

[<Measure>] type sec

type Direction =
    | Up
    | Down

type Doors =
    | Open
    | Closed

type IdleingState = {
    id: int
    floor: int
}

type WaitingState = {
    id: int
    floor: int
    remainingTime: int<sec>
    direction: Direction option
}

type MovingState = {
    id: int
    floor: int
    destinations: (Direction option * int) list
}

type Car =
    | Idleing of IdleingState
    | Waiting of WaitingState * MovingState option
    | Moving of MovingState

let getCarId car =
    match car with
    | Idleing(state) -> state.id
    | Waiting(state, _) -> state.id
    | Moving(state) -> state.id

type Commands =
    | Call of (Direction * int)
    | Send of int 

let createCar id =
    Idleing({
      id = id
      floor = 0 })

let createBank numCars =
    seq { for i in 1 .. numCars -> createCar i } |> Seq.toList