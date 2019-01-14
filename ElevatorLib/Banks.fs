module ElevatorLib.Banks
open ElevatorLib.Types
open ElevatorLib.Cars

let getFloors bank =
    bank |> List.map getFloor        

let getDoors bank =
    bank |> List.map getDoor        

let findFirstIdleCareOrWithFewestDestinations direction floor cars =
    let idle = cars |> List.tryFind isIdle
    match idle with
    | Some(car) -> car
    | None ->
        cars |> List.sortBy (fun f -> f |> getDestinations |> List.length) |> List.head

let findClosestCarByJourneyTime direction floor cars =
    cars
    |> List.sortBy (timeToFloor direction floor)
    |> List.head
    
let dispatchWithStrategy strategy command bank =
    match command with
    | Call(direction, floor) ->
        let car = bank |> strategy (Some direction) floor
        bank |> List.map (fun c -> if c = car then (send command car) else c)
