module ElevatorLib.Banks
open ElevatorLib.Types
open ElevatorLib.Cars

let getFloors bank =
    bank |> List.map getFloor        

let getDoors bank =
    bank |> List.map getDoor        

let findClosestCarByJourneyTime floor cars =
    let idle = cars |> List.tryFind isIdle
    match idle with
    | Some(car) -> car
    | None ->
        cars |> List.sortBy (fun f -> f |> getDestinations |> List.length) |> List.head

let findFirstIdleCar floor cars =
    cars |> List.find isIdle

let dispatchWithStrategy strategy command bank =
    match command with
    | Call(direction, floor) ->
        let car = bank |> strategy floor
                       
        bank
        |> List.map (fun c -> if c = car then (send command car) else c)

let dispatch command bank =
    dispatchWithStrategy findClosestCarByJourneyTime command bank
    