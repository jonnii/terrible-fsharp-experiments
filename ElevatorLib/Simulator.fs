module ElevatorLib.Simulator
open System
open ElevatorLib.Types
open ElevatorLib.Cars
open ElevatorLib.Banks

type StandingState = {
    startingFloor: int
    destinationFloor: int
    startingTime: int
}

type TransitState = {
    boardingTime: int
    carId: int
}

type ArrivedState = {
    arrivalTime: int
}

type Occupancy = {
    capacity: int
    occupancy: int
}

type SimulationState = {
    occupiedCars: (Occupancy * Car) list
    standing: StandingState list
    transiting: (StandingState * TransitState) list
    arrived: (StandingState * TransitState * ArrivedState) list
}

let createOccupancy =
    { capacity = 10
      occupancy = 0 }

let generateRides numFloors (random: Random) history =
    let generateRide second =
        match second % 5 with
        | _ when second > 240 -> []
        | 0 ->
            match random.Next(numFloors), random.Next(numFloors) with
            | startingFloor, destinationFloor when startingFloor = destinationFloor -> []
            | startingFloor, destinationFloor -> 
                let standing = {
                    startingFloor = startingFloor
                    startingTime = second
                    destinationFloor = destinationFloor
                }
                [standing]
        | _ -> []

    history
    |> Seq.toList
    |> List.map generateRide

let shouldTransit standing (waitingState: WaitingState) transit =
    standing.destinationFloor = waitingState.floor && transit.carId = waitingState.id

let createCall (ride : StandingState) =
    if ride.startingFloor < ride.destinationFloor then
        (Call (Up, ride.startingFloor))
    else
        (Call (Down, ride.startingFloor))

let doMoveOut time (state: SimulationState) (occupancy, car) =
    match car with
    | Waiting(waitingState, _) ->
        let shouldTransit (standingState, transitState) =
            transitState.carId = waitingState.id &&
            standingState.destinationFloor = waitingState.floor 
    
        let (leavers, remainers) = state.transiting |> List.partition shouldTransit
    
        let arrivers = leavers|> List.map (fun (standing, transit) -> (standing, transit, { arrivalTime = time } ))
        
        let state = { state with arrived = state.arrived @ arrivers ; transiting = remainers }
        
        ((occupancy, car), state)
    | _ -> ((occupancy, car), state)

let moveIn time carId (occupancy: Occupancy, car: Car) (standing: StandingState) = 
    let nextState = (standing, { carId = carId ; boardingTime = time } )
    nextState, (occupancy, car |> send (Send standing.destinationFloor))

let shouldMoveIn (waitingState: WaitingState) standingState =
    standingState.startingFloor = waitingState.floor

let doMoveIn time (state: SimulationState) (occupancy: Occupancy, car: Car) =
    match car with
    | Waiting(waitingState, _) ->
        let (leavers, remainers) = state.standing |> List.partition (shouldMoveIn waitingState)
    
        let (arrivers, (occupancy, car)) =
            leavers
            |> List.mapFold (moveIn time waitingState.id) (occupancy, car)
        
        let state = { state with transiting = state.transiting @ arrivers ; standing = remainers }
        
        ((occupancy, car), state)
    | _ -> ((occupancy, car), state)

let simulate strategy (state : SimulationState) (second, rides) =
    let occupiedCars = state.occupiedCars
    
    let (occupiedCars, state) = occupiedCars |> List.mapFold (doMoveOut second) state
    let (occupiedCars, state) = occupiedCars |> List.mapFold (doMoveIn second) state
    
    let state = { state with standing = state.standing @ rides }
    let calls = rides |> List.map createCall

    let (occupancies, bank) = occupiedCars |> List.unzip

    let bank = calls |> List.fold (fun call bank -> call |> dispatchWithStrategy strategy bank) bank
    let bank = bank |> List.map tick
        
    let occupiedCars = List.zip occupancies bank

    let state = { state with occupiedCars = occupiedCars }
    
    bank, state