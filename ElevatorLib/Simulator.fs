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

type Person =
    | Standing of StandingState
    | InTransit of StandingState * TransitState
    | Arrived of StandingState * TransitState * ArrivedState

type Occupancy = {
    capacity: int
    occupancy: int
}

type SimulationState = {
    occupiedCars: (Occupancy * Car) list
    people: Person list
}

let createOccupancy =
    { capacity = 10
      occupancy = 0 }

let generateRides numFloors (random: Random) history =
    let generateRide second =
        match second % 5 with
        | _ when second > 240 -> []
        | 0 ->
            // every 5 seconds place a random person
            let startingFloor = random.Next(numFloors)
            let destinationFloor = random.Next(numFloors)
            
            let person = Standing({
                startingFloor = startingFloor
                startingTime = second
                destinationFloor = destinationFloor
            })
            
            [person]
        | _ -> []

    history
    |> Seq.toList
    |> List.map generateRide

let shouldTransit standing (waitingState: WaitingState) transit =
    standing.destinationFloor = waitingState.floor && transit.carId = waitingState.id

let processExit second (occupancy, car) person =
    match person, car, occupancy with
    | InTransit(standing, transit), Waiting(waitingState, _), _ when shouldTransit standing waitingState transit ->
        Arrived(standing, transit, { arrivalTime = second }), (occupancy, car)
    | _ -> person, (occupancy, car)

let processEnter second (occupancy: Occupancy, car: Car) (person: Person) =
    match person, car, occupancy with
    | Standing(standing), Waiting(waitingState, _), _ when standing.startingFloor = waitingState.floor ->
        InTransit(standing, { boardingTime = second ; carId = waitingState.id }), (occupancy, car |> send (Send standing.destinationFloor))
    | _ -> person, (occupancy, car)

let createCall ride =
    match ride with
    | Standing(state) when state.startingFloor < state.destinationFloor ->
        (Call (Up, state.startingFloor))
    | Standing(state) ->
        (Call (Down, state.startingFloor))
    | _ -> failwith "cannot create a ride in a non waiting state"

let transit func people occupiedCar =
    let (people, occupiedCar) = people |> List.mapFold (func) occupiedCar
    (occupiedCar, people)

let simulate (state : SimulationState) (second, rides) =
    let (occupiedCars, people) = state.occupiedCars |> List.mapFold (transit (processExit second)) state.people
    let (occupiedCars, people) = occupiedCars |> List.mapFold (transit (processEnter second)) people    
    
    let people = rides @ people
    let calls = rides |> List.map createCall
    
    let (occupancies, bank) = occupiedCars |> List.unzip
    
    let bank = calls |> List.fold (fun call bank -> call |> dispatch bank) bank
    let bank = bank |> List.map tick
        
    let occupiedCars = List.zip occupancies bank
    
    let state = { state with occupiedCars = occupiedCars ; people = people }
    
    bank, state