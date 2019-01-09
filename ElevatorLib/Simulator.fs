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
    bank: Car list
    occupancies: Occupancy list
    people: Person list
}

let createOccupancy =
    { capacity = 10
      occupancy = 0 }

let generateRides numFloors (random: Random) history =
    let generateRide second =
        match second % 5 with
        | _ when second > 30 -> []
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

let simulate (state : SimulationState) (second, rides) =
    let createCall ride =
        match ride with
        | Standing(state) when state.startingFloor < state.destinationFloor ->
            (Call (Up, state.startingFloor))
        | Standing(state) ->
            (Call (Down, state.startingFloor))
        | _ -> failwith "cannot create a ride in a non waiting state"

    
    let processExits people occupiedCar =
        let processExit (occupancy: Occupancy, car: Car) (person: Person) =
            match person, car, occupancy with
            | InTransit(standing, transit), Waiting(waitingState, _), _ when standing.destinationFloor = waitingState.floor && transit.carId = waitingState.id ->
                Arrived(standing, transit, { arrivalTime = second }), (occupancy, car)
            | _ -> person, (occupancy, car)
        let (people, occupiedCar) = people |> List.mapFold processExit occupiedCar
        (occupiedCar, people)

    let processEnters people occupiedCar =
        let processExit (occupancy: Occupancy, car: Car) (person: Person) =
            match person, car, occupancy with
            | Standing(standing), Waiting(waitingState, _), _ when standing.startingFloor = waitingState.floor ->
                InTransit(standing, { boardingTime = second ; carId = waitingState.id }), (occupancy, car |> send (Send standing.destinationFloor))
            | _ -> person, (occupancy, car)
        let (people, occupiedCar) = people |> List.mapFold processExit occupiedCar
        (occupiedCar, people)
    
    // move people in and out of the cars
    let (list, people) =
        List.zip state.occupancies state.bank
        |> List.mapFold processExits state.people
    
    let (list, people) = list |> List.mapFold processEnters people
    
    let (occupancies, cars) = list |> List.unzip
    
    // place new riders
    let people = rides @ people
    
    // dispatch any actions and update state of the elevators
    let calls = rides |> List.map createCall
    
    let bank =
        calls
        |> List.fold (fun call bank -> call |> dispatch bank) cars
        |> List.map tick 
    
    let state = { state with bank = bank ; occupancies = occupancies ; people = people }
    
    bank, state