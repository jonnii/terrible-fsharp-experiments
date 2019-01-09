module ElevatorLib.Cars
open ElevatorLib.Types

let hasDestinations car =
    match car with
    | Idleing(_) -> false
    | Waiting(_, None) -> false
    | Waiting(_, Some(_)) -> true
    | Moving(_) -> true
    
let getDestinations car =
    let destinations =
        match car with
        | Idleing(_) -> []
        | Waiting(_, None) -> []
        | Waiting(_, Some(state)) -> state.destinations
        | Moving(state) -> state.destinations
    destinations |> List.map (fun (direction, floor) -> floor)

let getFloor car =
    match car with
    | Moving(state) -> state.floor
    | Waiting(state, _) -> state.floor
    | Idleing(state) -> state.floor

let getDoor car =
    match car with
    | Waiting(_, _) -> Open
    | _ -> Closed

let nextDestination state =
    match state.destinations with
    | (_, floor)::tail -> floor
    | _ -> failwith "expected a destination"

let getMovingDirection state =
    match state.floor > (state |> nextDestination) with
    | true -> Down
    | false -> Up

let getRequestDirection state =
    state.destinations |> List.head |> fst

let shouldKeepMoving state =
    state.floor <> (state |> nextDestination)

let getFloorWait floor =
    if floor = 0 then
        30<sec>
    else
        5<sec>

let isIdle car =
    match car with
    | Idleing(_) -> true
    | _ -> false

//let totalQueueLength queue =
//    0
//    //    car.Stops.DownQueue.Length + car.Stops.UpQueue.Length

let calculateTravelTimeNotOnCurrentJourney targetFloor car =
//    let timeToCompleteCurrentJourney =
//        let pendingWaits = (totalQueueLength car) * (getFloorWait 1)
//        let pendingTravels =
//            match car.Stops.UpQueue with
//            | [] -> 0
//            | [one] -> 0 // we are moving
//            | head :: tail -> abs(head - (tail |> List.last))
//        
//        let pendingTime = pendingWaits + pendingTravels
//        
//        match car.State with
//        | Idle -> pendingTime
//        | Waiting(_, time) -> time
//        | Moving(state) -> pendingTime + abs(car.Floor - state.DesiredFloor)
//
//    let timeToGetToTargetFloorFromLastStop =
//        abs(car.Stops.UpQueue |> List.tryLast |> Option.defaultValue(car.Floor - targetFloor))
//        
//    timeToGetToTargetFloorFromLastStop + timeToCompleteCurrentJourney
    0

let calculateTravelTimeOnCurrentJourney targetFloor car =
//    let stops = car.Stops.UpQueue |> List.filter (fun i -> i < targetFloor)
//    let pendingWaits = stops.Length * (getFloorWait 1)
//    pendingWaits + abs(targetFloor - car.Floor)
    0

//let isOnCurrentJourney targetFloor car =
////    match car.Stops.UpQueue with
////    | head::tail -> head < targetFloor && targetFloor < (tail |> List.last)
////    | _ -> false
//    false
//
//let timeToFloor targetFloor car =
//   let method =
//       if isOnCurrentJourney targetFloor car then
//           calculateTravelTimeOnCurrentJourney
//       else
//           calculateTravelTimeNotOnCurrentJourney
//   
//   method targetFloor car

let rec enqueueDestination (journey: (Direction * int)) request destinations =
    let isOnJourney request =
        match request, journey with 
        | (Some(Up), requestFloor), (Up, journeyFloor) when requestFloor > journeyFloor -> true
        | (Some(Down), requestFloor), (Down, journeyFloor) when requestFloor < journeyFloor -> true
        | _ -> false
    
    match destinations with
    | [] -> [request]
    | head::tail ->
        
        match head, request, journey with
        // duplicate requests or going somewhere we already have a specific
        // request for then ignore
        | head, request, _ when head = request ->
            destinations
        | (Some(_), floor), (None, requestFloor), _ when floor = requestFloor ->
            destinations
        
        // keep going if we're all aligned but not on the current journey
        | (Some(Up), _), (Some(Up), _), (Up, _) when not (isOnJourney request) ->
           head :: enqueueDestination journey request tail
        | (Some(Down), _), (Some(Down), _), (Down, _) when not (isOnJourney request) ->
           head :: enqueueDestination journey request tail
        
        // call direction upgrades when we have no defined direction on arrival
        | (None, floor), (None, requestFloor), _ when floor < requestFloor ->
            ((Some Up), floor) :: enqueueDestination journey request tail
        | (None, floor), (None, requestFloor), _ when floor > requestFloor ->
            ((Some Down), floor) :: enqueueDestination journey request tail
        | (None, floor), (Some(_), requestFloor), _ when floor = requestFloor ->
            request :: tail
        
        // put being on journey ahead of not being on journey
        | head, request, _ when request |> isOnJourney && not (head |> isOnJourney) ->
             request :: destinations
        
        // going up or down and we are on the "right" side of the trip
        | (Some(Up), floor), (Some(Up), requestFloor), _ when requestFloor < floor ->
            request :: destinations
        | (Some(Down), floor), (Some(Down), requestFloor), _ when requestFloor > floor ->
            request :: destinations
            
        | _ -> head :: enqueueDestination journey request tail

let addDestination floor requestDirection state =
    let moving = (state |> getMovingDirection, state.floor)
    
    let destinations =
        state.destinations
        |> enqueueDestination (moving) (requestDirection, floor)
    
    {state with destinations = destinations}
    
let startMoving floor (requestDirection: Direction option) (state: IdleingState) =
     let movingDirection =
        if state.floor > floor then
            Down
        else
            Up
     
     { id = state.id
       floor = state.floor
       destinations = [(requestDirection, floor)] }

let queueMove requestDirection floor (state: WaitingState) =
    let movingDirection =
        if state.floor > floor then
            Down
        else
            Up
     
    { id = state.id
      floor = state.floor
      destinations = [(requestDirection, floor)] }

let startWaiting (direction : Direction option) (state: IdleingState) =
    { id = state.id
      floor = state.floor
      remainingTime = (getFloorWait state.floor)
      direction = direction }

let resetWaiting (state: WaitingState) =
    { state with remainingTime = (getFloorWait state.floor) }

let shouldOpenDoor direction floor (call : WaitingState) =
    match (call.floor, call.direction) with
    | (f, Some(d)) -> floor = f && direction <> d
    | _ -> true    

let send command car =
    match (command, car) with
    | (Call(direction, floor), Idleing(state)) when floor = state.floor ->
        Waiting(state |> startWaiting (Some direction), None)

    | (Call(direction, floor), Idleing(state)) ->
        Moving(state |> startMoving floor (Some direction))
    
    | (Call(direction, floor), Waiting(state, next)) when shouldOpenDoor direction floor state ->
        Waiting(state, Some(state |> queueMove (Some direction) floor))

    | (Call(_, floor), Waiting(state, next)) when floor = state.floor ->
        Waiting(state |> resetWaiting, next)

    | (Call(direction, floor), Waiting(state, None)) ->
        Waiting(state, Some(state |> queueMove (Some direction) floor))

    | (Call(direction, floor), Waiting(state, Some(next))) ->
        Waiting(state, Some(next |> addDestination floor (Some direction)))
    
    | (Call(direction, floor), Moving(state)) ->
        Moving(state |> addDestination floor (Some direction))
        
    | (Send(floor), Idleing(state)) when floor = state.floor ->
        Waiting(state |> startWaiting None, None)
        
    | (Send(floor), Idleing(state)) ->
        Moving(state |> startMoving floor None)

    | (Send(floor), Waiting(state, None)) ->
        Waiting(state, Some(state |> queueMove None floor))
        
    | (Send(floor), Waiting(state, Some(next))) ->
        Waiting(state, Some(next |> addDestination floor None))
    
    | command, car -> failwithf "missingcommand %O %O" command car
        
let toWaiting (direction : Direction option) car =
    { id = car.id
      floor = car.floor
      remainingTime = (getFloorWait car.floor)
      direction = direction }

let toNextMove state =
    match state.destinations with
    | [] -> None
    | [_] -> None
    | head::tail -> Some({state with destinations = tail})
    
let toNextFloor state =
    let (_, floor) = state.destinations |> List.head
    
    let nextFloor =
        if floor < state.floor then
            state.floor - 1
        else
            state.floor + 1
                
    { state with floor = nextFloor }

let toIdle (state: WaitingState) =
    { id = state.id
      floor = state.floor }

let toNextWait state =
    { state with remainingTime = state.remainingTime - 1<sec> }
    
let tick car =
    match car with
    | Idleing(state) ->
        car
        
    | Waiting(state, next) when state.remainingTime > 0<sec> ->
        Waiting(state |> toNextWait, next)
    
    | Waiting(state, None) ->
        Idleing(state |> toIdle)

    | Waiting(state, Some(movingState)) ->
        Moving(movingState)
    
    | Moving(state) when state |> shouldKeepMoving ->
        Moving(state |> toNextFloor)
        
    | Moving(state) ->
        Waiting(state |> toWaiting (state |> getRequestDirection), state |> toNextMove)