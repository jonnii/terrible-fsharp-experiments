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

let calculateJourneyTimeTo state (direction, floor) =
    let calculateDistanceBetween ((_, leftFloor), (_, rightFloor)) =
        abs(leftFloor - rightFloor)
    
    let moving = (state |> getMovingDirection, state.floor)
    
    if state.destinations |> List.exists (fun f -> f = (direction, floor)) then 
        0
    else
        let destinations =
            state.destinations
            |> enqueueDestination (moving) (direction, floor)
    
        let (l, _) =
            state.destinations
            |> Seq.zip destinations
            |> Seq.takeWhile (fun (x, y) -> x = y)
            |> Seq.toList
            |> List.unzip
            
        match l with
        | [] -> abs(floor - state.floor)
        | some -> 
            let kk = ((Some (state |> getMovingDirection), state.floor)) :: l
            
            let travelDistance = kk |> List.pairwise |> List.map calculateDistanceBetween |> List.sum
            let lflf = travelDistance + ((max 0 ((l |> List.length))) * 5)
            
            lflf + abs((kk |> List.last |> snd) - floor)

let timeToFloor direction floor car =
    match car with
    | Idleing(state) -> abs(state.floor - floor)
    | Waiting(state, None) ->
        int(state.remainingTime) + abs(state.floor - floor)
    | Waiting(state, Some(next)) ->
        int(state.remainingTime) + calculateJourneyTimeTo next (direction, floor)
    | Moving(state) ->
        calculateJourneyTimeTo state (direction, floor)

let addDestination floor requestDirection state =
    let moving = (state |> getMovingDirection, state.floor)
    
    let destinations =
        state.destinations
        |> enqueueDestination (moving) (requestDirection, floor)
    
    {state with destinations = destinations}
    
let startMoving floor requestDirection (state: IdleingState) =
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