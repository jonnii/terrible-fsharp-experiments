module Cars

    open FsUnit
    open Xunit
    open ElevatorLib.Types
    open ElevatorLib.Cars
    
    let moveToFloor floor car =
        match car with
        | Idleing(state) ->
            Idleing({ state with floor = floor })
        | _ -> failwith "can only explicitely move an idleing car"
    
    let repeat f n =
        Seq.init n (fun _ u -> f u)
            |> Seq.reduce (>>)

    let shouldBeWaitingAt floor direction (time: int<sec>) car = 
        match car with
        | Waiting(state, _) -> 
            state.floor |> should equal floor
            state.remainingTime |> should equal time
            state.direction |> should equal direction
        | _ -> failwith "wrong state"
    
    [<Fact>]
    let ``creating a car`` () =
        let car = createCar 0
        match car with
        | Idleing(state) -> 
            state.floor |> should equal 0
        | _ -> failwith "wrong state"
  
    [<Fact>]
    let ``calling an car from same floor`` () =
        let car = createCar 0
                  |> send (Call (Up, 0))
        
        car |> shouldBeWaitingAt 0 (Some Up) 30<sec>
        car |> hasDestinations |> should be False
        
    [<Fact>]
    let ``calling an car from same floor with doors already open should reset time`` () =
        let car = createCar 0
                  |> send (Call (Up, 0))
        
        let car = car |> repeat tick 5 |> send (Call (Up, 0))
        
        car |> shouldBeWaitingAt 0 (Some Up) 30<sec>
        car |> hasDestinations |> should be False

    [<Fact>]
    let ``calling an car from same floor with doors already open but for diff direction`` () =
        let car = createCar 0
                  |> moveToFloor 5
                  |> send (Call (Up, 5))
                  |> send (Call (Down, 5))
                  |> send (Call (Up, 10))
        
        car |> getDestinations |> should equal [10; 5]
        
    [<Fact>]
    let ``calling an car down from different floor`` () =
        let car = createCar 0
                  |> moveToFloor 1
                  |> send (Call (Up, 0))
        
        match car with
        | Moving(state) -> 
            state.floor |> should equal 1
            state |> nextDestination |> should equal 0
            state |> getMovingDirection |> should equal Down
            state |> getRequestDirection |> should equal (Some Up)
        | _ -> failwith "wrong state"
        
        car |> getDestinations |> should equal [0]
        car |> hasDestinations |> should be True

    [<Fact>]
    let ``calling an car up from different floor`` () =
        let car = createCar 0
                  |> send (Call (Up, 30))
        
        match car with
        | Moving(state) -> 
            state.floor |> should equal 0
            state |> nextDestination |> should equal 30
            state |> getMovingDirection |> should equal Up
            state |> getRequestDirection |> should equal (Some Up)
        | _ -> failwith "wrong state"
        
        car |> getDestinations |> should equal [30]

    [<Fact>]
    let ``calling a car to multiple floors`` () =
        let car = createCar 0
                  |> moveToFloor 20
                  |> send (Call (Up, 5))
                  |> send (Call (Up, 10))
                  |> send (Call (Up, 8))
        
        car |> getDestinations |> should equal [5; 8; 10]

    [<Fact>]
    let ``calling a car to multiple floors going down`` () =
        let car = createCar 0
                  |> moveToFloor 80
                  |> send (Call (Down, 5))
                  |> send (Call (Down, 10))
                  |> send (Call (Down, 8))
        
        car |> getDestinations |> should equal [10; 8; 5]

    [<Fact>]
    let ``calling a car down and then up`` () =
        let car = createCar 0
                  |> moveToFloor 80
                  |> send (Call (Down, 5))
                  |> send (Call (Down, 10))
                  |> send (Call (Down, 8))
                  |> send (Call (Up, 12))
                  |> send (Call (Up, 4))
                  |> send (Call (Up, 8))
         
        car |> getDestinations |> should equal [10; 8; 5; 4; 8; 12]
        
    [<Fact>]
    let ``calling up then down then up`` () =
        let car = createCar 0
                  |> moveToFloor 10
                  |> send (Call (Up, 15))
                  |> send (Call (Up, 20))
                  |> send (Call (Down, 8))
                  |> send (Call (Up, 6))
                  |> send (Call (Up, 18))
                  |> send (Call (Up, 9))
         
        car |> getDestinations |> should equal [15; 18; 20; 8; 6; 9]
        
    [<Fact>]
    let ``calling down then up then down`` () =
        let car = createCar 0
                  |> moveToFloor 15
                  |> send (Call (Down, 10))
                  |> send (Call (Down, 5))
                  |> send (Call (Up, 8))
         
        car |> getDestinations |> should equal [10; 5; 8]

    [<Fact>]
    let ``calling an car from 1 floor up to the lobby`` () =
        let car = createCar 0
                  |> moveToFloor 1
                  |> send (Call (Up, 0))

        match car with
        | Moving(state) -> 
            state |> nextDestination |> should equal 0
            state.floor |> should equal 1
            state |> getMovingDirection |> should equal Down
            state |> getRequestDirection |> should equal (Some Up)
        | _ -> failwith "wrong state"

        let car = car |> tick

        car |> getFloor |> should equal 0
        
        let car = car |> tick
        
        match car with
        | Waiting(state, None) -> 
            state.floor |> should equal 0
            state.remainingTime |> should equal 30
        | _ -> failwith "wrong state"
        
        car |> getDestinations |> should be Empty
        
    [<Fact>]
    let ``calling a waiting car up to a new floor`` () =
        let car = createCar 0
                  |> send (Call (Up, 0))

        match car with
        | Waiting(state, None) -> 
            state.floor |> should equal 0
        | _ -> failwith "wrong state"

        let car = car |> tick
        let car = car |> send (Call (Up, 70))

        car |> getFloor |> should equal 0
        car |> getDestinations |> should equal [70]
        
    [<Fact>]
    let ``calling a waiting car up to a new floor when already has a destination`` () =
        let car = createCar 0
                  |> send (Call (Up, 0))

        match car with
        | Waiting(state, None) -> 
            state.floor |> should equal 0
        | _ -> failwith "wrong state"

        let car = car |> tick
        let car = car |> send (Call (Up, 70))
        
        let car = car |> tick
        let car = car |> send (Call (Down, 65))

        car |> getFloor |> should equal 0
        car |> getDestinations |> should equal [70; 65]

    [<Fact>]
    let ``calling a car from the 10th floor`` () =
        let car = createCar 0
                  |> send (Call (Down, 10))

        let car = car |> repeat tick 10
        car |> getFloor |> should equal 10
        
        match car with
        | Moving(state) -> state |> getRequestDirection |> should equal (Some Down)
        | _ -> failwith "wrong state"
        
        let car = car |> tick
        match car with
        | Waiting(state, None) -> state.remainingTime |> should equal 5
        | _ -> failwith "wrong state"
        car |> getFloor |> should equal 10
        
        let car = car |> repeat tick 3
        match car with
        | Waiting(state, None) -> state.remainingTime |> should equal 2
        | _ -> failwith "wrong state"

        let car = car |> repeat tick 3
        match car with
        | Idleing(state) -> state.floor |> should equal 10
        | _ -> failwithf "wrong state"

    [<Fact>]
    let ``toNextFloor`` () =
        let next = toNextFloor { id = 0; floor = 5; destinations = [(Some Up, 3)] }
        next.floor |> should equal 4
    
    [<Fact>]
    let ``calling a car with multiple stop`` () =
        let car = createCar 0
                  |> send (Call (Down, 10))
                  |> send (Call (Down, 5))
                  |> send (Call (Down, 8))

        car |> getDestinations |> should equal [10; 8; 5]
        car |> hasDestinations |> should be True
        
        // first go to the 10th floor
        let car = car |> repeat tick 10
        car |> getFloor |> should equal 10
        
        let car = car |> tick
        car |> getDestinations |> should equal [8; 5]
        car |> hasDestinations |> should be True

        match car with
        | Waiting(state, Some(next)) ->
            state.remainingTime |> should equal 5
            next |> getRequestDirection |> should equal (Some Down)
            next.destinations |> should equal [(Some Down, 8); (Some Down, 5)]
        | _ -> failwith "wrong state"
        
        let car = car |> repeat tick 5
        match car with
        | Waiting(state, Some(next)) ->
            state.remainingTime |> should equal 0
            next |> getRequestDirection |> should equal (Some Down)
        | _ -> failwith "wrong state"
        car |> getFloor |> should equal 10
        
        let car = car |> repeat tick 4
        car |> getFloor |> should equal 8
        car |> getDestinations |> should equal [5]
                
        let car = car |> repeat tick 10
        car |> getFloor |> should equal 5
        
        let car = car |> repeat tick 10
        car |> getFloor |> should equal 5
        match car with
        | Idleing(state) -> state.floor |> should equal 5
        | _ -> failwith "wrong state"
    
    [<Fact>]
    let ``sending a car from the same floor`` () =
        let car = createCar 0
                  |> moveToFloor 1
                  |> send (Send 1)
        
        car |> shouldBeWaitingAt 1 None 5<sec>
        car |> hasDestinations |> should be False
        
    [<Fact>]
    let ``sending an car from same floor with doors already open but for diff direction`` () =
        let car = createCar 0
                  |> moveToFloor 5
                  |> send (Send 10)
        
        car |> getDestinations |> should equal [10]
    
    [<Fact>]
    let ``sending when waiting`` () =
        let car = createCar 0
                  |> moveToFloor 10
                  |> send (Call (Up, 10))
                  |> send (Send 15)
        
        car |> shouldBeWaitingAt 10 (Some Up) 5<sec>
        car |> getDestinations |> should equal [15]
            
    [<Fact>]
    let ``sending when waiting to multiple floors`` () =
        let car = createCar 0
                  |> moveToFloor 10
                  |> send (Call (Up, 10))
                  |> send (Send 15)
                  |> send (Send 20)
        
        car |> shouldBeWaitingAt 10 (Some Up) 5<sec>
        car |> getDestinations |> should equal [15; 20]
    
    [<Fact>]
    let ``adding a destination with direction upgrade going up`` () =
        let none : Direction option = None
        let journey = (Up, 0)
        
        []
        |> enqueueDestination journey (none, 5)
        |> enqueueDestination journey (none, 10)
        |> should equal [(Some(Up), 5); (None, 10)]

    [<Fact>]
    let ``adding a destination with direction upgrade going down`` () =
        let none : Direction option = None
        let journey = (Down, 15)
        
        []
        |> enqueueDestination journey (none, 10)
        |> enqueueDestination journey (none, 5)
        |> should equal [(Some(Down), 10); (None, 5)]
        
    [<Fact>]
    let ``adding duplicate destinations`` () =
        let none : Direction option = None
        let journey = (Up, 0)
        
        []
        |> enqueueDestination journey ((Some Up), 5)
        |> enqueueDestination journey ((Some Up), 5)
        |> should equal [(Some(Up), 5)]
    
    [<Fact>]
    let ``adding with specific up destinations`` () =
        let none : Direction option = None
        let journey = (Up, 0)
        
        []
        |> enqueueDestination journey ((Some Up), 5)
        |> enqueueDestination journey ((Some Up), 10)
        |> should equal [(Some(Up), 5); (Some(Up), 10)]
        
        []
        |> enqueueDestination journey ((Some Up), 10)
        |> enqueueDestination journey ((Some Up), 5)
        |> should equal [(Some(Up), 5); (Some(Up), 10)]

    [<Fact>]
    let ``adding with specific down destinations`` () =
        let none : Direction option = None
        let journey = (Down, 15)
        
        []
        |> enqueueDestination journey ((Some Down), 5)
        |> enqueueDestination journey ((Some Down), 10)
        |> should equal [(Some(Down), 10); (Some(Down), 5)]
        
        []
        |> enqueueDestination journey ((Some Down), 10)
        |> enqueueDestination journey ((Some Down), 5)
        |> should equal [(Some(Down), 10); (Some(Down), 5)]

    [<Fact>]
    let ``adding a destination with a specific direction on existing floor should upgrade`` () =
        let none : Direction option = None
        let journey = (Up, 0)
        
        []
        |> enqueueDestination journey (none, 5)
        |> enqueueDestination journey (Some(Up), 5)
        |> should equal [(Some(Up), 5)]
        
        []
        |> enqueueDestination journey (none, 5)
        |> enqueueDestination journey (none, 10)
        |> enqueueDestination journey ((Some Down), 10)
        |> should equal [(Some(Up), 5); (Some(Down), 10)]
    
    [<Fact>]
    let ``adding a destination with no specific floor with existing direction should be ignored`` () =
        let none : Direction option = None
        let journey = (Up, 0)

        []
        |> enqueueDestination journey (Some(Up), 5)
        |> enqueueDestination journey (none, 5)
        |> should equal [(Some(Up), 5)]

    [<Fact>]
    let ``adding a direction for a different direction than already scheduled`` () =
        let none : Direction option = None
        let journey = (Up, 0)

        []
        |> enqueueDestination journey (Some(Up), 5)
        |> enqueueDestination journey (Some(Down), 5)
        |> should equal [(Some(Up), 5); (Some(Down), 5)]

        []
        |> enqueueDestination journey (Some(Down), 5)
        |> enqueueDestination journey (Some(Up), 5)
        |> should equal [(Some(Up), 5); (Some(Down), 5)]

    [<Fact>]
    let ``adding a direction below the current floor when going up`` () =
        let none : Direction option = None
        let journey = (Up, 5)

        []
        |> enqueueDestination journey (Some(Up), 10)
        |> enqueueDestination journey (Some(Up), 1)
        |> should equal [(Some(Up), 10); (Some(Up), 1)]

    [<Fact>]
    let ``adding a direction above the current floor when going down`` () =
        let journey = (Up, 5)
        
        []
        |> enqueueDestination journey (Some(Down), 1)
        |> enqueueDestination journey (Some(Down), 10)
        |> should equal [(Some(Down), 10); (Some(Down), 1)]
    
    [<Fact>]
    let ``magical thing`` () =
        let journey = (Up, 5)
        
        []
        |> enqueueDestination journey (Some(Down), 5)
        |> enqueueDestination journey (Some(Up), 10)
        |> should equal [(Some(Up), 10) ; (Some(Down), 5)]

    [<Fact>]
    let ``adding a new up journey for a car which is above journey start`` () =
        let journey = (Up, 20)
        
        []
        |> enqueueDestination journey (Some(Up), 5)
        |> enqueueDestination journey (Some(Up), 25)
        |> should equal [(Some(Up), 25) ; (Some(Up), 5)]
