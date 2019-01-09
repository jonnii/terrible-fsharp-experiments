module Banks

    open FsUnit
    open Xunit
    open ElevatorLib.Types
    open ElevatorLib.Cars
    open ElevatorLib.Banks
    
    let moveToFloor floor car =
        match car with
        | Idleing(state) ->
            Idleing({ state with floor = floor })
        | _ -> failwith "can only explicitely move an idleing car"
    
    let repeat f n =
        Seq.init n (fun _ u -> f u)
            |> Seq.reduce (>>)

    [<Fact>]    
    let ``creating a bank`` () =
        let bank = createBank 3
        bank.Length |> should equal 3
    
    [<Fact>]
    let ``calling the elevator from the lobby`` () =
        let bank = createBank 3
        bank |> getFloors |> should equal [0; 0; 0]
        bank |> getDoors |> should equal [Closed; Closed; Closed]
        
        let bank = bank |> dispatch (Call (Up, 0))
        let bank = bank |> List.map tick
        
        bank |> getFloors |> should equal [0; 0; 0]
        bank |> getDoors |> should equal [Open; Closed; Closed]

    [<Fact>]
    let ``calling multiple elevators`` () =
        let bank = createBank 3
        
        let dispatch = dispatchWithStrategy findFirstIdleCar
        
        let bank = bank |> dispatch (Call (Up, 5)) 
        let bank = bank |> dispatch (Call (Down, 10))
        let bank = bank |> dispatch (Call (Down, 10))
        
        let bank = bank |> repeat (List.map tick) 30
        
        bank |> getFloors |> should equal [5; 10; 10]
        bank |> getDoors |> should equal [Closed; Closed; Closed]
     
////    [<Fact>]
////    let ``calculating time to get to destination when idle`` () =
////        let car = createCar 0
////        car |> timeToFloor 3 |> should equal 3
////        car |> moveToFloor 10 |> timeToFloor 3 |> should equal 7
////    
////    [<Fact>]
////    let ``calculating time to get to destination when waiting`` () =
////        let car = createCar 0
////        {car with State = Waiting(Up, 30) } |> timeToFloor 10 |> should equal 40
//
////    [<Fact>]
////    let ``calculating time to get to destination not on current journey`` () =
////        let car = createCar 0
////                    |> send (Call (Down, 10))
////                    |> send (Call (Down, 5))
////                    |> send (Call (Down, 8))
////
////        // cumulative travel time and waits                      
////        car |> timeToFloor 0 |> should equal (20 + 5 + 5 + 5)
////
////    [<Fact>]
////    let ``calculating time to get to a floor which is on current journey`` () =
////        let car = createCar 0
////                    |> send (Call (Up, 10))
////                    |> send (Call (Up, 5))
////                    |> send (Call (Up, 8))
////
////        car |> timeToFloor 7 |> should equal (7 + 5)
////        
////    [<Fact>]
////    let ``calculating time to get to a floor which is on current journey going down`` () =
////        let car = createCar 0
////                    |> moveToFloor 20
////                    |> send (Call (Down, 10))
////                    |> send (Call (Down, 5))
////                    |> send (Call (Down, 8))
////
////        car |> isOnCurrentJourney 4 |> should equal false
////        car |> timeToFloor 4 |> should equal (5 + 5 + 5 + 16)
////        