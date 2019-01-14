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
        
        let bank = bank |> dispatchWithStrategy findFirstIdleCareOrWithFewestDestinations (Call (Up, 0))
        let bank = bank |> List.map tick
        
        bank |> getFloors |> should equal [0; 0; 0]
        bank |> getDoors |> should equal [Open; Closed; Closed]

    [<Fact>]
    let ``calling multiple elevators`` () =
        let bank = createBank 3
        
        let dispatch = dispatchWithStrategy findFirstIdleCareOrWithFewestDestinations
        
        let bank = bank |> dispatch (Call (Up, 5)) 
        let bank = bank |> dispatch (Call (Down, 10))
        let bank = bank |> dispatch (Call (Down, 10))
        
        let bank = bank |> repeat (List.map tick) 30
        
        bank |> getFloors |> should equal [5; 10; 10]
        bank |> getDoors |> should equal [Closed; Closed; Closed]