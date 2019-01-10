module Simulator

    open FsUnit
    open Xunit
    open ElevatorLib.Types
    open ElevatorLib.Cars
    open ElevatorLib.Banks
    open ElevatorLib.Simulator
    open FsCheck
    
    [<Fact>]    
    let ``transiting an already arrived `` () =
//        let occupiedCar = ({ capacity = 10 ; occupancy = 0 }, Idleing)
//        
//        let ride = Arrived({ startingFloor = 10 ; destinationFloor = 15 ; startingTime = 5 }, { boardingTime = 10 ; carId = 5 }, { arrivalTime = 20 })
//        let (_, transit) = transit occupiedCar ride
//        ride |> should equal transit

//        let standingStateGenerator = Arb.generate<StandingState> 
//        let k = Gen.sample 10 standingStateGenerator
//        
//        Check.Quick

        ignore