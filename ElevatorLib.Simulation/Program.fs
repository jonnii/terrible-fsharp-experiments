open System
open ElevatorLib.Types
open ElevatorLib.Banks
open ElevatorLib.Simulator

[<EntryPoint>]
let main argv =
    let numFloors = 100
    let numElevators = 3
    let random = new Random(3000)
    let steps = 3000
    
    let timeSeries = seq { for i in 1 .. steps -> i }
    let rides = generateRides numFloors random timeSeries
    
    let bank = createBank numElevators
    let occupancies = List.init numElevators (fun _ -> createOccupancy)
    
    let occupiedCars = List.zip occupancies bank
    
    let initialState = {
        occupiedCars = occupiedCars 
        standing = List.empty
        transiting = List.empty
        arrived = List.empty
    }
    
    let (states, final) =
        Seq.zip timeSeries rides
        |> Seq.mapFold (simulate findFirstIdleCareOrWithFewestDestinations) initialState
    
    let calculateTravelTime (waited, traveled, arrived) =
        float(arrived.arrivalTime - waited.startingTime)

    let avg = final.arrived |> List.map calculateTravelTime |> List.average

    printf "idle or fewest destinations %O rides completed in %f seconds\n" final.arrived.Length avg

    let (states, final) =
        Seq.zip timeSeries rides
        |> Seq.mapFold (simulate findClosestCarByJourneyTime) initialState
    
    let calculateTravelTime (waited, traveled, arrived) =
        float(arrived.arrivalTime - waited.startingTime)

    let avg = final.arrived |> List.map calculateTravelTime |> List.average

    printf "closest car by journey time %O rides completed in %f seconds\n" final.arrived.Length avg
    
    let timeByCar =
        final.arrived
        |> List.groupBy (fun (_, t, _) -> t.carId)
        |> List.map (fun (_, list) -> list |> List.map calculateTravelTime |> List.average)

    printf "%O\n" timeByCar

    0