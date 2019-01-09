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
    
    let initialState = {
        bank = bank
        occupancies = occupancies
        people = List.empty
    }
    
    let (states, final) =
        Seq.zip timeSeries rides
        |> Seq.mapFold simulate initialState

//    states
//    |> Seq.toList
//    |> List.map (fun state -> printf "%O\n" (state |> getFloors))
//    |> ignore

    printf "%O\n" (final.bank |> getFloors)

    // we should have none of these    
    printf "%O\n" final.occupancies
    
    // everyone should have transited
//    printf "%O\n" final.floors
//
//        
    printf "Scheduled Rides\n"
    let scheduledRides = rides |> List.collect id
    
    let finishedRides =
        final.people
    
    let calculateTravelTime person =
        match person with 
        | Arrived(waited, traveled, arrived) -> 
            float(arrived.arrivalTime - waited.startingTime)
        | _ -> failwith "not supported"

    let avg = final.people |> List.map calculateTravelTime |> List.average

    printf "%O of %O rides completed in %f seconds\n" finishedRides.Length scheduledRides.Length avg

    for person in final.people do
        printf "%O\n" person
        
    0