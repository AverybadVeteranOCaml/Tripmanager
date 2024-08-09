
let tgvCitiesConnectivities = [
    ("Paris", ["Lyon"; "Lille"; "Nancy"; "Le Havre"; "Bordeaux"; "Strasbourg"; "Le Mans"]);
    ("Brest", ["Rennes"]);
    ("Le Havre", ["Paris"]);
    ("Lille", ["Paris"]);
    ("Strasbourg", ["Paris"; "Nancy"; "Dijon"]);
    ("Nancy", ["Paris"; "Dijon"; "Strasbourg"]);
    ("Dijon", ["Nancy"; "Paris"; "Strasbourg"]);
    ("Lyon", ["Paris"; "Dijon"; "Marseille"]);
    ("Marseille", ["Lyon"; "Montpellier"]);
    ("Montpellier", ["Marseille"; "Toulouse"]);
    ("Bordeaux", ["Toulouse"; "Paris"]);
    ("Nantes", ["Le Mans"]);
    ("Rennes", ["Brest"; "Le Mans"]);
    ("Toulouse", ["Bordeaux"; "Montpellier"]);
    ("Le Mans", ["Rennes"; "Nantes"; "Paris"]);
]

let thalysConnectivities = [
    ("Paris", ["Lille"]);
    ("Lille", ["Paris"; "Brussels"]);
    ("Liege", ["Cologne"]);
    ("Brussels", ["Amsterdam"; "Lille"]);
    ("Amsterdam", ["Lille"; "Brussels"]);
    ("Cologne", ["Liege"; "Essen"]);
    ("Essen", ["Cologne"]);
]

let eurostarConnectivities = [
    ("Paris", ["Lille"]);
    ("Lille", ["Paris"; "London"; "Brussels"]);
    ("London", ["Lille"]);
    ("Brussels", ["Lille"]);
]

let checkCmdName = function
    | "create" -> 1
    | "delete" -> 2
    | "list" -> 3
    | "quit" -> 4
    | _ -> -1

let checkTrainNamme = function
    | "TGV" -> true
    | "Thalys" -> true
    | "Eurostar" -> true
    | _ -> false

let isNumericChar c = c >= '0' && c <= '9'

let isValidDayNumber str = int_of_string str > 0 && int_of_string str < 32

let isValidMonthNumber str = int_of_string str > 0 && int_of_string str < 13

let isValidYearNumber str = int_of_string str  >= 2024

(* Send all the trip's stops to this function, cause a trip assumes we only use one kind of train *)
let rec isGoodTrainNetwork network trainStop = match network with
    | (station :: tl) -> if station = trainStop 
        then
            true
        else
            isGoodTrainNetwork tl trainStop
    | [] -> false

(* Ex: 10-02-2017 *)
let checkDateFormat str = if (String.length str) != 10 then raise (Invalid_argument "Bad format date") else
    let rec aux = function
        | 0 -> isNumericChar (String.get str 0) && aux 1
        | 1 -> isNumericChar (String.get str 1) && isValidDayNumber (String.sub str 0 2) && aux 2
        | 2 -> (String.get str 2) = '-' && aux 3
        | 3 -> isNumericChar (String.get str 3) && aux 4
        | 4 -> isNumericChar (String.get str 4) && isValidMonthNumber (String.sub str 3 2) && aux 5
        | 5 -> (String.get str 5) = '-' && aux 6
        | 6 -> isNumericChar (String.get str 6) && aux 7
        | 7 -> isNumericChar (String.get str 7) && aux 8
        | 8 -> isNumericChar (String.get str 8) && aux 9
        | 9 -> isNumericChar (String.get str 9) && isValidYearNumber (String.sub str 6 4) && aux 10;
        | 10 -> true
        | _ -> raise (Invalid_argument "Bad format date")
    in aux 0

(* 12:12 *)
let checkHourFormat str = if (String.length str) != 5 then raise (Invalid_argument "Bad format hour") else
    let rec aux = function
        | 0 -> ((String.get str 0) >= '0' && (String.get str 0) <= '2') && aux 1
        | 1 -> ((String.get str 1) >= '0' && (String.get str 1) <= '9') && aux 2
        | 2 -> (String.get str 2) = ':' && aux 3
        | 3 -> ((String.get str 3) >= '0' && (String.get str 3) <= '5') && aux 4
        | 4 -> ((String.get str 4) >= '0' && (String.get str 4) <= '9') && aux 5
        | 5 -> true
        | _ -> raise (Invalid_argument "Bad format hour")
    in aux 0

let checkDestination str =
    let validCities = ["Paris"; "Dijon"; "Brest"; "Lille"; "Liege"; "Le Mans"; "Cologne"; "Lyon"; "Brussels"; "Rennes"; "Le Mans"; "Nancy"; "Toulouse"; "Montpellier"; "Marseille";
                        "London"; "Essen"; "Le Havre"; "Nantes"; "Bordeaux"; "Strasbourg"; "Amsterdam"] in
        let splitStr = (String.split_on_char ',' str) in
            let rec aux listL it = match it with
                | it when it = listL -> true
                | it when it < listL -> if (List.mem (List.nth splitStr it) validCities) = true then (aux listL (it+1)) else raise (Invalid_argument "City isn't part of the network.")
                | _ -> raise (Invalid_argument "City isn't part of the network. (Should no throw this)")
            in aux (List.length splitStr) 0

let check_cmd allTokens =
    if List.length allTokens < 1
        then
            -1
        else
            checkCmdName (List.nth allTokens 0)

(* BFS trough train stations lists *)
let searchNodes currentNodes arrival breadth currentRoad network allRoads =
    let rec aux currentNodes arrival breadth currentRoad visited allRoads = match currentNodes with
        | hd :: _ when hd = arrival  -> (((List.rev ( hd :: currentRoad )), breadth) :: allRoads)
        | hd :: tl                   -> if (List.mem hd visited)
                then
                    aux tl arrival breadth currentRoad visited allRoads
                else
                    aux tl arrival breadth currentRoad (hd :: visited)
                     (aux (try List.assoc hd network with Not_found -> [] ) arrival (breadth + 1 ) (hd :: currentRoad) (hd::visited) allRoads)
        | [] -> allRoads
    in aux currentNodes arrival breadth currentRoad [] allRoads

let get_lhs_from_pair (a, _) = a

let solveRoad stations network =
    let rec aux current next acc = match next with
        | arrivalStation :: tl -> let allRoads = (searchNodes (List.assoc current network) arrivalStation 0 (current :: []) network []) in (* besoin de gestion d'erreur si il n'y a pas de routes. *)
                let sortedConnections = (List.sort (fun (_, dist1) (_, dist2) -> compare dist1 dist2) allRoads) in
            aux arrivalStation tl ((List.rev (get_lhs_from_pair (List.hd sortedConnections))) :: acc)
        | _ -> List.rev (List.flatten acc)
    in aux (List.hd stations) (List.tl stations) []

module TrainImpl = Trainmanager.Trip.Train
module RoadImpl = Trainmanager.Trip.Road
module Journey = Trainmanager.Trip.MakeJourney(TrainImpl)(RoadImpl)

module TripManager = struct
  let allTrips = ref []

  let add_trip trip =
    allTrips := trip :: !allTrips

  let remove_trip args =
      let rec aux trip = match trip with
    | hd :: _ when (Trainmanager.Trip.Train.get_model (Journey.get_train hd)) ^ string_of_int (Journey.get_id hd)  = (List.nth args 1) ^ (List.nth args 2) ->
      begin
        allTrips := List.filter (fun x -> (Trainmanager.Trip.Train.get_model (Journey.get_train x)) ^  string_of_int (Journey.get_id hd) <> (List.nth args 1) ^ (List.nth args 2) ) !allTrips;
        print_endline "Deleted";
        ()
      end
    | _ :: tl -> aux tl
    | [] -> ()
  in aux !allTrips

  let get_all_trips () =
    !allTrips
end

let print_error msg = Printf.printf "create_trip : %s\n" msg

let createTrain allTokens =
    let rec aux = function
        | 1 -> if (checkTrainNamme (List.nth allTokens 1)) = true then aux 2 else raise (Invalid_argument "Train name not recognized")
        | 2 -> if (checkDateFormat (List.nth allTokens 2)) = true then aux 3 else raise (Invalid_argument "Date  format is : dd/mm/yyyy")
        | 3 -> if (checkHourFormat (List.nth allTokens 3)) = true then aux 4 else raise (Invalid_argument "Hour format is : hh:mm:ss")
        | 4 -> if (checkDestination (List.nth allTokens 4)) = true then true else raise (Invalid_argument "Destination format is CityA,CityB,CityC...")
        | _ -> false
    in aux 1

let getConnectivity trainType = match trainType with
    | "TGV" -> tgvCitiesConnectivities
    | "Thalys" -> thalysConnectivities
    | "Eurostar" -> eurostarConnectivities
    | _ -> raise (Invalid_argument "Non existing network")

let tripCollision newTrip =
    let rec aux trips tripSegment = match trips with
        | hd :: tl -> let itSegment =  Trainmanager.Trip.Road.get_segments (Journey.get_route hd) in
            if (Trainmanager.Trip.Road.are_segments_similar itSegment tripSegment)
                then
                    begin
                        Printf.printf "Trip not created: conflict with %s %d\n" (Trainmanager.Trip.Train.get_model (Journey.get_train hd)) (Journey.get_id hd);
                        raise (Invalid_argument "Trip conflict")
                    end
                else
                    aux tl tripSegment
        | [] -> false
    in aux (TripManager.get_all_trips ()) (Trainmanager.Trip.Road.get_segments (Journey.get_route newTrip))

let get_list_of_trip_ids trips =
    let rec aux trip acc = match trip with
        | hd :: tl -> aux tl ((Journey.get_id hd) :: acc)
        | [] -> acc
    in aux trips []

let create_trip args =
    try
        let trainType = List.nth args 1 in
        let tripDate = List.nth args 2 in
        let tripHour = List.nth args 3 in
        let tripStations =  (String.split_on_char ','  (List.nth args 4)) in
            let trainSpeed = Trainmanager.Trip.Train.getTrainSpeed trainType in
                    let stations = if (Trainmanager.Trip.Train.check_station_network trainType tripStations) = true then tripStations else [] in
                        let trainNetwork = getConnectivity trainType in
                            let connections = solveRoad stations trainNetwork in
                                let exampleTrain = Trainmanager.Trip.Train.create trainType trainSpeed in
                                   let exampleRoad = Trainmanager.Trip.Road.create connections tripDate tripHour trainType in
                                        let listOfTripIds = get_list_of_trip_ids (TripManager.get_all_trips ()) in
                                       let newJourney = Journey.create exampleTrain exampleRoad listOfTripIds in
                                            if (tripCollision newJourney) = false then
                                                    begin
                                                        TripManager.add_trip newJourney;
                                                        Printf.printf "Trip created: %s %s\n" (Trainmanager.Trip.Train.get_model exampleTrain) (Journey.to_string(newJourney))
                                                    end
                                            else
                                                ()
    with
        | Trainmanager.Trip.Train.Unknow_train_type str      -> print_error str ; raise (Trainmanager.Trip.Train.Unknow_train_type str)
        | Trainmanager.Trip.Train.Station_not_in_network str -> print_error str ; raise (Trainmanager.Trip.Train.Station_not_in_network str)
        | Invalid_argument str -> print_error str ; raise (Invalid_argument str)
        | _ -> raise (Invalid_argument "Wrong args")

let delete_journey args = TripManager.remove_trip args

let list_journeys () =
    let rec aux_journeys journey_list = match journey_list with
        | hd :: tl -> 
            let train_model = Trainmanager.Trip.Train.get_model (Journey.get_train hd) in
            let journey_id = Journey.get_id hd in
            let trip_segments = Trainmanager.Trip.Road.get_segments (Journey.get_route hd) in
            let stostr = Trainmanager.Trip.Road.segment_toString trip_segments in
            let journey_str = Printf.sprintf "%s %d\n%s\n" train_model journey_id stostr in
            Printf.printf "%s" journey_str;
            aux_journeys tl
        | [] -> ()
    in 
    aux_journeys (TripManager.get_all_trips ())

let cmdLauncher cmdId args = match cmdId with
    | 1 -> if (createTrain args) = true 
                then
                    try
                        create_trip args 
                    with
                        | _ -> ()
            else raise (Invalid_argument "create trip args error")
    | 2 -> delete_journey args
    | 3 -> list_journeys ()
    | 4 -> exit 0 
    | -1 -> print_endline "Unknown cmd, available : create, delete, list, quit"
    | _  -> print_endline "Unknown cmd, available : create, delete, list, quit"

let parse_stdin = function
    | "" -> ()
    | str -> try
            let splitInput = (String.split_on_char ' ' str) in
                let cmdId = check_cmd splitInput in
                    cmdLauncher cmdId splitInput
        with
            | Invalid_argument str -> print_endline str
            | _ -> print_endline "Error"

let main () =
    let rec aux () =
        try
            let stdin = read_line() in
                parse_stdin stdin;
            aux ()
        with
            | End_of_file -> ()
            | Not_found -> print_endline "Not found!"
            | _ -> aux ()
    in aux ()

let () = main()

