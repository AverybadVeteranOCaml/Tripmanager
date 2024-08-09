module type Train = sig
  type t
  exception Unknow_train_type of string
  exception Station_not_in_network of string
  val create : string -> float -> t
  val get_model : t -> string
  val get_speed : t -> float
  val getTrainSpeed : string -> float
  val check_station_network : string -> string list -> bool
  val get_associated_network : string -> string list
end

module Train : Train = struct
  type t = { model : string; speed : float }
  exception Unknow_train_type of string
  exception Station_not_in_network of string
  let trainNetworks = [
    ("TGV", ["Paris" ; "Brest" ;"Le Havre"; "Lille"; "Strasbourg"; "Nancy"; "Dijon"; "Lyon"; "Marseille"; "Bordeaux"; "Nantes"; "Rennes"; "Toulouse"; "Le Mans" ]);
    ("Thalys", ["Paris" ; "Lille" ;"Liege"; "Brussels"; "Amsterdam"; "Cologne"; "Essen" ]);
    ("Eurostar", ["Paris" ; "Lille" ;"London"; "Brussels" ]);
  ]
  let create model speed = { model; speed }
  let get_model train = train.model
  let get_speed train = train.speed
  let getTrainSpeed trainType = match trainType with
    | "TGV"         -> 230.0
    | "Thalys"      -> 210.0
    | "Eurostar"    -> 160.0
    | _ -> raise (Unknow_train_type "Unknown train type needs to be : TGV, Thalys or Eurostar")

  let check_station_network trainType trainStations =
    let rec aux stations associatedTrainNetwork = match stations with
        | hd :: tl -> if (List.mem hd associatedTrainNetwork)
            then aux tl associatedTrainNetwork
            else 
                raise (Station_not_in_network "Station out of train's network")
        | [] -> true
    in aux trainStations (List.assoc trainType trainNetworks)

  let get_associated_network trainType = List.assoc trainType trainNetworks

end

module type Road = sig
  type segment = {
      start_station : string;
      end_station : string;
      departure_time : string;
      arrival_time : string;
      date : string;
      arrival_date : string
  }
  type t
  val create : string list -> string -> string -> string -> t
  val get_segments : t -> segment list
  val segment_toString : segment list -> string
  val are_segments_similar : segment list -> segment list -> bool

end

module Road : Road = struct
  type segment = {
      start_station : string;
      end_station : string;
      departure_time : string;
      arrival_time : string;
      date : string;
      arrival_date : string;
  }
  type t = {
        segments : segment list;
        trip_date : string;
        trip_hour : string;
  }

let tripDistances = [
    (("Paris", "Lyon") , 427);
    (("Dijon", "Lyon") , 192);
    (("Paris", "Lille"), 225);
    (("Paris", "Nancy"), 327);
    (("Dijon", "Nancy"), 226);
    (("Brest", "Rennes"), 248);
    (("Lille", "London"), 269);
    (("Liege", "Cologne"), 118);
    (("Le Mans", "Paris"), 201);
    (("Cologne", "Essen"), 81);
    (("Lyon", "Marseille"), 325);
    (("Brussels", "Liege"), 104);
    (("Paris", "Le Havre"), 230);
    (("Rennes", "Le Mans"), 163);
    (("Le Mans", "Nantes"), 183);
    (("Paris", "Bordeaux"), 568);
    (("Lille", "Brussels"), 106);
    (("Nancy", "Strasbourg"), 149);
    (("Paris", "Strasbourg"), 449);
    (("Dijon", "Strasbourg"), 309);
    (("Toulouse", "Bordeaux"), 256);
    (("Brussels", "Amsterdam"), 211);
    (("Montpellier", "Toulouse"), 248);
    (("Marseille", "Montpellier"), 176);
]

(* get the distances between a pair of station, tries both order (a,b) (b,a) *)
let getStationDistances start arrival =
    try List.assoc (start, arrival) tripDistances with Not_found -> try List.assoc (arrival, start) tripDistances with Not_found -> -1

let get_arrival_time startTime distance trainType =
    let stSplit = String.split_on_char ':' startTime in
    let startTimeHourPart = List.nth stSplit 0 in
    let startTimeMinutePart = List.nth stSplit 1 in
    let startTimeInMinutes = ((int_of_string startTimeHourPart) * 60) + (int_of_string startTimeMinutePart) in
    let durationInMinutes = (float_of_int distance /. (Train.getTrainSpeed trainType)) *. 60.0 in
    let integerDurationInMinutes = int_of_float (Float.floor durationInMinutes) in
    let arrivalTimeInMinutes = startTimeInMinutes + integerDurationInMinutes in
    let arrivalTimeHoursPart = (arrivalTimeInMinutes / 60) mod 24 in
    let arrivalTimeMinutesPart = arrivalTimeInMinutes mod 60 in
    string_of_int arrivalTimeHoursPart ^ ":" ^ string_of_int arrivalTimeMinutesPart

let add_minutes currentHour additionalMinutes =
    let chSplit = String.split_on_char ':' currentHour in
    let startTimeHourPart = List.nth chSplit 0 in
    let startTimeMinutePart = List.nth chSplit 1 in
    let startTimeInMinutes = ((int_of_string startTimeHourPart) * 60) + (int_of_string startTimeMinutePart) in
    let arrivalTimeInMinutes = startTimeInMinutes + additionalMinutes in
    let arrivalTimeHoursPart = (arrivalTimeInMinutes / 60) mod 24 in
    let arrivalTimeMinutesPart = arrivalTimeInMinutes mod 60 in
    string_of_int arrivalTimeHoursPart ^ ":" ^ string_of_int arrivalTimeMinutesPart

let crossedADay startTime arrivalTime =
    let hoursSt = String.split_on_char ':' startTime in
        let hoursAt = String.split_on_char ':' arrivalTime  in
            if (int_of_string (List.nth hoursSt 0)) <= 23 && (int_of_string (List.nth hoursAt 0)) <= (int_of_string (List.nth hoursSt 0)) then true else false

  let makeSegments stations hour trainType departureDate =
    let rec aux dep arr startTime acc = match dep , arr with
        | dep , arrHd :: arrTl when dep = arrHd -> aux arrHd arrTl startTime acc
        | dep , arrHd :: arrTl when dep != arrHd ->
            let arrTime = get_arrival_time startTime  (getStationDistances dep arrHd) trainType in
                if (crossedADay startTime arrTime) = false then
                    aux arrHd arrTl (add_minutes  arrTime 10) ({start_station = dep ; end_station = arrHd ;
                                    departure_time = startTime ; arrival_time = arrTime ;
                                    date = departureDate ; arrival_date = departureDate} :: acc)
                else
                    let boxedDate = Date.boxRawDate departureDate in
                        let boxedNextDayDate = Date.next boxedDate in
                            let nextDayDate = Date.unboxDate boxedNextDayDate in
                        aux arrHd arrTl (add_minutes  arrTime 10) ({start_station = dep ; end_station = arrHd ;
                                        departure_time = startTime ; arrival_time = arrTime ; date = departureDate ;
                                        arrival_date = nextDayDate } :: acc)
        | _ -> List.rev acc
    in aux (List.hd stations) (List.tl stations) hour []

  let create stations date hour trainType =
    let tripSegments = makeSegments stations hour trainType date in
        { segments = tripSegments ; trip_date = date ; trip_hour = hour }

  let get_segments route = route.segments

  let segment_toString seg_list =
    let rec aux_segments seg_list it acc = match seg_list with
        | hd :: tl when it = 0 && (List.length seg_list = 1) ->
            let segment_str = Printf.sprintf "%s (,) (%s,%s)\n%s (%s,%s) (,)" 
                hd.start_station hd.date hd.departure_time
                hd.end_station hd.arrival_date (add_minutes hd.arrival_time (-10)) in
            aux_segments tl (it + 1) (acc ^ segment_str)
        | hd :: tl when it = 0 && (List.length seg_list > 1) ->
            let segment_str = Printf.sprintf "%s (,) (%s,%s)\n" 
                hd.start_station hd.date hd.departure_time in
            aux_segments tl (it + 1) (acc ^ segment_str)
        | hd :: tl when tl = [] ->
            let segment_str = Printf.sprintf "%s (%s,%s) (%s,%s)\n%s (%s,%s) (,)" 
                hd.start_station hd.date (add_minutes hd.departure_time (-10)) hd.arrival_date hd.departure_time 
                hd.end_station hd.arrival_date (add_minutes hd.arrival_time (-10)) in
            aux_segments tl (it + 1) (acc ^ segment_str)
        | hd :: tl ->
            let segment_str = Printf.sprintf "%s (%s,%s) (%s,%s)\n" 
                hd.start_station hd.date (add_minutes hd.departure_time (-10)) hd.arrival_date hd.departure_time in
            aux_segments tl (it + 1) (acc ^ segment_str)
        | [] -> acc
    in aux_segments seg_list 0 ""

  let time_to_minutes time =
    let splitTime = String.split_on_char ':' time in
        let hourPart = int_of_string (List.nth splitTime 0) in
            let minutePart = int_of_string (List.nth splitTime 1) in
                (hourPart * 60) + minutePart

  let check_overlap existingStarTime existingEndTime newStartTime newEndTime =
    let estMinutes = time_to_minutes existingStarTime in
    let eetMinutes = time_to_minutes existingEndTime in
    let nstMinutes = time_to_minutes newStartTime in
    let netMinutes = time_to_minutes newEndTime in
        (nstMinutes >= estMinutes && nstMinutes <= eetMinutes) || (netMinutes >= estMinutes && netMinutes <= eetMinutes)

  let are_segments_similar lhs rhs =
    let rec aux validSegments newSegments = match validSegments , newSegments with
        | hdv :: _ , hdn :: tln -> 
                if (hdv.start_station = hdn.start_station) && (hdv.end_station = hdn.end_station) && (hdv.arrival_date) = (hdn.date)
                    then
                        if (check_overlap hdv.departure_time hdv.arrival_time hdn.departure_time hdn.arrival_time) = true
                            then
                                true
                            else
                                aux (List.tl validSegments) (hdn :: tln)
                else
                    aux (List.tl validSegments) (hdn :: tln)
        | [] , _ :: tln -> aux lhs tln
        | _ , [] -> false
    in aux lhs rhs

end

module MakeJourney = functor (TrainImpl : Train) (RoadImpl : Road) -> struct
  type t = { train : TrainImpl.t; route : RoadImpl.t ; id : int}

  let create train route tripIds =
    let rec generate_unique_id () =
      let id = 1000 + Random.int 9000 in
      if List.mem id tripIds then
        generate_unique_id ()
      else
        id
    in
    let unique_id = generate_unique_id () in
    { train; route; id = unique_id }

  let get_id journey = journey.id
  let get_route journey = journey.route
  let get_train journey = journey.train
  let to_string journey = Printf.sprintf "%d" journey.id

end

