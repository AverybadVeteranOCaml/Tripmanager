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

