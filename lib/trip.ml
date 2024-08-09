open Train
open Road

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

