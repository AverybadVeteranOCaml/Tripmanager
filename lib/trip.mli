module type Train =
  sig
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

module Train : Train

module type Road =
  sig
    type t
    type segment = {
        start_station : string;
        end_station : string;
        departure_time : string;
        arrival_time : string;
        date : string;
        arrival_date : string;
    }
    val create : string list -> string -> string -> string -> t
    val get_segments : t -> segment list
    val segment_toString : segment list -> string
    val are_segments_similar : segment list -> segment list -> bool
  end

module Road : Road

module MakeJourney :
  functor (TrainImpl : Train) (RoadImpl : Road) ->
    sig
      type t = { train : TrainImpl.t; route : RoadImpl.t; id : int }
      val create : TrainImpl.t -> RoadImpl.t -> int list -> t
      val get_id : t -> int
      val get_route : t -> RoadImpl.t
      val get_train : t -> TrainImpl.t
      val to_string : t -> string
    end
