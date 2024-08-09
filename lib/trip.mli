open Train

module MakeJourney :
  functor (TrainImpl : Train) (RoadImpl : Road.Road) ->
    sig
      type t = { train : TrainImpl.t; route : RoadImpl.t; id : int }
      val create : TrainImpl.t -> RoadImpl.t -> int list -> t
      val get_id : t -> int
      val get_route : t -> RoadImpl.t
      val get_train : t -> TrainImpl.t
      val to_string : t -> string
    end
