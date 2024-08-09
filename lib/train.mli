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

