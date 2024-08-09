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

