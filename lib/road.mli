module type Road =  sig
    type segment = {
      start_station : string;
      end_station : string;
      departure_time : string;
      arrival_time : string;
      date : string;
      arrival_date : string;
    }
    type t
    val create : string list -> string -> string -> string -> t
    val get_segments : t -> segment list
    val segment_toString : segment list -> string
    val are_segments_similar : segment list -> segment list -> bool
  end

module Road : Road
