type t

val init : Data.t -> int -> t
val train : t -> unit
val predict : t -> Lin_alg.t -> Data.label
val step : t -> Lin_alg.t -> Data.label -> bool
val get_weight : t -> Lin_alg.t
val get_bias : t -> int
