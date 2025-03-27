type t
type label

val read_from_csv : string -> t
val count_labels : t -> int * int
