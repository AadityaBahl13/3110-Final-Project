type t
type key
type label

val read_from_csv : string -> t
val count_labels : t -> int * int
val to_list : t -> (key * label) list
val get_key : key -> int list
val label_to_string : key * label -> string
