type t
(** [type t] represents a collection of feature vectors and their associated
    label *)

type key = Lin_alg.t
(** [type key] is the type of the label *)

type label
(** [type label] is the type of the feature vector *)

val read_from_csv : string -> t
(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)

val count_labels : t -> int * int
(** [count_labels t] the number of positive and negative labels in [t] *)

val table_to_list : t -> (key * label) list
(** [to_list t] converts the contents of [t] to a list *)

val label_to_string : key * label -> string
(** [label_to_string (key, label)] converts [(key,label)] to [type string] *)
