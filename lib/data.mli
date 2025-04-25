open Bogue

type t
(** [type t] represents a collection of feature vectors and their associated
    label *)

type key
(** [type key] is the type of the label *)

type label
(** [type label] is the type of the feature vector *)

val read_from_csv : string -> t
(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)

val count_labels : t -> int * int
(** [count_labels t] the number of positive and negative labels in [t] *)

val to_list : t -> (key * label) list
(** [to_list t] converts the contents of [t] to a list *)

val get_key : key -> int list
(** [get_key key] converts [key] to [type int list] *)

val label_to_string : key * label -> string
(** [label_to_string (key, label)] converts [(key,label)] to [type string] *)

val color_of_label : label -> Draw.rgb
(** [color_of_label label] returns the associated color for each label*)
