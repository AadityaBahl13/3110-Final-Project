open Bogue

type t
(** [type t] represents a collection of feature vectors and their associated
    label *)

type tensor = Lin_alg.t
(** [type key] is the type of the label *)

type label
(** [type label] is the type of the feature vector *)

val positive : label
val negative : label
val int_of_label : label -> int

val read_from_csv : string -> t
(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)

val count_labels : t -> int * int
(** [count_labels t] the number of positive and negative labels in [t] *)

val data_to_list : t -> (tensor * label) list
(** [to_list t] converts the contents of [t] to a list *)

val get_dimension : t -> int
val get_data_set : t -> (tensor, label) Hashtbl.t

val label_to_string : tensor * label -> string
(** [label_to_string (key, label)] converts [(key,label)] to [type string] *)

val color_of_label : label -> Draw.rgb
(** [color_of_label label] returns the associated color for each label*)

val init_data : t

(** [filter data p] returns the subset of [data] whose feature vectors satisfy
    the predicate [p].  All other examples are dropped. *)
val filter : t -> (tensor -> bool) -> t
