open Bogue

type t
(** [type t] represents a collection of feature vectors and their associated
    label *)

type tensor = Lin_alg.t
(** [type tensor] is the type of a vector in the data set. *)

type label
(** [type label] is the type of the feature vector *)

val positive : label
(** [positive] is a label of +1 *)

val negative : label
(** [positive] is a label of -1 *)

val int_of_label : label -> int
(** [int_of_label label] is -1 if [label = negative] and +1 if
    [label = positive] *)

val read_from_csv : string -> t
(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)

val count_labels : t -> int * int
(** [count_labels t] the number of positive and negative labels in [t] *)

val data_to_list : t -> (tensor * label) list
(** [to_list t] converts the contents of [t] to a list *)

val list_to_data : (tensor * label) list -> t
(** [list_to_data data] is a list representation of the data in [data] *)

val get_dimension : t -> int
(** [get_dimension data] is the dimension of the feature vectors in the data set
    [data] *)

val get_data_set : t -> (tensor, label) Hashtbl.t
(** [get_data_set data] is hashtable that maps the feature vectors in [data] to
    their corresponding label *)

val label_to_string : tensor * label -> string
(** [label_to_string (key, label)] converts [(key,label)] to [type string] *)

val color_of_label : label -> Draw.rgb
(** [color_of_label label] returns the associated color for each label*)

val init_data : t
(** [init_data] is an empty data set *)

val filter : t -> (tensor -> bool) -> t
(** [filter data p] returns the subset of [data] whose feature vectors satisfy
    the predicate [p]. All other examples are dropped. *)
