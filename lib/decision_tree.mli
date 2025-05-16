type tree
(** [type tree] represents a node of a decision tree that is not a leaf, and
    splits the data at this node via some function of type [split] *)

type t =
  | Leaf of Data.label
  | Tree of tree
      (** [type t] is a decision tree that predicts a binary label for a given
          piece of data based on a series of splits for specific values along
          the various dimensions of the training data *)

type split = Lin_alg.t -> bool
(** [type split] is a function that splits a piece of data based on the value it
    has at a specific dimension *)

val init_decision_tree : Data.t -> int -> t
(** [init_decision_tree data n] is a decision tree with max depth [n] and
    training data [data] *)

val train : t -> unit
(** [train tree] trains decision tree [tree] until achieving 0 entropy or
    reaching the maximum depth *)

val predict : t -> Lin_alg.t -> Data.label
(** [predict tree tensor] predicts a label for [tensor] based on the splits in
    [tree] *)

val split : t -> unit
(** [split tree] updates [tree] so that it splits the data optimally *)

val find_split : t -> split * int option * int option
(** [find_split tree] is the function that splits the data along the optimal
    dimension with the optimal split value to yeild the greatest possible
    reduction in entropy *)

val entropy : Data.t -> int -> split -> float
(** [entropy data size split] is the entropy of splitting the data in [data]
    that has size [size] with function [split] *)

val get_left : tree -> t
(** [get_left tree] is the left child of the node [tree] *)

val get_right : tree -> t
(** [get_left tree] is the right child of the node [tree] *)

val get_split_dim_and_val : tree -> int * int
(** [get_split_dim_and_val tree] is the dimension and the value that [tree]
    splits its data upon *)
