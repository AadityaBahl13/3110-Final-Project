type t
(** [type t] represents a tensor of up to two dimensions *)

exception InvalidDimensions
exception OutOfBound

val create : int list list -> t
(** [create lst] is a tensor of [type t] with the same dimensions as [lst] and
    the same values as those in [lst] *)

val add : t -> t -> t
(** [add t1 t2] is the tensor whose values in a given position are the sum of
    the values of [t1] and [t2] in that same position.

    Raises: InvalidDimension if [t1] and [t2] are not of the same dimension *)

val dot : t -> t -> t
(** [dot t1 t2] is the tensor whose values is the result of left matrix
    multiplying [t2] with [t1].

    Raises: InvalidDimension if the number of columns in [t1] is not equal to
    the number of rows in [t2] *)

val transpose : t -> t
(** [transpose t] is the transpose of tensor [t] *)

val sum : ?axis:int option -> t -> t
(** [sum t ax] is the sum of all of the values in [t] along the axis [ax]. If
    axis is not provided, it is the sum of all of the values in [t].

    Raises: OutOfBounds if axis greater than 1 or less than 0 *)


val scalar_mul : t -> int -> t
val to_list : t -> int list list
val shape : t -> int * int
val get : t -> int -> int -> int
