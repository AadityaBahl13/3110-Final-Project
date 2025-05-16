type t
(** [type t] represents a tensor of two dimensions *)

exception InvalidDimensions
(** [excpetion InvalidDimensions] is raised when an operation requires a tensor
    to be of a certain shape, but that tensor is not of that specific shape *)

exception OutOfBound
(** [excpetion OutOfBounds] is raised when an operation requires a value from a
    tensor at a location that is out of bounds for the shape of that tensor *)

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
(** [scalar_mul t c] is the tensor [t] with every element in [t] multiplied by
    the value [c] *)

val to_list : t -> int list list
(** [to_list t] is a list representation of [t] *)

val shape : t -> int * int
(** [shape t] is the number of rows and columns that [t] has *)

val get : t -> int -> int -> int
(** [get t r c] is the value in row [r] and column [c] in tensor [t] *)
