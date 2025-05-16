type t
(** [type t] is a perceptron that predicts a binary label for a given piece of
    data based on a linear decision boundary that was derived from provided
    training data *)

val init_perceptron : Data.t -> int -> t
(** [init_perceptron data n] is a perceptron with max steps [n] and training
    data [data] *)

val train : t -> unit
(** [train t] trains perceptron [t] until achieving convergence or reaching the
    maximum step count *)

val predict : t -> Lin_alg.t -> Data.label
(** [predict t tensor] predicts a label for [tensor] based on the decision
    weights in [t] *)

val step : t -> Lin_alg.t -> Data.label -> bool
(** [step t x y] adjusts the weights of [t] based on whether the current
    predicition for [x] matches the actual label [y] *)

val get_weight : t -> Lin_alg.t
(** [get_weight t] is the weight vector of the perceptron *)

val get_bias : t -> int
(** [get_bias t] is the bias value of the perceptron *)

val check_perceptron : t -> bool
(** [check_perceptron t] is true if [t] correctly predicts the label of every
    training point. Otherwise, it is false. *)
