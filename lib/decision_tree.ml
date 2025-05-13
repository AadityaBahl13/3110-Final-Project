open Data

type split = Lin_alg.t -> bool

type t =
  | Leaf of Data.label
  | Tree of tree

and tree = {
  data : Data.t;
  split : split ref;
  data_size : int;
  entropy : float;
  left : t ref;
  right : t ref;
}
