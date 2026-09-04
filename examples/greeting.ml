module K = Kube
module C = Kube_crd

module Spec = struct
  type t = {
    message : string;
        [@kube.schema C.Schema.string ~min_length:1 ()]
        [@kube.description "Message reconciled into status."]
  }
  [@@deriving kube]
end

module Phase = struct
  type t = Pending | Ready | Failed of string [@@deriving kube]
end

module Status = struct
  type t = {
    observed_generation : int;
    reconciled_message : string;
    phase : Phase.t;
    conditions : C.Condition.t list;
  }
  [@@deriving kube]
end

include C.Resource.Make (struct
  module Spec = Spec
  module Status = Status

  let group = "demo.ocaml-kube.dev"
  let version = "v1alpha1"
  let kind = "Greeting"
  let plural = "greetings"
  let singular = "greeting"
  let scope = K.Core.Namespaced
  let short_names = [ "greet" ]
  let categories = []
end)
