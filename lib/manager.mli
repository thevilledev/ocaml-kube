(** Structured supervision and composition for long-running controller
    components. Components may declare shared dependencies; each dependency is
    started exactly once by a manager. *)

type component

val component :
  ?dependencies:component list ->
  name:string ->
  (client:Client.t -> cancel:Cancel.t -> (unit, Client.error) result) ->
  component
(** Define a supervised component. The callback must run until cancellation or
    return an error. Dependencies are registered transitively and deduplicated.
*)

val component_name : component -> string

type error = { component : string; cause : Client.error }

val pp_error : Format.formatter -> error -> unit

type t

val create : ?cancel:Cancel.t -> Client.t -> t

val add : t -> component -> unit
(** Add a component and all of its dependencies. Raises [Invalid_argument] once
    the manager has started. Adding the same component more than once is safe.
*)

val run : t -> (unit, error) result
(** Start all registered components, propagate the first failure by cancelling
    the shared scope, and join every component before returning. A manager is
    single-use. *)
