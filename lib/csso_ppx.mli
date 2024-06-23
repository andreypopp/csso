module Stylesheet : sig
  type t
  (** Extracted CSS stylesheet. *)

  val empty : t
  (** An empty stylesheet. *)

  val of_ml : string -> t -> t
  (** Extract the CSS from an OCaml parsetree and add it to the stylesheet. *)

  val of_css : string -> t -> t
  (** Extract the CSS from a CSS file and add it to the stylesheet. *)

  val output_css : Out_channel.t -> t -> unit
  (** Output the stylesheet's CSS to a channel. *)
end
