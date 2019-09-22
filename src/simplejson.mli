module Table : sig
  type 'a t
  type 'a col

  val time : label:string -> Ptime.t list col
  val string : label:string -> string list col
  val int : label:string -> int list col
  val int32 : label:string -> int32 list col
  val int64 : label:string -> int64 list col
  val float : label:string -> float list col

  val t0 : unit t
  val t1: 'a col -> 'a t
  val t2: 'a col -> 'b col -> ('a * 'b) t
  val t3: 'a col -> 'b col -> 'c col -> ('a * 'b * 'c) t

  val construct : 'a t -> 'a -> Json_repr.ezjsonm
end

module Query : sig
  type t = {
    requestId: string option;
    timezone: string option;
    panelId: string;
    dashboardId: int option;
    range: range;
    interval: Ptime.Span.t;
    targets: target list;
    maxDataPoints: int;
    startTime: Ptime.t option;
    adhocFilters: adhocFilter list;
  } [@@deriving sexp]
  and range = {
    from: Ptime.t;
    to_:  Ptime.t;
  }
  and target = {
    name: string option;
    refId: string;
    typ: targetType;
  }
  and targetType = Timeserie | Table
  and adhocFilter = {
    op: op;
    key: string;
    value: string;
  }
  and op = Eq | Neq | Gt | Lt | Geq | Leq

  val t : t Json_encoding.encoding
end

module Search : sig
  type request = string [@@deriving sexp]
  type response = string list [@@deriving sexp]
  val request_encoding : request Json_encoding.encoding
  val response_encoding : response Json_encoding.encoding
end

module Tag : sig
  val key_response : string list Json_encoding.encoding
  val value_request : string Json_encoding.encoding
  val value_response : string list Json_encoding.encoding
end
