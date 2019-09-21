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
