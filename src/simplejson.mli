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
  type t = string [@@deriving sexp]
  val request_encoding : t Json_encoding.encoding
end
