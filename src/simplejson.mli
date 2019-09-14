module Query : sig
  type t = {
    requestId: string option;
    timezone: string option;
    panelId: int;
    dashboardId: int option;
    range: range;
    interval: Ptime.Span.t;
    targets: target list;
    maxDataPoints: int;
    startTime: Ptime.t;
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
  val request_encoding : string Json_encoding.encoding
end
