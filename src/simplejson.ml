open Sexplib.Std

module Ptime = struct
  include Ptime

  module Span = struct
    include Span
    let t_of_sexp sexp =
      let sexp_fl = float_of_sexp sexp in
      Option.get (of_float_s sexp_fl)

    let sexp_of_t t =
      sexp_of_float (to_float_s t)
  end

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 ~frac_s:3 t)
end

module Query = struct
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

  open Json_encoding

  let op =
    string_enum [
      "=", Eq;
      "!=", Neq;
      ">", Gt;
      "<", Lt;
      ">=", Geq;
      "<=", Leq;
    ]

  let targetType =
    string_enum [
      "timeserie", Timeserie;
      "table", Table;
    ]

  let rfc3339 =
    conv
      Ptime.to_rfc3339
      (fun s -> match Ptime.of_rfc3339 s with
         | Ok (t, _, _) -> t
         | Error _ -> invalid_arg "rfc3339")
      string

  let spanms =
    conv
      (fun t -> (Ptime.Span.to_float_s t) *. 1e3)
      (fun v -> Option.get (Ptime.Span.of_float_s (v *. 1e-3)))
      float

  let unixtime =
    conv
      (fun t -> 1e3 *. Ptime.to_float_s t)
      (fun i -> Option.get (Ptime.of_float_s (i *. 1e-3)))
      float

  let range =
    conv
      (fun { from; to_ } -> (),(from, to_))
      (fun ((), (from, to_)) -> { from; to_ })
      (merge_objs unit
         (obj2
            (req "from" rfc3339)
            (req "to" rfc3339)))

  let target =
    conv
      (fun { name; refId; typ } -> (name, refId, typ))
      (fun (name, refId, typ) -> { name; refId; typ })
      (obj3
         (opt "target" string)
         (req "refId" string)
         (req "type" targetType))

  let adhocFilter =
    conv
      (fun { op ; key ; value } -> (op, key, value))
      (fun (op, key, value) -> { op ; key ; value })
      (obj3
         (req "op" op)
         (req "key" string)
         (req "value" string))

  let stringint =
    union [
      case string (fun a -> Some a) (fun a -> a) ;
      case int53 Int64.of_string_opt Int64.to_string ;
    ]

  let t_ =
    conv
      (fun ({ requestId; timezone; panelId; dashboardId;
              range; interval; targets;
              maxDataPoints; startTime; adhocFilters }:t) ->
        requestId, timezone, panelId, Some dashboardId,
        range, interval, targets,
        maxDataPoints, startTime, adhocFilters)
      (fun (requestId, timezone, panelId, dashboardId,
        range, interval, targets,
            maxDataPoints, startTime, adhocFilters) ->
        let dashboardId = Option.value dashboardId ~default:None in
        { requestId; timezone; panelId; dashboardId;
          range; interval; targets;
          maxDataPoints; startTime; adhocFilters } )
      (obj10
         (opt "requestId" string)
         (opt "timezone" string)
         (req "panelId" stringint)
         (opt "dashboardId" (option int))
         (req "range" range)
         (req "intervalMs" spanms)
         (req "targets" (list target))
         (req "maxDataPoints" int)
         (opt "startTime" unixtime)
         (req "adhocFilters" (list adhocFilter)))

  let t =
    conv (fun t -> (), t) (fun ((), t) -> t) (merge_objs unit t_)
end

module Search = struct
  type t = string [@@deriving sexp]
  let request_encoding =
    let open Json_encoding in
    conv (fun s -> s) (fun s -> s) (obj1 (req "target" string))
end
