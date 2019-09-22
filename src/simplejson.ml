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

module Table = struct
  type 'a t =
    | Nil : unit t
    | Cons : 'a col * 'b t -> ('a * 'b) t
    | Conv : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t

  and 'a col = {
    label: string;
    typ: 'a typ
  }

  and 'a typ =
    | Time : Ptime.t list typ
    | String : string list typ
    | Int : int list typ
    | Int32 : int32 list typ
    | Int64 : int64 list typ
    | Float : float list typ

  let string_of_typ : type a. a typ -> string = function
    | Time -> "time"
    | String -> "string"
    | _ -> "number"

  let json_of_typ_v : type a. a typ -> a -> Json_repr.ezjsonm list =
    fun typ v -> match typ with
      | Time -> List.map (fun v -> `Float (Ptime.to_float_s v)) v
      | String -> List.map (fun v -> `String v) v
      | Int -> List.map (fun v -> `Float (Int.to_float v)) v
      | Int32 -> List.map (fun v -> `Float (Int32.to_float v)) v
      | Int64 -> List.map (fun v -> `Float (Int64.to_float v)) v
      | Float -> List.map (fun v -> `Float v) v

  let flip ls =
    let rec inner acc ls =
      match ls with
      | [] -> []
      | [] :: _ -> List.rev acc
      | _ -> inner (List.(map hd ls) :: acc) List.(map tl ls) in
    inner [] ls

  let time ~label = { label; typ = Time }
  let string ~label = { label; typ = String }
  let int ~label = { label; typ = Int }
  let int32 ~label = { label; typ = Int32 }
  let int64 ~label = { label; typ = Int64 }
  let float ~label = { label; typ = Float }

  let conv proj inj col = Conv (proj, inj, col)

  let t0 = Nil
  let t1 c1 =
    conv (fun t -> (t, ())) (fun (t, ()) -> t) (Cons (c1, Nil))
  let t2 c1 c2 =
    conv (fun (t1, t2) -> (t1, (t2, ()))) (fun (t1, (t2, ())) -> t1, t2) (Cons (c1, (Cons (c2, Nil))))
  let t3 c1 c2 c3 =
    conv (fun (t1, t2, t3) -> (t1, (t2, (t3, ())))) (fun (t1, (t2, (t3, ()))) -> t1, t2, t3) (Cons (c1, (Cons (c2, (Cons (c3, Nil))))))

  let rec construct :
    type a. a t -> a -> Json_repr.ezjsonm list * Json_repr.ezjsonm list list = fun a b ->
    match a, b with
    | Conv (p, _, w), _ -> construct w (p b)
    | Nil, _ -> [], []
    | Cons ({ label; typ }, b), (va, vb) ->
      let a, b = construct b vb in
      `O ["text", `String label;
          "type", `String (string_of_typ typ)] :: a,
      json_of_typ_v typ va :: b

  let construct a b =
    let t, v = construct a b in
    `O [
      "type", `String "table";
      "columns", `A t;
      "rows", `A (List.map (fun a -> `A a) (flip v))
    ]
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
  type request = string [@@deriving sexp]
  type response = string list [@@deriving sexp]

  open Json_encoding

  let request_encoding =
    conv (fun s -> s) (fun s -> s) (obj1 (req "target" string))

  let response_encoding = list string
end

module Tag = struct
  open Json_encoding

  let key_response =
    list @@ conv (fun s -> (), s) (fun ((), s) -> s)
      (obj2
         (req "type" (constant "string"))
         (req "text" string))

  let value_request = obj1 (req "key" string)
  let value_response = list (obj1 (req "text" string))
end
