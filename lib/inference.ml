open Eval

let rec sample_rec rng prog n =
  if n <= 0 then Ok []
  else
    match eval_program rng prog with
    | Error e -> Error e
    | Ok trace -> (
        match sample_rec rng prog (n - 1) with
        | Error e -> Error e
        | Ok rest -> Ok (trace :: rest))

let sample rng prog n =
  match sample_rec rng prog n with
  | Error e -> Error e
  | Ok traces ->
      let normalized_weights =
        Util.logsumexp_normalization (List.map (fun t -> t.log_weight) traces)
      in
      Ok (traces, normalized_weights)
