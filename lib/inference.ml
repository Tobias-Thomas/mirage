open Eval

let likelihood_weighting rng prog n =
  let rec loop n =
    if n <= 0 then Ok []
    else
      match eval_program rng prog with
      | Error e -> Error e
      | Ok trace -> (
          match loop (n - 1) with
          | Error e -> Error e
          | Ok rest -> Ok (trace :: rest))
  in
  match loop n with
  | Error e -> Error e
  | Ok traces ->
      let normalized_weights =
        Util.logsumexp_normalization
          (List.map (fun t -> t.log_likelihood) traces)
      in
      Ok (traces, normalized_weights)

let metropolis_hastings rng prog n =
  let rec loop curr acc n =
    if n <= 0 then Ok (List.rev acc)
    else
      match eval_program rng prog with
      | Error e -> Error e
      | Ok trace ->
          let u = Random.State.float rng 1. in
          if log u < trace.log_likelihood -. curr.log_likelihood then
            loop trace (trace :: acc) (n - 1)
          else loop curr (curr :: acc) (n - 1)
  in
  match eval_program rng prog with
  | Error e -> Error e
  | Ok first_sample -> (
      match loop first_sample [ first_sample ] (n - 1) with
      | Error e -> Error e
      | Ok traces ->
          let w = 1. /. float_of_int n in
          Ok (traces, List.init n (fun _ -> w)))
