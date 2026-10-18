let logsumexp numbers =
  let max_val = List.fold_left Float.max Float.neg_infinity numbers in
  max_val
  +. log (List.fold_left (fun s v -> s +. exp (v -. max_val)) 0. numbers)

let logsumexp_normalization numbers =
  let lse = logsumexp numbers in
  List.map (fun v -> exp (v -. lse)) numbers
