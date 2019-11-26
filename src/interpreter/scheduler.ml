open Runtime_env

type schedule = Random

(* choose a random thread from the pool *)
let random_schedule thread_pool =
  let thread_ids = List.map (fun (TThread (threadID, _, _)) -> threadID) thread_pool in
  let index = Random.int (List.length thread_ids) in
  List.nth thread_ids index

let schedule_thread schedule thread_pool _heap =
  match schedule with Random -> random_schedule thread_pool
