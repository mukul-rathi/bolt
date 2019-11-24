open Runtime_env

type schedule = Random

let random_schedule thread_pool =
  let threadIDs = List.map (fun (TThread (threadID, _, _)) -> threadID) thread_pool in
  let index = Random.int (List.length threadIDs) in
  List.nth threadIDs index

let schedule_thread schedule thread_pool _heap =
  match schedule with Random -> random_schedule thread_pool
