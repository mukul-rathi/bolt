open Runtime_env

type schedule = Random

val schedule_thread : schedule -> thread_pool -> heap -> threadID
