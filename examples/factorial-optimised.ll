; ModuleID = 'Module'
source_filename = "Module"
target triple = "x86_64-apple-darwin18.7.0"

%pthread_t = type opaque

declare i32 @printf(i8*, ...)

declare i8* @GC_malloc(i64)

declare i32 @pthread_create(%pthread_t**, i8*, i8* (i8*)*, i8*)

declare i32 @pthread_join(%pthread_t*, i8**)

declare i32 @pthread_equal(%pthread_t*, %pthread_t*)

declare %pthread_t* @pthread_self()

define i32 @factorial(i32) {
entry:
  %eq = icmp eq i32 %0, 0
  br i1 %eq, label %ifcont, label %else

else:                                             ; preds = %entry
  %sub = add i32 %0, -1
  %1 = call i32 @factorial(i32 %sub)
  %mult = mul i32 %1, %0
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %iftmp = phi i32 [ %mult, %else ], [ 1, %entry ]
  ret i32 %iftmp
}

define i32 @main() {
entry:
  %0 = call i32 @factorial(i32 5)
  ret i32 0
}
