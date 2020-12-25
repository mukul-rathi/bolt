; ModuleID = 'Module'
source_filename = "Module"
target triple = "x86_64-apple-darwin18.7.0"

declare i32 @printf(i8*, ...)

declare i8* @GC_malloc(i64)

declare i32 @pthread_create(i8**, i8*, i8* (i8*)*, i8*)

declare i32 @pthread_join(i8*, i8**)

declare i32 @pthread_equal(i8*, i8*)

declare i8* @pthread_self()

define i32 @factorial(i32) {
entry:
  %n = alloca i32
  store i32 %0, i32* %n
  %1 = load i32, i32* %n
  %eq = icmp eq i32 %1, 0
  br i1 %eq, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  %2 = load i32, i32* %n
  %3 = load i32, i32* %n
  %sub = sub i32 %3, 1
  %4 = call i32 @factorial(i32 %sub)
  %mult = mul i32 %2, %4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 1, %then ], [ %mult, %else ]
  ret i32 %iftmp
}

define i32 @main() {
entry:
  %0 = call i32 @factorial(i32 5)
  ret i32 0
}
