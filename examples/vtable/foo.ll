; ModuleID = 'Module'
source_filename = "Module"
target triple = "x86_64-apple-darwin18.7.0"

%_VtableFoo = type { void (%Foo*, i32)* }
%Foo = type { %_VtableFoo*, %pthread_t*, i32, i32, i32, i32 }
%pthread_t = type opaque

@_VtableFoo = global %_VtableFoo { void (%Foo*, i32)* @_Foo__setgi }
@0 = private unnamed_addr constant [12 x i8] c"This works!\00", align 1

declare i32 @printf(i8*, ...)

declare i8* @malloc(i64)

declare i32 @pthread_create(%pthread_t**, i8*, i8* (i8*)*, i8*)

declare i32 @pthread_join(%pthread_t*, i8**)

declare i32 @pthread_equal(%pthread_t*, %pthread_t*)

declare %pthread_t* @pthread_self()

define void @_Foo__setgi(%Foo*, i32) {
entry:
  %this = alloca %Foo*
  store %Foo* %0, %Foo** %this
  %x = alloca i32
  store i32 %1, i32* %x
  %2 = load i32, i32* %x
  %3 = load %Foo*, %Foo** %this
  %4 = getelementptr inbounds %Foo, %Foo* %3, i32 0, i32 5
  store i32 %2, i32* %4
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @0, i32 0, i32 0))
  ret void
}

define i32 @main() {
entry:
  %_var_y0 = alloca %Foo*
  %0 = call i8* @malloc(i64 ptrtoint (%Foo* getelementptr (%Foo, %Foo* null, i64 1) to i64))
  %1 = bitcast i8* %0 to %Foo*
  %2 = getelementptr inbounds %Foo, %Foo* %1, i32 0, i32 0
  store %_VtableFoo* @_VtableFoo, %_VtableFoo** %2
  %3 = getelementptr inbounds %Foo, %Foo* %1, i32 0, i32 2
  store i32 0, i32* %3
  %4 = getelementptr inbounds %Foo, %Foo* %1, i32 0, i32 3
  store i32 0, i32* %4
  store %Foo* %1, %Foo** %_var_y0
  %5 = load %Foo*, %Foo** %_var_y0
  %6 = getelementptr inbounds %Foo, %Foo* %5, i32 0, i32 0
  %7 = load %_VtableFoo*, %_VtableFoo** %6
  %8 = getelementptr inbounds %_VtableFoo, %_VtableFoo* %7, i32 0, i32 0
  %9 = load void (%Foo*, i32)*, void (%Foo*, i32)** %8
  call void %9(%Foo* %5, i32 10)
  ret i32 0
}
