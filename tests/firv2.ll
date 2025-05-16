; RUN: opt -passes='firv2' -load-pass-plugin=../build/lib/LLVMFirv2.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s


target triple = "x86_64-redhat-linux-gnu"
; CHECK-LABEL: foo
define void @foo() {
  ; CHECK: call i32 @main()
  call i32 @main()
  ret void
}

declare void @internal_firv2_0_identifier()

; CHECK-LABEL: main.original
define dso_local i32 @main() {
  ; CHECK-NOT: internal_firv2_0_identifier 
  call void @internal_firv2_0_identifier()
  ret i32 0
}

; CHECK-LABEL: internal_firv2_0_eq
define i1 @internal_firv2_0_eq(ptr %x, ptr %y) {
start:
  %_3 = load i32, ptr %x, align 4
  %_4 = load i32, ptr %y, align 4
  %_0 = icmp eq i32 %_3, %_4
  ret i1 %_0
}

; CHECK-LABEL: main
; CHECK-2: call i32 @main.original()
; CHECK: call i1 @internal_firv2_0_eq
; CHECK: br i1

; CHECK-LABEL: return:
; CHECK: ret i32

; CHECK-LABEL: error:
; CHECK: call void @llvm.trap()
; CHECK: unreachable
