; RUN: opt -passes='cfip<harden-all>' -load-pass-plugin=../build/lib/LLVMCfip.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s


; CHECK-LABEL: main
define i32 @main(i1 %0) {
  ; CHECK: %unexpected_result = sub i1 false, %0
  ; CHECK-NEXT: br i1 %0
  br i1 %0, label %true, label %false

; CHECK-LABEL: false:
false:
  ; CHECK-NEXT: %false_branch_fault = icmp ne i1 %unexpected_result, false
  ; CHECK: %3 = or i8 0, %2
  ; CHECK: br i1 %4, label %error_handling, label %ret_block

; CHECK-LABEL: ret_block:
  ; CHECK: ret i32 1
  ret i32 1

; CHECK-LABEL: true:
true:
  ; CHECK-NEXT: %true_branch_fault = icmp ne i1 %unexpected_result, true
  ; CHECK: %6 = or i8 0, %5
  ; CHECK: br i1 %7, label %error_handling, label %ret_block1

; CHECK-LABEL: ret_block1:
  ; CHECK-NEXT: ret i32 0
  ret i32 0

; CHECK-LABEL: error_handling:
  ; CHECK:  call void @llvm.trap()
  ; CHECK:  unreachable
}
