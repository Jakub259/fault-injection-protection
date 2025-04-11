; RUN: opt -passes='cfip<harden-all>' -load-pass-plugin=../build/lib/LLVMCfip.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s

target triple = "x86_64-redhat-linux-gnu"
declare i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %a)


; CHECK-LABEL: main
define dso_local i32 @main(i1 %0) local_unnamed_addr {
  ; CHECK: %unexpected_result = sub i1 false, %0
  ; CHECK-NEXT br i1 %0
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



!llvm.module.flags = !{!0, !1}
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 2}
