; RUN: opt -passes='function(acfip)' -load-pass-plugin=../build/lib/LLVMCfip.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s

target triple = "x86_64-redhat-linux-gnu"
declare i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %a)


; CHECK-LABEL: main
define dso_local i32 @main() local_unnamed_addr {
  ; scalar add
  ; CHECK-COUNT-2: add i32 1, 42 
  %sum = add i32 1, 42
  ; CHECK: icmp ne i32 %1, %2
  ; CHECK: or i1 false, %is_fault_detected

  ; CHECK-COUNT-4: insertelement 
  %vec0 = insertelement <4 x i32> undef, i32 %sum, i32 0
  %vec1 = insertelement <4 x i32> %vec0, i32 %sum, i32 1
  %vec2 = insertelement <4 x i32> %vec1, i32 %sum, i32 2
  %vec = insertelement <4 x i32> %vec2, i32 %sum, i32 3
  
  ; vector add
  ; CHECK-COUNT-2: add <4 x i32> <i32 12, i32 23, i32 34, i32 -28>, %vec
  %vec_sum = add <4 x i32> <i32 12, i32 23, i32 34, i32 -28> , %vec
  ; Vector additionally has to be reduced
  ; CHECK: call i1 @llvm.vector.reduce.or.v4i1(<4 x i1> %is_fault_detected
  ; CHECK: or i1

  ; CHECK: %reduced = call i32 @llvm.vector.reduce.add.v4i32 
  %reduced = call i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %vec_sum)
  ; CHECK-COUNT-2: add i32 %reduced, %1
  %total = add i32 %reduced, %sum
  ; CHECK: icmp ne i32
  ; CHECK: or i1

  ; check if fault was detected
  ; CHECK: br i1 
  ; CHECK: label %error_handling, label %ret_block, !prof !

  ; CHECK-LABEL: ret_block:
  ; CHECK: ret i32
  ret i32 %total

  ; CHECK-LABEL: error_handling:
  ; CHECK: call void @llvm.trap()
  ; CHECK: unreachable
}



!llvm.module.flags = !{!0, !1}
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 2}
