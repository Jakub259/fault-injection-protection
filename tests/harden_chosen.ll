; RUN: opt -passes='cfip' -load-pass-plugin=../build/lib/LLVMCfip.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s


target triple = "x86_64-redhat-linux-gnu"

declare dso_local i32 @internal_cfip_opaque_call_(i32 noundef) local_unnamed_addr
declare i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %a)

; original function -> do not modify
; CHECK-LABEL: i32 @add_fn(i32 %0, i32 %1)
define i32 @add_fn(i32 %0, i32 %1) {
  ; CHECK: %s1 = add i32 0, %0
  %s1 = add i32 0, %0
  ; CHECK: %s2 = add i32 %s1, %1
  %s2 = add i32 %s1, %1
  ; CHECK: ret i32 %s2
  ret i32 %s2
}

; CHECK-LABEL: main
; CHECK-NOT: internal_cfip_opaque_call_
define dso_local i32 @main() local_unnamed_addr {
  %val = call i32 @internal_cfip_opaque_call_(i32 noundef 42)
  ; scalar add
  ; CHECK-COUNT-2: add i32 1, 42 
  %s1 = add i32 1, %val
  ; CHECK: icmp ne i32 %1, %2
  ; CHECK: %3 = zext i1 %is_fault_detected
  ; CHECK: or i8 0, %3

  ; call hardened version instead of original
  ; CHECK: %sum = call i32 @add_fn.hardened.1(i32 0, i32 %1)
  %sum = call i32 @add_fn(i32 0, i32 %s1)

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
  ; CHECK: or i8

  ; CHECK: %reduced = call i32 @llvm.vector.reduce.add.v4i32 
  %reduced = call i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %vec_sum)
  ; CHECK-COUNT-2: add i32 %reduced, %sum
  %total = add i32 %reduced, %sum
  ; CHECK: icmp ne i32
  ; CHECK: or i8

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

; CHECK-LABEL: @add_fn.hardened.1
  ; not an user of critical, thus occurs only once
  ; CHECK: %s1 = add i32 0, %0
  ; user of critical value, expect twice
  ; CHECK-COUNT-2: add i32 %s1, %1
; CHECK-LABEL: ret_block
  ; CHECK: ret i32
; CHECK-LABEL: error_handling:                                   ; preds = %2
  ; CHECK: call void @llvm.trap()

!llvm.module.flags = !{!0, !1}
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 2}
