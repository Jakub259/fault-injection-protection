; RUN: opt -passes='cfip' -load-pass-plugin=../build/lib/LLVMCfip.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s

; cfip on finalization runs sroa, thus afterwards there are no structs
; this test could be improved by not running sroa after cfip
; but this would require a change in plugin source code

declare void @cfip_harden_var_(ptr sret([1 x i32]) %a, ptr byval([1 x i32]) %b)

; CHECK-LABEL: main
define dso_local i32 @main() local_unnamed_addr {
  ; CHECK-NOT: cfip_harden_var_
  %result = alloca [1 x i32], align 4
  %input = alloca [1 x i32], align 4
  %input_ptr = getelementptr inbounds [1 x i32], ptr %input, i64 0, i64 0
  ; CHECK: store volatile i32 42
  store volatile i32 42, ptr %input_ptr, align 4
  
  call void @cfip_harden_var_(ptr sret([1 x i32]) %result, ptr byval([1 x i32]) %input)
  
  %result_ptr = getelementptr inbounds [1 x i32], ptr %result, i64 0, i64 0
  ; CHECK: load volatile i32
  %val = load volatile  i32, ptr %result_ptr, align 4
  ; CHECK-COUNT-2: sub i32 42
  %s1 = sub i32 42, %val

  ; CHECK: br i1
; CHECK-LABEL: ret_block:
  ret i32 %s1
; CHECK-LABEL: error_handling:
    ; CHECK: unreachable
}
