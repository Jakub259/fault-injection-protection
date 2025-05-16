; RUN: opt -passes='firv2' -load-pass-plugin=../build/lib/LLVMFirv2.so %s -S -o %t.ll
; RUN: clang %t.ll -o %t.out
; RUN: %t.out
; RUN: cat %t.ll | FileCheck %s

target triple = "x86_64-redhat-linux-gnu"

%struct.Point = type { i32, i32 }

declare void @internal_firv2_0_identifier()

; Function returning a struct by pointer (sret)
; CHECK-LABEL: create_point.original
define dso_local void @create_point(ptr sret(%struct.Point) %result, i32 %x, i32 %y) {
  ; CHECK-NOT: call void @internal_firv2_0_identifier
  call void @internal_firv2_0_identifier()
  ; CHECK-NEXT: getelementptr inbounds %struct.Point, ptr %result, i32 0, i32 0
  %x_ptr = getelementptr inbounds %struct.Point, ptr %result, i32 0, i32 0
  ; CHECK-NEXT: store i32 %x, ptr %x_ptr, align 4
  store i32 %x, ptr %x_ptr, align 4
  ; CHECK-NEXT: %y_ptr = getelementptr inbounds %struct.Point, ptr %result, i32 0, i32 1
  %y_ptr = getelementptr inbounds %struct.Point, ptr %result, i32 0, i32 1
  ; CHECK-NEXT: store i32 %y, ptr %y_ptr, align 4
  store i32 %y, ptr %y_ptr, align 4
  ; CHECK-NEXT: ret void
  ret void
}

; CHECK-LABEL: internal_firv2_0_eq
define i1 @internal_firv2_0_eq(ptr %x, ptr %y) {
start:
  %x_struct = load %struct.Point, ptr %x, align 4
  %y_struct = load %struct.Point, ptr %y, align 4
  
  %x_x = extractvalue %struct.Point %x_struct, 0
  %y_x = extractvalue %struct.Point %y_struct, 0
  %cmp_x = icmp eq i32 %x_x, %y_x
  
  %x_y = extractvalue %struct.Point %x_struct, 1
  %y_y = extractvalue %struct.Point %y_struct, 1
  %cmp_y = icmp eq i32 %x_y, %y_y
  
  %result = and i1 %cmp_x, %cmp_y
  ret i1 %result
}

; Main function using the create_point function
; CHECK-LABEL: main
define dso_local i32 @main() {
  ; CHECK: %point = alloca %struct.Point, align 4
  %point = alloca %struct.Point, align 4
  ; CHECK-NEXT: call void @create_point(ptr %point, i32 10, i32 -10)
  call void @create_point(ptr %point, i32 10, i32 -10)
  ; CHECK-NEXT: %point_struct = load %struct.Point, ptr %point, align 4
  %point_struct = load %struct.Point, ptr %point, align 4
  ; CHECK-NEXT: %point_x = extractvalue %struct.Point %point_struct, 0
  %point_x = extractvalue %struct.Point %point_struct, 0
  ; CHECK-NEXT: %point_y = extractvalue %struct.Point %point_struct, 1
  %point_y = extractvalue %struct.Point %point_struct, 1
  ; CHECK-NEXT: %sum = add i32 %point_x, %point_y
  %sum = add i32 %point_x, %point_y
  ; CHECK-NEXT: ret i32 %sum
  ret i32 %sum
}

; CHECK: define void @create_point(ptr %0, i32 %1, i32 %2) {
; CHECK-LABEL: entry:
  ; CHECK-NEXT: %3 = alloca %struct.Point, align 8
  ; CHECK-NEXT: call void @create_point.original(ptr %0, i32 %1, i32 %2)
  ; CHECK-NEXT: call void @create_point.original(ptr %3, i32 %1, i32 %2)
  ; CHECK-NEXT: %4 = call i1 @internal_firv2_0_eq(ptr %0, ptr %3)
  ; CHECK-NEXT: br i1 %4, label %return, label %error, !prof !0
; CHECK-LABEL: return:                                           ; preds = %entry
  ; CHECK-NEXT: ret void

; CHECK-LABEL: error:                                            ; preds = %entry
  ; CHECK-NEXT: call void @llvm.trap()
  ; CHECK-NEXT: unreachable
