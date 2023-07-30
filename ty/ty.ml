include Syntax

(** 文字列のリストを自由リンクのリストへ変換する． *)
let ctx_of (x, ys) = (x, List.map (fun y -> FreeLink y) ys)
