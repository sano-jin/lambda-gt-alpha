(** リスト系の追加関数

    - List.Extra モジュールなどとして定義した方が良いかも *)

open Combinator
(** open basic combinators *)

open Option_extra

(** リストの要素の添字番号を取得する *)
let index_of elem =
  let rec helper index = function
    | [] -> None
    | h :: t -> if h = elem then Some index else helper (succ index) t
  in
  helper 0

(** [('a -> 'b -> 'a * ('c * 'd list)) -> 'a -> 'b list -> 'a * ('c list * 'd list)]
    2重の fold_left_map． *)
let fold_left_map2 f acc xs =
  let acc, ys = List.fold_left_map f acc xs in
  let zs, ws = List.split ys in
  (acc, (zs, List.concat ws))

(** 要素に Option を返す関数を適用して，初めて Some になったところでリストを分割する

    - Some になった値も返す
    - None になった部分のリストは反転して返すことに注意
    - [rev_appned] をすると元のリストから一つ要素を除いたものになる
    - [break_opt (fun x -> if x > 3 then Some x else None) \[1; 2; 3; 4; 5; 6\]
      ---> Some (4, (\[3; 2; 1\], \[5; 6\]))
      ]
    - Tail-recursive

    @return a list, (Some (f a, a list) | None) *)
let rev_break_opt f =
  let rec helper left = function
    | [] -> None
    | h :: t -> (
        match f h with
        | None -> helper (h :: left) t
        | Some s -> Some (s, (left, t)))
  in
  helper []

(** Tail-recursive List.concat with List.rev_append

    - リストのリストを反転させながら結合する
    - [rev_concat \[\[6; 5; 4\]; \[3; 2\]; \[1\]\] \[7; 8\]
      ---> rev_append \[1\] (rev_append \[3; 2\] (rev_append \[6; 5; 4\] \[7; 8\]))
      ---> \[1; 2; 3; 4; 5; 6; 7; 8\]
      ] *)
let rev_concat_append lists list =
  List.fold_left (flip List.rev_append) list lists

(** Tail-recursive List.concat

    - リストのリストを反転させながら結合する
    - [rev_concat \[\[1; 2; 3\]; \[4; 5\]; \[6\]\]
      ---> rev_append \[6\] (rev_append \[4; 5\] (rev_append \[1; 2; 3\] \[\]))
      ---> \[6; 5; 4; 3; 2; 1\]
      ] *)
let rev_concat lists = rev_concat_append [] lists

(** [List.fold_left] with indices *)
let fold_lefti f =
  let rec helper i acc = function
    | [] -> acc
    | h :: t -> helper (succ i) (f i acc h) t
  in
  helper 0

(** [List.fold_left_map] with indices *)
let fold_left_mapi f acc xs =
  let rec helper i acc = function
    | [] -> (acc, [])
    | h :: t ->
        let acc, h = f i acc h in
        let acc, t = helper (succ i) acc t in
        (acc, h :: t)
  in
  helper 0 acc xs

(** リストを "回転" する

    - [roll \[1; 2; 3; 4\] ---> \[2; 3; 4; 1\]]
    - TODO: もっと効率の良い実装にする．キューを使うなど *)
let roll = function [] -> [] | h :: t -> t @ [ h ]

(** Drop the last element of the list *)
let rec dropLast1 = function
  | [] -> failwith "cannot drop the last element from an empty list"
  | [ _ ] -> []
  | h :: t -> h :: dropLast1 t

(** get the duplicated elements of the list *)
let get_dup_opt l =
  let rec helper rest = function
    | [] -> None
    | h :: t -> if List.mem h rest then Some h else helper (h :: rest) t
  in
  helper [] l

(** filter_map with indices *)
let filter_mapi f l =
  let rec helper i = function [] -> [] | h :: t -> f h :: helper (succ i) t in
  helper 0 l

(** randomize list *)
let rec shuffle = function
  | [] -> []
  | [ x ] -> [ x ]
  | list ->
      let before, after = List.partition (fun _ -> Random.bool ()) list in
      List.rev_append (shuffle after) (shuffle before)

(** Remove duplications in a list *)
let remove_dup comparer list =
  let rec helper result = function
    | [] -> result
    | h :: t ->
        if List.exists (comparer h) result then helper result t
        else helper (h :: result) t
  in
  List.rev @@ helper [] list

(** make n length list with elem as the all elements *)
let repeat n elem = List.init n @@ const elem

(** Either 型の要素のリストを左右に振り分ける *)
let partition_eithers l = List.partition_map id l

(** safe [List.combine] *)
let combine_opt list1 list2 =
  let rec helper = function
    | [], [] -> Some []
    | h1 :: t1, h2 :: t2 ->
        let+ t = helper (t1, t2) in
        (h1, h2) :: t
    | _ -> None
  in
  helper (list1, list2)

(** pretty print list with the given pretty printer of the elements *)
let string_of_list string_of_elem list =
  "[" ^ String.concat "; " (List.map string_of_elem list) ^ "]"

(** Set operations. * Note this is not as efficient as the operations that the
    [Set] module provides *)

(** set minus. e.g. [set_minus \[1, 2, 3\] \[2, 4\] = \[1, 3\]] *)
let set_minus l r = List.filter (not <. flip List.mem r) l

(** set minus using physical equality *)
let set_minusq l r = List.filter (not <. flip List.memq r) l

(** symmetric difference. e.g. [sym_diff \[1, 2, 3\] \[2, 4\] = \[1, 3, 2, 4\]] *)
let sym_diff l r = set_minus l r @ set_minus r l

let set_eq l r = List.sort_uniq compare l = List.sort_uniq compare r

(** intersection. e.g. [sym_diff \[1, 2, 3\] \[2, 3, 4\] = \[2, 3\]] *)
let intersection l r = List.filter (flip List.mem r) l

(** for all with indices*)
let for_alli f l = List.fold_left ( && ) true @@ List.mapi f l

(** split the list to the n first list and the rest *)
let rec uncons_n n list =
  if n < 0 then failwith @@ Printf.sprintf "cannot uncons first %d element(s)" n
  else if n = 0 then ([], list)
  else
    match list with
    | [] ->
        failwith
        @@ Printf.sprintf "cannot uncons first %d element(s) from an empty list"
             n
    | h :: t ->
        let hs, t = uncons_n (pred n) t in
        (h :: hs, t)

(** 連想リストを更新しながら値を返す． もし更新したら [Some] に包んだ関数の返り値と，更新されたリストを返す． *)
let rec update_list f x1 = function
  | [] -> None
  | (x2, v2) :: t ->
      if x1 = x2 then
        let v3, result = f v2 in
        Some ((x2, v3) :: t, result)
      else
        let+ t, result = update_list f x1 t in
        ((x2, v2) :: t, result)

(** 連想リストの leftmost binding of x を 更新する．*)
let rec update_assoc f x = function
  | [] -> []
  | (x', v) :: t ->
      if x = x' then (x, f v) :: t else (x, v) :: update_assoc f x t

(** 以下試作であまり価値がなさそう *)

(** リストのリストをそれぞれ head と tail に分けてそれぞれリストにしたタプルを返す． transpose のための補助関数．
    [unconses \[\[1; 2; 3\]; \[\]; \[4; 5\]\] = (\[1; 4\], \[\[2; 3\]; \[5\]\])] *)
let rec unconses = function
  | [] -> ([], [])
  | [] :: ts -> unconses ts
  | (h :: t) :: ts ->
      let hs, ts = unconses ts in
      (h :: hs, t :: ts)

(** transpose the list of lists; i.e. matrix.
    [transpose \[\[1; 2; 3\]; \[\]; \[4; 5\]\] = \[\[1; 4\]; \[2; 5\]; \[3\]\]] *)
let rec transpose lists =
  let hs, ts = unconses lists in
  hs :: transpose ts

(** [\[x -> y1; x -> y2; ...\]] を [\[x -> \[y1; y2; ...\]; ...\] にする *)
let gather mappings =
  let rec insert (x, y) = function
    | [] -> [ (x, [ y ]) ]
    | (x2, ys) :: t ->
        if x = x2 then (x2, y :: ys) :: t else (x2, ys) :: insert (x, y) t
  in
  List.fold_right insert mappings []

(** 要素のリストから，その要素が何個含まれていたかの連想リストを作って返す *)
let multiset l =
  let rec update f x default = function
    | [] -> [ default ]
    | (h1, h2) :: t ->
        if x = h1 then (h1, f h2) :: t else (h1, h2) :: update f x default t
  in
  let helper multiset x = update succ x (x, 1) multiset in
  List.fold_left helper [] l

(** Make a quotient set. *)
let make_eqrel rel =
  let merge (x, y) ss =
    let xs = List.find (List.mem x) ss in
    let ys = List.find (List.mem y) ss in
    List.sort_uniq compare (xs @ ys)
    :: List.filter (fun zs -> zs <> xs && zs <> ys) ss
  in
  List.fold_right merge rel @@ List.sort_uniq compare
  @@ List.concat_map (fun (x, y) -> [ [ x ]; [ y ] ]) rel
