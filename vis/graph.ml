open Util
open Gt

let links_of_atoms atoms = List.concat_map snd @@ atoms

(** local link と free link を分ける *)
let unzip_links =
  List.partition_map @@ function LocalLink l -> Left l | FreeLink f -> Right f

type connected_to = Port of int * int | HLink of int
type port_ = { port_id : int; port_label : string; port_to_ : connected_to }
type atom_ = { atom_id : int; atom_label : string; ports : port_ list }

type hlink_ = {
  hlink_id : int;
  hlink_label : string;
  hlink_to_ : connected_to list;
}

type graph_ = { atoms_ : atom_ list; hlinks_ : hlink_ list }

(** 局所リンクで端点が二つしか無いリンク (normal link) と，そうでは無いものに分離する．*)
let partition_links link_dict =
  let helper = function
    | LocalLink x, [ p1; p2 ] -> Either.Left (x, (p1, p2))
    | mapping -> Either.Right mapping
  in
  List.partition_map helper link_dict

(** アトムリストを可視化しやすいデータ構造に変換する．

    @author simplify 局所リンクを除去するかどうかの真偽値． *)
let portgraph_of_atoms ~simplify (atoms : graph) =
  let atoms = List.mapi (fun i x -> (i, x)) atoms in

  (* リンク名からポートの集合への写像を作る． *)
  let link_map =
    let helper (atom_i, (_, args)) =
      List.mapi (fun arg_i link -> (link, (atom_i, arg_i))) args
    in
    List.concat @@ List.map helper atoms
  in
  let link_dict = ListExtra.gather link_map in

  (* 局所リンクで端点が二つしか無いリンク (normal link) と，そうでは無いものに分離する．*)
  let normal_link_dict, hlink_dict =
    if simplify then partition_links link_dict else ([], link_dict)
  in

  (* リンク名から，hlink の id への写像． hlink の id は atom の id から連続してそれらよりも大きな値を取る． *)
  let atoms_length = List.length atoms in
  let free_link_names =
    List.mapi (fun i (x, _) -> (x, i + atoms_length)) hlink_dict
  in
  let get_link_i x =
    Option.value (List.assoc_opt x free_link_names) ~default:0
  in

  (* リンク名 x と 接続元の atom と port の id の組から，接続先の情報を取得する． *)
  let normal_link_dict =
    List.map (first @@ fun x -> LocalLink x) normal_link_dict
  in
  let connected_to_of x atom_port_id =
    match List.assoc_opt x normal_link_dict with
    | Some ((a1, p1), (a2, p2)) ->
        if (a1, p1) = atom_port_id then Port (a2, p2) else Port (a1, p1)
    | None -> HLink (get_link_i x)
  in

  (* hyperlinks *)
  let hlinks_ =
    let hlink_of x =
      let hlink_id = get_link_i x in
      {
        hlink_id;
        hlink_label = (match x with FreeLink x -> x | _ -> "");
        hlink_to_ =
          List.map (fun (atom_id, port_id) -> Port (atom_id, port_id))
          @@ List.assoc x hlink_dict;
      }
    in
    List.map (hlink_of <. fst) hlink_dict
  in

  (* atoms *)
  let atoms_ =
    (* (int * atom_name) * link list *)
    let atom_of (atom_id, (atom_name, links)) =
      {
        atom_id;
        atom_label = string_of_atom_name atom_name;
        ports =
          List.mapi
            (fun port_id x ->
              {
                port_id;
                port_label = string_of_int (port_id + 1);
                port_to_ = connected_to_of x (atom_id, port_id);
              })
            links;
      }
    in
    List.map atom_of atoms
  in
  { atoms_; hlinks_ }
