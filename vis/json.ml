open Util
open Graph

(** Json への変換関数 *)
let json_of_connected_to = function
  | Port (atom_id, port_id) ->
      `Assoc [ ("nodeId", `Int atom_id); ("portId", `Int port_id) ]
  | HLink hlink_id -> `Assoc [ ("nodeId", `Int hlink_id) ]

let json_of_port port_ =
  `Assoc
    [
      ("id", `Int port_.port_id);
      ("label", `String port_.port_label);
      ("to", json_of_connected_to port_.port_to_);
    ]

let json_of_atom atom_ =
  `Assoc
    [
      ("id", `Int atom_.atom_id);
      ("label", `String atom_.atom_label);
      ("ports", `List (List.map json_of_port atom_.ports));
    ]

let json_of_hlink hlink_ =
  `Assoc
    [
      ("id", `Int hlink_.hlink_id);
      ("label", `String hlink_.hlink_label);
      ("to", `List (List.map json_of_connected_to hlink_.hlink_to_));
    ]

let json_of_graph graph_ =
  `Assoc
    [
      ("atoms", `List (List.map json_of_atom graph_.atoms_));
      ("hlinks", `List (List.map json_of_hlink graph_.hlinks_));
    ]

(** 可視化のために，アトムリストを JSON の文字列に変換する *)
let pretty_graph =
  Yojson.Basic.pretty_to_string <. json_of_graph
  <. Graph.portgraph_of_atoms ~simplify:true
