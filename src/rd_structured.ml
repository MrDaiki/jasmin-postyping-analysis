open Jasmin.Utils
open Jasmin.Prog

module Rd = struct 


  type  t = {
    rd_in : (int, (uid,int Set.t)Map.t) Map.t; 
    rd_out : (int, (uid,int Set.t)Map.t) Map.t;
    prev_out : (uid,int Set.t) Map.t;
  }

  let empty = {
    rd_in = Map.empty;
    rd_out = Map.empty;
    prev_out = Map.empty;
  }
  
  
  
  let merge_rd_maps (m1:(uid,int Set.t) Map.t) (m2:(uid,int Set.t) Map.t) : (uid,'info Set.t) Map.t =
    let var_set = Map.foldi (fun k _ s -> Set.add k s) m1 Set.empty in
    let var_set = Map.foldi (fun k _ s -> Set.add k s) m2 var_set in
    Set.fold (fun (v:uid) m -> 
      let m1_in = Map.find_default Set.empty v m1 in
      let m2_in = Map.find_default Set.empty v m2 in
      Map.add v (Set.union m1_in m2_in) m
    ) var_set Map.empty
  
  let merge_rd (rd1:('info, (uid,'info Set.t)Map.t) Map.t ) (rd2:('info, (uid,'info Set.t)Map.t) Map.t) : ('info, (uid,'info Set.t)Map.t) Map.t =
    let instrs_set = Map.foldi (fun k _ s -> Set.add k s) rd1 Set.empty in
    let instrs_set = Map.foldi (fun k _ s -> Set.add k s) rd2 instrs_set in
    Set.fold (
      fun v m -> 
        let m1_out_v = Map.find_default Map.empty v rd1 in
        let m2_out_v = Map.find_default Map.empty v rd2 in
        let m_out_v = merge_rd_maps m1_out_v m2_out_v in
        Map.add v m_out_v m
    ) (instrs_set) Map.empty

  let merge (rd1: t) (rd2: t) :  t =
    let rd_in = merge_rd rd1.rd_in rd2.rd_in in
    let rd_out = merge_rd rd1.rd_out rd2.rd_out in
    let prev_out = merge_rd_maps rd1.prev_out rd2.prev_out in
    {
      rd_in;
      rd_out;
      prev_out;
    }
  
  let cmp_rd_var (rd1_vars : (uid,'info Set.t)Map.t) (rd2_vars : (uid,'info Set.t)Map.t) : bool =
    let rec compare_values (values) = 
      match values with
      | [] -> true
      | (k)::values -> 
          let s1 = Map.find_default Set.empty k rd1_vars in
          let s2 = Map.find_default Set.empty k rd2_vars in
          match Set.equal s1 s2 with
          | false -> false
          | true -> compare_values values 
      in

    let rd1_keys = Map.foldi (fun k _ s -> Set.add k s) rd1_vars Set.empty in
    let rd2_keys = Map.foldi (fun k _ s -> Set.add k s) rd2_vars Set.empty in
    match Set.equal  rd1_keys rd2_keys with
    | false -> false
    | true -> 
      let keys = Map.foldi (fun k _ l ->  k::l) rd1_vars [] in
      compare_values keys 


  let cmp_rd (rd1 : ('info,(uid,'info Set.t)Map.t) Map.t) (rd2 : ('info,(uid,'info Set.t)Map.t) Map.t) : bool =
    let rec compare_values (values) =
      match values with
      | [] -> true
      | (k)::values -> 
        let s1 = Map.find_default Map.empty k rd1 in
        let s2 = Map.find_default Map.empty k rd2 in
        match cmp_rd_var s1 s2 with
        | false -> false
        | true ->  compare_values values
      in
    let rd1_keys = Map.foldi (fun k _ s -> Set.add k s) rd1 Set.empty in
    let rd2_keys = Map.foldi (fun k _ s -> Set.add k s) rd2 Set.empty in
    match Set.equal  rd1_keys rd2_keys with
    | false -> false
    | true -> compare_values  (Map.foldi (fun k _ l ->  k::l) rd1 [])

  let cmp (rd1: t) (rd2: t) : bool =
    cmp_rd rd1.rd_out rd2.rd_out && cmp_rd rd1.rd_in rd2.rd_in

end

let repr_rd_var (k: uid) (v: int Set.t) : unit =
  Printf.printf "\t %s:  " (string_of_uid k);
  Set.iter ( fun v -> Printf.printf "%d " v) v
  
let repr_rd_var_instr (instr: int) (vars: (uid, int Set.t) Map.t ) : unit =
  Printf.printf "i %d: \n" instr;
  Map.iter ( fun k v -> 
    repr_rd_var k v) vars;
  Printf.printf "\n"

let repr_rd (rd: Rd.t) : unit =
  Map.iter (
    fun k v -> repr_rd_var_instr k v
  ) rd.rd_out


let computed_vars (lvs: int glvals) : uid Set.t =
  List.fold_left( fun s lv ->
    match lv with
    | Lnone (_,_) -> s
    | Lvar gv -> Set.add (L.unloc gv).v_id s
    | Lmem ( _,_ ,gv,_ ) -> Set.add (L.unloc gv).v_id s
    | Laset (_,_,_,gv,_)  -> Set.add (L.unloc gv).v_id s
    | Lasub (_,_,_,gv,_) -> Set.add (L.unloc gv).v_id s
    ) Set.empty lvs


let compute_instr_rd (prev: (uid,'info Set.t) Map.t) (instr:int) (vars:uid Set.t) : (uid,'info Set.t) Map.t =
    Set.fold (
      fun v m ->
        let new_instr_set = Set.add instr Set.empty in 
        Map.add v new_instr_set m
    ) vars prev

let rec reaching_definitions (rd:  Rd.t) (instrs:(int,'info,'asm) gstmt) : Rd.t =

  let update_rd (rd:  Rd.t) (instr:(int,'info,'asm) ginstr)  (cp: uid Set.t):  Rd.t =
    let out = compute_instr_rd rd.prev_out instr.i_loc.uid_loc cp in
    {
      rd_in = (Map.add instr.i_loc.uid_loc rd.prev_out rd.rd_in);
      rd_out = Map.add instr.i_loc.uid_loc out rd.rd_out;
      prev_out = out;
    }
  in

  let rec stabilize (rd: Rd.t) (instrs:(int,'info,'asm) gstmt) : Rd.t =
    let new_rd = reaching_definitions rd instrs in
    if (Rd.cmp rd new_rd) then
      new_rd
    else (
      stabilize new_rd instrs
    )
  in

  let reaching_definitions_instr (rd: Rd.t) (instr:(int,'info,'asm) ginstr) : Rd.t =
    match instr.i_desc with
    | Cassgn (lv,_,_,_) ->
      let cp = computed_vars [lv] in
      update_rd rd instr cp
    | Copn (lvs,_ ,_,_) ->
      let cp = computed_vars lvs in
      update_rd rd instr cp
    | Csyscall (lvs,_,_) ->
      let cp = computed_vars lvs in
      update_rd rd instr cp
    | Ccall (lvs,_,_) -> 
      let cp = computed_vars lvs in
      update_rd rd instr cp
    | Cif (_,b1,b2) ->
      let rd_left = reaching_definitions rd b1 in
      let rd_right = reaching_definitions rd b2 in
      Rd.merge rd_left rd_right
    | Cfor  (vd,_,s) -> 
      let cp = Set.singleton (L.unloc vd).v_id in
      let rd = update_rd rd instr cp in
      stabilize rd s
    | Cwhile (_,b1,_,b2) -> 
      let rd = reaching_definitions rd b1 in 
      stabilize rd (b1 @ b2)
    in 

    List.fold (fun rd instr -> reaching_definitions_instr rd instr) rd instrs

let rd_prog ((_,funcs): ('info,'asm) prog) : unit =
  let rds = List.map (fun fn -> reaching_definitions Rd.empty fn.f_body) funcs in
  List.iter repr_rd rds