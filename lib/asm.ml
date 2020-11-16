open Batteries

type t = block list

and block = {name: string option; instrs: instr list}

and instr =
  | Add
  | Sub
  | Mul
  | Div
  | Exp
  | Eq
  | Not
  | Push of {width: int}
  | Dup of {level: int}
  | Pop
  | Jumpi of {block: string}
  | Jump of {block: string}
  | Return
  | Mload
  | Mstore
  | Sload
  | Sstore
  | Lit of Evm.literal

module M = Map.Make (String)

type state =
  { pc: int;
    (* current program counter *)
    table: int M.t (* table from block name to program counter *) }

type bc = UnboundLabelPush of string | Code of Evm.code

let instr code = Code (Evm.Instr code)
let lit literal = Code (Evm.Literal literal)
let incr_pc state = {state with pc= state.pc + 1}
let add_jump name pc state = {state with table= M.add name pc state.table}

let rec to_bytecode_aux blocks state =
  match blocks with
  | [] -> (state, [])
  | block :: tail ->
      let state, blockp = block_to_bytecode block state in
      let state, tailp = to_bytecode_aux tail state in
      (state, blockp @ tailp)

and block_to_bytecode {name; instrs} (state : state) =
  match name with
  | None -> instrs_to_bytecode instrs state
  | Some name ->
      let state = add_jump name state.pc state in
      let state = incr_pc state in
      let state, instrs = instrs_to_bytecode instrs state in
      (state, instr Evm.JUMPDEST :: instrs)

and instrs_to_bytecode instrs state =
  match instrs with
  | [] -> (state, [])
  | i :: tail ->
      let open Evm in
      let instrs, pc_incr =
        match i with
        | Jumpi {block} ->
            ([UnboundLabelPush block; instr JUMPI], 3)
            (* UnboundLabelPush will be expanded as 2 instrs *)
        | Jump {block} ->
            ([UnboundLabelPush block; instr JUMP], 3)
            (* UnboundLabelPush will be expanded as 2 instrs *)
        | Push {width} ->
            if not (1 <= width && width <= 32) then
              failwith "Asm.instrs_to_bytecode: wrong push width"
            else ([instr (Ops.push width)], 1)
        | Dup {level} ->
            if not (1 <= level && level <= 16) then
              failwith "Asm.instrs_to_bytecode: wrong dup width"
            else ([instr (Ops.dup level)], 1)
        | Lit literal -> ([lit literal], Evm.literal_width literal)
        | Add -> ([instr ADD], 1)
        | Sub -> ([instr SUB], 1)
        | Mul -> ([instr MUL], 1)
        | Div -> ([instr DIV], 1)
        | Exp -> ([instr EXP], 1)
        | Pop -> ([instr POP], 1)
        | Eq -> ([instr EQ], 1)
        | Not -> ([instr NOT], 1)
        | Return -> ([instr RETURN], 1)
        | Mload -> ([instr MLOAD], 1)
        | Mstore -> ([instr MSTORE], 1)
        | Sload -> ([instr SLOAD], 1)
        | Sstore -> ([instr SSTORE], 1) in
      let state = {state with pc= state.pc + pc_incr} in
      let state, tail_instrs = instrs_to_bytecode tail state in
      (state, instrs @ tail_instrs)

let resolve_names (instrs : bc list) state =
  List.fold_right
    (fun instr acc ->
      match instr with
      | UnboundLabelPush name ->
          let jump_pos =
            try M.find name state.table
            with Not_found ->
              let m =
                Printf.sprintf "resolve_names: could not find name %s" name
              in
              failwith m in
          let jump_pos_lit = Evm.literal_of_int jump_pos in
          let jump_pos_width = Evm.literal_width jump_pos_lit in
          Evm.Instr (Evm.Ops.push jump_pos_width)
          :: Evm.Literal jump_pos_lit :: acc
      | Code c -> c :: acc)
    instrs []

let to_bytecode blocks =
  let state, instrs = to_bytecode_aux blocks {pc= 0; table= M.empty} in
  resolve_names instrs state

let fresh =
  let counter = ref 0 in
  fun () ->
    let x = !counter in
    incr counter ;
    "fresh" ^ string_of_int x

let switch (cases : (Evm.literal * block) list) =
  let jump_table =
    List.fold_right
      (fun (case, block) table ->
        let width = Evm.literal_width case in
        Dup {level= 1}
        :: Push {width}
        :: Lit case :: Eq
        :: Jumpi {block= Option.get block.name}
        :: table)
      cases [] in
  let jump_table_block = {name= None; instrs= jump_table} in
  let end_name = fresh () in
  let end_block = {name= Some end_name; instrs= []} in
  let cases_code =
    List.map
      (fun (_, block) ->
        {block with instrs= block.instrs @ [Jump {block= end_name}]})
      cases in
  (jump_table_block :: cases_code) @ [end_block]
