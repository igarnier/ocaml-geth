
type t = block list

and block =
  {
    name   : string;
    instrs : instr list
  }

and instr =
  | Add
  | Sub
  | Mul
  | Div
  | Exp
  | Eq
  | Push of { width : int }
  | Dup  of { width : int }
  | Pop
  | Jumpi of { block : string }
  | Lit of Evm.literal

module M = Map.Make(String)

type state =
  {
    pc    : int;    (* current program counter *)
    table : int M.t (* table from block name to program counter *)
  }

type bc =
  | UnboundLabelPush of string
  | Code of Evm.code

let instr code = Code (Evm.Instr code)
let lit literal = Code (Evm.Literal literal)    

let incr_pc state = { state with pc = state.pc + 1 }
let add_jump name pc state = { state with table = M.add name pc state.table }
  
let rec to_bytecode_aux blocks state =
  match blocks with
  | [] -> (state, [])
  | block :: tail ->
    let state, blockp = block_to_bytecode block state in
    let state, tailp  = to_bytecode_aux tail state in
    (state, blockp @ tailp)

and block_to_bytecode { name; instrs } (state : state) =
  let state = add_jump name state.pc state in
  let state = incr_pc state in
  let state, instrs = instrs_to_bytecode instrs state in
  (state, (instr Evm.JUMPDEST) :: instrs)

and instrs_to_bytecode instrs state =
  match instrs with
  | [] -> (state, [])
  | i :: tail ->
    let open Evm in
    let instrs, pc_incr =
      match i with
      | Jumpi { block } ->
        [UnboundLabelPush block; instr JUMPI], 3 (* UnboundLabelPush will be expanded as 2 instrs *)
      | Push { width } ->
        if not (1 <= width && width <= 32) then
          failwith "Asm.instrs_to_bytecode: wrong push width"
        else
          [instr (Ops.push width)], 1
      | Dup { width } ->
        if not (1 <= width && width <= 16) then
          failwith "Asm.instrs_to_bytecode: wrong dup width"
        else
          [instr (Ops.dup width)], 1
      | Lit literal -> [lit literal], (Evm.literal_width literal)
      | Add -> [instr ADD], 1
      | Sub -> [instr SUB], 1
      | Mul -> [instr MUL], 1
      | Div -> [instr DIV], 1
      | Exp -> [instr EXP], 1
      | Pop -> [instr POP], 1
      | Eq  -> [instr EQ], 1
    in
    let state =
      { state with pc = state.pc + pc_incr }
    in
    let state, tail_instrs = instrs_to_bytecode tail state in
    state, (instrs @ tail_instrs)

let resolve_names (instrs : bc list) state =
  List.fold_right (fun instr acc ->
      match instr with
      | UnboundLabelPush name ->
        let jump_pos =
          try M.find name state.table with
          | Not_found ->
            let m = Printf.sprintf "resolve_names: could not find name %s" name in
            failwith m
        in
        let jump_pos_lit = Evm.literal_of_int jump_pos in
        let jump_pos_width = Evm.literal_width jump_pos_lit in
        Printf.printf "lit : .%s., width: %d\n%!" (Obj.magic jump_pos_lit) jump_pos_width;
        (Evm.Instr (Evm.Ops.push jump_pos_width)) :: (Evm.Literal jump_pos_lit) :: acc
      | Code c ->
        c :: acc
    ) instrs []

let to_bytecode blocks =
  let state, instrs = to_bytecode_aux blocks { pc = 0; table = M.empty } in
  resolve_names instrs state
