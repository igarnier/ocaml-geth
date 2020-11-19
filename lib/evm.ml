(* A literal is an hexadecimal string, without 0x prefix, of length at most 32 bytes and at least 1 byte (2 hex digits) *)
type literal = string

type instr_code =
  | STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | ADDMOD
  | MULMOD
  | EXP
  | SIGNEXTEND
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | ISZERO
  | AND
  | OR
  | XOR
  | NOT
  | BYTE
  | SHA3
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | RETURNDATASIZE
  | RETURNDATACOPY
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | GETPC
  | MSIZE
  | GAS
  | JUMPDEST
  | PUSH1
  | PUSH2
  | PUSH3
  | PUSH4
  | PUSH5
  | PUSH6
  | PUSH7
  | PUSH8
  | PUSH9
  | PUSH10
  | PUSH11
  | PUSH12
  | PUSH13
  | PUSH14
  | PUSH15
  | PUSH16
  | PUSH17
  | PUSH18
  | PUSH19
  | PUSH20
  | PUSH21
  | PUSH22
  | PUSH23
  | PUSH24
  | PUSH25
  | PUSH26
  | PUSH27
  | PUSH28
  | PUSH29
  | PUSH30
  | PUSH31
  | PUSH32
  | DUP1
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | DUP7
  | DUP8
  | DUP9
  | DUP10
  | DUP11
  | DUP12
  | DUP13
  | DUP14
  | DUP15
  | DUP16
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | SWAP5
  | SWAP6
  | SWAP7
  | SWAP8
  | SWAP9
  | SWAP10
  | SWAP11
  | SWAP12
  | SWAP13
  | SWAP14
  | SWAP15
  | SWAP16
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4
  (* These "tentative" instructions are not in the original EVM c++ code but are EIPs *)
  | JUMPTO (* tentative *)
  | JUMPIF (* tentative *)
  | JUMPSUB (* tentative *)
  | JUMPSUBV (* tentative *)
  | BEGINSUB (* tentative *)
  | BEGINDATA (* tentative *)
  | RETURNSUB (* tentative *)
  | PUTLOCAL (* tentative *)
  | GETLOCAL (* tentative *)
  | SLOADBYTES
  | SSTOREBYTES
  | SSIZE
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | CALLBLACKBOX
  | STATICCALL
  | CREATE2
  | TXEXECGAS
  | REVERT
  | INVALID
  | SELFDESTRUCT

type code = Instr of instr_code | Literal of literal | MissingOpcode of char
type bytecode = code list

exception WrongOpcode of char

let codeop (c : char) =
  match c with
  | '\x00' -> STOP
  | '\x01' -> ADD
  | '\x02' -> MUL
  | '\x03' -> SUB
  | '\x04' -> DIV
  | '\x05' -> SDIV
  | '\x06' -> MOD
  | '\x07' -> SMOD
  | '\x08' -> ADDMOD
  | '\x09' -> MULMOD
  | '\x0a' -> EXP
  | '\x0b' -> SIGNEXTEND
  | '\x10' -> LT
  | '\x11' -> GT
  | '\x12' -> SLT
  | '\x13' -> SGT
  | '\x14' -> EQ
  | '\x15' -> ISZERO
  | '\x16' -> AND
  | '\x17' -> OR
  | '\x18' -> XOR
  | '\x19' -> NOT
  | '\x1a' -> BYTE
  | '\x20' -> SHA3
  | '\x30' -> ADDRESS
  | '\x31' -> BALANCE
  | '\x32' -> ORIGIN
  | '\x33' -> CALLER
  | '\x34' -> CALLVALUE
  | '\x35' -> CALLDATALOAD
  | '\x36' -> CALLDATASIZE
  | '\x37' -> CALLDATACOPY
  | '\x38' -> CODESIZE
  | '\x39' -> CODECOPY
  | '\x3a' -> GASPRICE
  | '\x3b' -> EXTCODESIZE
  | '\x3c' -> EXTCODECOPY
  | '\x3d' -> RETURNDATASIZE
  | '\x3e' -> RETURNDATACOPY
  | '\x40' -> BLOCKHASH
  | '\x41' -> COINBASE
  | '\x42' -> TIMESTAMP
  | '\x43' -> NUMBER
  | '\x44' -> DIFFICULTY
  | '\x45' -> GASLIMIT
  | '\x50' -> POP
  | '\x51' -> MLOAD
  | '\x52' -> MSTORE
  | '\x53' -> MSTORE8
  | '\x54' -> SLOAD
  | '\x55' -> SSTORE
  | '\x56' -> JUMP
  | '\x57' -> JUMPI
  | '\x58' -> GETPC
  | '\x59' -> MSIZE
  | '\x5a' -> GAS
  | '\x5b' -> JUMPDEST
  | '\x60' -> PUSH1
  | '\x61' -> PUSH2
  | '\x62' -> PUSH3
  | '\x63' -> PUSH4
  | '\x64' -> PUSH5
  | '\x65' -> PUSH6
  | '\x66' -> PUSH7
  | '\x67' -> PUSH8
  | '\x68' -> PUSH9
  | '\x69' -> PUSH10
  | '\x6a' -> PUSH11
  | '\x6b' -> PUSH12
  | '\x6c' -> PUSH13
  | '\x6d' -> PUSH14
  | '\x6e' -> PUSH15
  | '\x6f' -> PUSH16
  | '\x70' -> PUSH17
  | '\x71' -> PUSH18
  | '\x72' -> PUSH19
  | '\x73' -> PUSH20
  | '\x74' -> PUSH21
  | '\x75' -> PUSH22
  | '\x76' -> PUSH23
  | '\x77' -> PUSH24
  | '\x78' -> PUSH25
  | '\x79' -> PUSH26
  | '\x7a' -> PUSH27
  | '\x7b' -> PUSH28
  | '\x7c' -> PUSH29
  | '\x7d' -> PUSH30
  | '\x7e' -> PUSH31
  | '\x7f' -> PUSH32
  | '\x80' -> DUP1
  | '\x81' -> DUP2
  | '\x82' -> DUP3
  | '\x83' -> DUP4
  | '\x84' -> DUP5
  | '\x85' -> DUP6
  | '\x86' -> DUP7
  | '\x87' -> DUP8
  | '\x88' -> DUP9
  | '\x89' -> DUP10
  | '\x8a' -> DUP11
  | '\x8b' -> DUP12
  | '\x8c' -> DUP13
  | '\x8d' -> DUP14
  | '\x8e' -> DUP15
  | '\x8f' -> DUP16
  | '\x90' -> SWAP1
  | '\x91' -> SWAP2
  | '\x92' -> SWAP3
  | '\x93' -> SWAP4
  | '\x94' -> SWAP5
  | '\x95' -> SWAP6
  | '\x96' -> SWAP7
  | '\x97' -> SWAP8
  | '\x98' -> SWAP9
  | '\x99' -> SWAP10
  | '\x9a' -> SWAP11
  | '\x9b' -> SWAP12
  | '\x9c' -> SWAP13
  | '\x9d' -> SWAP14
  | '\x9e' -> SWAP15
  | '\x9f' -> SWAP16
  | '\xa0' -> LOG0
  | '\xa1' -> LOG1
  | '\xa2' -> LOG2
  | '\xa3' -> LOG3
  | '\xa4' -> LOG4
  | '\xb0' -> JUMPTO
  | '\xb1' -> JUMPIF
  | '\xb2' -> JUMPSUB
  | '\xb4' -> JUMPSUBV
  | '\xb5' -> BEGINSUB
  | '\xb6' -> BEGINDATA
  | '\xb8' -> RETURNSUB
  | '\xb9' -> PUTLOCAL
  | '\xba' -> GETLOCAL
  | '\xe1' -> SLOADBYTES
  | '\xe2' -> SSTOREBYTES
  | '\xe3' -> SSIZE
  | '\xf0' -> CREATE
  | '\xf1' -> CALL
  | '\xf2' -> CALLCODE
  | '\xf3' -> RETURN
  | '\xf4' -> DELEGATECALL
  | '\xf5' -> CALLBLACKBOX
  | '\xfa' -> STATICCALL
  | '\xfb' -> CREATE2
  | '\xfc' -> TXEXECGAS
  | '\xfd' -> REVERT
  | '\xfe' -> INVALID
  | '\xff' -> SELFDESTRUCT
  | _ -> raise (WrongOpcode c)

let opcode = function
  | STOP -> 0x00
  | ADD -> 0x01
  | MUL -> 0x02
  | SUB -> 0x03
  | DIV -> 0x04
  | SDIV -> 0x05
  | MOD -> 0x06
  | SMOD -> 0x07
  | ADDMOD -> 0x08
  | MULMOD -> 0x09
  | EXP -> 0x0a
  | SIGNEXTEND -> 0x0b
  | LT -> 0x10
  | GT -> 0x11
  | SLT -> 0x12
  | SGT -> 0x13
  | EQ -> 0x14
  | ISZERO -> 0x15
  | AND -> 0x16
  | OR -> 0x17
  | XOR -> 0x18
  | NOT -> 0x19
  | BYTE -> 0x1a
  | SHA3 -> 0x20
  | ADDRESS -> 0x30
  | BALANCE -> 0x31
  | ORIGIN -> 0x32
  | CALLER -> 0x33
  | CALLVALUE -> 0x34
  | CALLDATALOAD -> 0x35
  | CALLDATASIZE -> 0x36
  | CALLDATACOPY -> 0x37
  | CODESIZE -> 0x38
  | CODECOPY -> 0x39
  | GASPRICE -> 0x3a
  | EXTCODESIZE -> 0x3b
  | EXTCODECOPY -> 0x3c
  | RETURNDATASIZE -> 0x3d
  | RETURNDATACOPY -> 0x3e
  | BLOCKHASH -> 0x40
  | COINBASE -> 0x41
  | TIMESTAMP -> 0x42
  | NUMBER -> 0x43
  | DIFFICULTY -> 0x44
  | GASLIMIT -> 0x45
  | POP -> 0x50
  | MLOAD -> 0x51
  | MSTORE -> 0x52
  | MSTORE8 -> 0x53
  | SLOAD -> 0x54
  | SSTORE -> 0x55
  | JUMP -> 0x56
  | JUMPI -> 0x57
  | GETPC -> 0x58
  | MSIZE -> 0x59
  | GAS -> 0x5a
  | JUMPDEST -> 0x5b
  | PUSH1 -> 0x60
  | PUSH2 -> 0x61
  | PUSH3 -> 0x62
  | PUSH4 -> 0x63
  | PUSH5 -> 0x64
  | PUSH6 -> 0x65
  | PUSH7 -> 0x66
  | PUSH8 -> 0x67
  | PUSH9 -> 0x68
  | PUSH10 -> 0x69
  | PUSH11 -> 0x6a
  | PUSH12 -> 0x6b
  | PUSH13 -> 0x6c
  | PUSH14 -> 0x6d
  | PUSH15 -> 0x6e
  | PUSH16 -> 0x6f
  | PUSH17 -> 0x70
  | PUSH18 -> 0x71
  | PUSH19 -> 0x72
  | PUSH20 -> 0x73
  | PUSH21 -> 0x74
  | PUSH22 -> 0x75
  | PUSH23 -> 0x76
  | PUSH24 -> 0x77
  | PUSH25 -> 0x78
  | PUSH26 -> 0x79
  | PUSH27 -> 0x7a
  | PUSH28 -> 0x7b
  | PUSH29 -> 0x7c
  | PUSH30 -> 0x7d
  | PUSH31 -> 0x7e
  | PUSH32 -> 0x7f
  | DUP1 -> 0x80
  | DUP2 -> 0x81
  | DUP3 -> 0x82
  | DUP4 -> 0x83
  | DUP5 -> 0x84
  | DUP6 -> 0x85
  | DUP7 -> 0x86
  | DUP8 -> 0x87
  | DUP9 -> 0x88
  | DUP10 -> 0x89
  | DUP11 -> 0x8a
  | DUP12 -> 0x8b
  | DUP13 -> 0x8c
  | DUP14 -> 0x8d
  | DUP15 -> 0x8e
  | DUP16 -> 0x8f
  | SWAP1 -> 0x90
  | SWAP2 -> 0x91
  | SWAP3 -> 0x92
  | SWAP4 -> 0x93
  | SWAP5 -> 0x94
  | SWAP6 -> 0x95
  | SWAP7 -> 0x96
  | SWAP8 -> 0x97
  | SWAP9 -> 0x98
  | SWAP10 -> 0x99
  | SWAP11 -> 0x9a
  | SWAP12 -> 0x9b
  | SWAP13 -> 0x9c
  | SWAP14 -> 0x9d
  | SWAP15 -> 0x9e
  | SWAP16 -> 0x9f
  | LOG0 -> 0xa0
  | LOG1 -> 0xa1
  | LOG2 -> 0xa2
  | LOG3 -> 0xa3
  | LOG4 -> 0xa4
  | JUMPTO -> 0xb0
  | JUMPIF -> 0xb1
  | JUMPSUB -> 0xb2
  | JUMPSUBV -> 0xb4
  | BEGINSUB -> 0xb5
  | BEGINDATA -> 0xb6
  | RETURNSUB -> 0xb8
  | PUTLOCAL -> 0xb9
  | GETLOCAL -> 0xba
  | SLOADBYTES -> 0xe1
  | SSTOREBYTES -> 0xe2
  | SSIZE -> 0xe3
  | CREATE -> 0xf0
  | CALL -> 0xf1
  | CALLCODE -> 0xf2
  | RETURN -> 0xf3
  | DELEGATECALL -> 0xf4
  | CALLBLACKBOX -> 0xf5
  | STATICCALL -> 0xfa
  | CREATE2 -> 0xfb
  | TXEXECGAS -> 0xfc
  | REVERT -> 0xfd
  | INVALID -> 0xfe
  | SELFDESTRUCT -> 0xff

module Ops = struct
  let push = function
    | 01 -> PUSH1
    | 02 -> PUSH2
    | 03 -> PUSH3
    | 04 -> PUSH4
    | 05 -> PUSH5
    | 06 -> PUSH6
    | 07 -> PUSH7
    | 08 -> PUSH8
    | 09 -> PUSH9
    | 10 -> PUSH10
    | 11 -> PUSH11
    | 12 -> PUSH12
    | 13 -> PUSH13
    | 14 -> PUSH14
    | 15 -> PUSH15
    | 16 -> PUSH16
    | 17 -> PUSH17
    | 18 -> PUSH18
    | 19 -> PUSH19
    | 20 -> PUSH20
    | 21 -> PUSH21
    | 22 -> PUSH22
    | 23 -> PUSH23
    | 24 -> PUSH24
    | 25 -> PUSH25
    | 26 -> PUSH26
    | 27 -> PUSH27
    | 28 -> PUSH28
    | 29 -> PUSH29
    | 30 -> PUSH30
    | 31 -> PUSH31
    | 32 -> PUSH32
    | i -> failwith ("invalid push depth " ^ string_of_int i)

  let dup = function
    | 01 -> DUP1
    | 02 -> DUP2
    | 03 -> DUP3
    | 04 -> DUP4
    | 05 -> DUP5
    | 06 -> DUP6
    | 07 -> DUP7
    | 08 -> DUP8
    | 09 -> DUP9
    | 10 -> DUP10
    | 11 -> DUP11
    | 12 -> DUP12
    | 13 -> DUP13
    | 14 -> DUP14
    | 15 -> DUP15
    | 16 -> DUP16
    | _ -> failwith "invalid dup depth"

  let pushdepth x =
    let c = opcode x in
    if 0x60 <= c && c <= 0x7f then c - 0x60 + 1
    else failwith "instruction not a PUSHn"

  (* let dupdepth x =
   *   let c = opcode x in
   *   if 0x80 <= c && c <= 0x8f then c - 0x80 + 1
   *   else failwith "instruction not a PUSHn" *)
end

let literal_of_int i =
  let res = Printf.sprintf "%x" i in
  if String.length res mod 2 = 0 then res else "0" ^ res

let literal_width i = String.length i / 2
let string_opcode x = Printf.sprintf "%.2x" (opcode x)

let string_of_bytecode = function
  | Instr code -> string_opcode code
  | Literal lit -> lit
  | MissingOpcode c ->
      let code = Char.code c in
      Printf.sprintf "%.2x" code

let length program =
  let rec length (program : bytecode) acc =
    match program with
    | [] -> acc
    | MissingOpcode _ :: tail | Instr _ :: tail -> length tail (acc + 1)
    | Literal l :: tail -> length tail (acc + (String.length l / 2))
    (* we need a "byte length of literal" function*) in
  length program 0

let dump (program : bytecode) =
  "0x" ^ String.concat "" (List.map string_of_bytecode program)

let bits_required positive_integer =
  let rec loop i acc = if i > 0 then loop (i lsr 1) (acc + 1) else acc in
  loop positive_integer 0

let bytes_required x =
  let bits = bits_required x in
  (bits / 8) + if bits mod 8 > 0 then 1 else 0

(* computes the lfp of an equation of the type X = Y + |X|
   where |X| is the number of bytes required to store X. Here
   |X| is constrained to be strictly positive and <= 32. *)
let solve_length_eq y =
  let rec loop x =
    let rhs = y + bytes_required x in
    if x < rhs then loop (x + 1)
    else if x = rhs then x
    else failwith "solve_length_eq: constant too large" in
  loop 1

let deploy (program : bytecode) =
  let length = length program in
  let lengthlit = literal_of_int length in
  let lengthlitw = literal_width lengthlit in
  (* X = push lengthlit (1 + |lengthlit| bytes)
       + dup1 (1 byte)
       + push where conract starts (quantity we're computing now) (1 + |X| bytes)
       + push where to store contract (0) (2 bytes)
       + codecopy (1 byte)
       + push address (0) (2 bytes)
       + return (1 byte)
     X = 9 + |lengthlit| + |X|
     |X| = |9 + |lengthlit| + |X||
  *)
  let prog_pos = literal_of_int (solve_length_eq (9 + lengthlitw)) in
  let prog_posw = literal_width prog_pos in
  let deploy_code =
    [ Instr (Ops.push lengthlitw); (* store the length of program *)
      Literal lengthlit; Instr DUP1; (* duplicate length of program *)
      Instr (Ops.push prog_posw); (* push program position in code *)
      Literal prog_pos; Instr (Ops.push 1); (* push 0, target in memory *)
      Literal (literal_of_int 0); Instr CODECOPY;
      (* copy code to memory (pos 0) *) Instr (Ops.push 1);
      (* return code position in memory (0) *) Literal (literal_of_int 0);
      Instr RETURN ] in
  deploy_code @ program

let parse_hexstring (str : Bitstr.Hex.t) =
  let compressed = Bitstr.(Bit.to_string (compress str)) in
  let rec loop bc acc =
    match bc with
    | [] -> List.rev acc
    | ('\x60' .. '\x7f' as code) :: tl ->
        (* push *)
        let code = codeop code in
        let depth = Ops.pushdepth code in
        let taken, rest = CCList.take_drop depth tl in
        let lit = String.of_seq (List.to_seq taken) in
        let (`Hex lit) = Hex.of_string lit in
        loop rest (Literal lit :: Instr code :: acc)
    | code :: tail ->
        let result =
          try Instr (codeop code) with WrongOpcode _ -> MissingOpcode code in
        loop tail (result :: acc) in
  loop (List.of_seq (String.to_seq compressed)) []

(* let deploy (program : bytecode) =
 *   let prog_len = length program in
 *   let lit      = literal_of_int prog_len in
 *   let lit_len  = String.length lit in
 *   [
 *     Instr (Ops.push lit_len);
 *     Literal lit;
 *   ] *)

(* PUSH1 0 CALLDATALOAD SLOAD NOT PUSH1 9 JUMPI STOP JUMPDEST PUSH1 32 CALLDATALOAD PUSH1 0 CALLDATALOAD SSTORE *)

(* let example =
 *   [ Instr PUSH1; Literal "00"; Instr CALLDATALOAD; Instr SLOAD; Instr NOT;
 *     Instr PUSH1; Literal "09"; Instr JUMPI; Instr STOP; Instr JUMPDEST;
 *     Instr PUSH1; Literal "20"; Instr CALLDATALOAD; Instr PUSH1; Literal "00";
 *     Instr CALLDATALOAD; Instr SSTORE ]
 * 
 * let bytecode = dump example *)
