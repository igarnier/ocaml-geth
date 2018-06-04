(* The EVM bytecodes "bytes" are in fact 256 bit (32 bytes) long, to facilitate SHA3 based
   operations. *)

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
  | JUMPSUB  (* tentative *)
  | JUMPSUBV  (* tentative *)
  | BEGINSUB  (* tentative *)
  | BEGINDATA  (* tentative *)
  | RETURNSUB  (* tentative *)
  | PUTLOCAL  (* tentative *)
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

type code =
  | Instr of instr_code
  | Literal of literal

type bytecode = code list

let opcode = function
  | STOP ->  0x00
  | ADD ->  0x01
  | MUL ->  0x02
  | SUB ->  0x03
  | DIV ->  0x04
  | SDIV ->  0x05
  | MOD ->  0x06
  | SMOD ->  0x07
  | ADDMOD ->  0x08
  | MULMOD ->  0x09
  | EXP ->  0x0a
  | SIGNEXTEND ->  0x0b
  | LT ->  0x10
  | GT ->  0x11
  | SLT ->  0x12
  | SGT ->  0x13
  | EQ ->  0x14
  | ISZERO ->  0x15
  | AND ->  0x16
  | OR ->  0x17
  | XOR ->  0x18
  | NOT ->  0x19
  | BYTE ->  0x1a
  | SHA3 -> 0x20
  | ADDRESS ->  0x30
  | BALANCE ->  0x31
  | ORIGIN ->  0x32
  | CALLER ->  0x33
  | CALLVALUE ->  0x34
  | CALLDATALOAD ->  0x35
  | CALLDATASIZE ->  0x36
  | CALLDATACOPY ->  0x37
  | CODESIZE ->  0x38
  | CODECOPY ->  0x39
  | GASPRICE ->  0x3a
  | EXTCODESIZE ->  0x3b
  | EXTCODECOPY ->  0x3c
  | RETURNDATASIZE ->  0x3d
  | RETURNDATACOPY ->  0x3e
  | BLOCKHASH ->  0x40
  | COINBASE ->  0x41
  | TIMESTAMP ->  0x42
  | NUMBER ->  0x43
  | DIFFICULTY ->  0x44
  | GASLIMIT ->  0x45
  | POP ->  0x50
  | MLOAD ->  0x51
  | MSTORE ->  0x52
  | MSTORE8 ->  0x53
  | SLOAD ->  0x54
  | SSTORE ->  0x55
  | JUMP ->  0x56
  | JUMPI ->  0x57
  | GETPC ->  0x58
  | MSIZE ->  0x59
  | GAS ->  0x5a
  | JUMPDEST ->  0x5b
  | PUSH1 ->  0x60 
  | PUSH2 ->  0x61 
  | PUSH3 ->  0x62 
  | PUSH4 ->  0x63 
  | PUSH5 ->  0x64 
  | PUSH6 ->  0x65 
  | PUSH7 ->  0x66 
  | PUSH8 ->  0x67 
  | PUSH9 ->  0x68 
  | PUSH10 ->  0x69 
  | PUSH11 ->  0x6a 
  | PUSH12 ->  0x6b 
  | PUSH13 ->  0x6c 
  | PUSH14 ->  0x6d 
  | PUSH15 ->  0x6e 
  | PUSH16 ->  0x6f 
  | PUSH17 ->  0x70 
  | PUSH18 ->  0x71 
  | PUSH19 ->  0x72 
  | PUSH20 ->  0x73 
  | PUSH21 ->  0x74 
  | PUSH22 ->  0x75 
  | PUSH23 ->  0x76 
  | PUSH24 ->  0x77 
  | PUSH25 ->  0x78 
  | PUSH26 ->  0x79 
  | PUSH27 ->  0x7a 
  | PUSH28 ->  0x7b 
  | PUSH29 ->  0x7c 
  | PUSH30 ->  0x7d 
  | PUSH31 ->  0x7e 
  | PUSH32 ->  0x7f 
  | DUP1 ->  0x80 
  | DUP2 ->  0x81 
  | DUP3 ->  0x82 
  | DUP4 ->  0x83 
  | DUP5 ->  0x84 
  | DUP6 ->  0x85 
  | DUP7 ->  0x86 
  | DUP8 ->  0x87 
  | DUP9 ->  0x88 
  | DUP10 ->  0x89 
  | DUP11 ->  0x8a 
  | DUP12 ->  0x8b 
  | DUP13 ->  0x8c 
  | DUP14 ->  0x8d 
  | DUP15 ->  0x8e 
  | DUP16 ->  0x8f 
  | SWAP1 ->  0x90 
  | SWAP2 ->  0x91 
  | SWAP3 ->  0x92 
  | SWAP4 ->  0x93 
  | SWAP5 ->  0x94 
  | SWAP6 ->  0x95 
  | SWAP7 ->  0x96 
  | SWAP8 ->  0x97 
  | SWAP9 ->  0x98 
  | SWAP10 ->  0x99 
  | SWAP11 ->  0x9a 
  | SWAP12 ->  0x9b 
  | SWAP13 ->  0x9c 
  | SWAP14 ->  0x9d 
  | SWAP15 ->  0x9e 
  | SWAP16 ->  0x9f 
  | LOG0 ->  0xa0 
  | LOG1 ->  0xa1 
  | LOG2 ->  0xa2 
  | LOG3 ->  0xa3 
  | LOG4 ->  0xa4 
  | JUMPTO ->  0xb0
  | JUMPIF ->  0xb1
  | JUMPSUB ->  0xb2
  | JUMPSUBV ->  0xb4
  | BEGINSUB ->  0xb5
  | BEGINDATA ->  0xb6
  | RETURNSUB ->  0xb8
  | PUTLOCAL ->  0xb9
  | GETLOCAL ->  0xba
  | SLOADBYTES ->  0xe1
  | SSTOREBYTES ->  0xe2
  | SSIZE ->  0xe3
  | CREATE ->  0xf0
  | CALL ->  0xf1
  | CALLCODE ->  0xf2
  | RETURN ->  0xf3
  | DELEGATECALL ->  0xf4
  | CALLBLACKBOX ->  0xf5 
  | STATICCALL ->  0xfa
  | CREATE2 -> 0xfb
  | TXEXECGAS ->  0xfc
  | REVERT ->  0xfd
  | INVALID ->  0xfe
  | SELFDESTRUCT ->  0xff

module Ops =
struct

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
    | i ->
      failwith ("invalid push depth "^(string_of_int i))

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
    | _ ->
      failwith "invalid dup depth"
  
end

let literal_of_int i =
  let res = Printf.sprintf "%x" i in
  if String.length res mod 2 = 0 then
    res
  else
    "0"^res

let literal_width i =
  (String.length i)/2

let string_opcode x =
  Printf.sprintf "%.2x" (opcode x)

let string_of_bytecode = function
  | Instr code  -> string_opcode code
  | Literal lit -> lit

let length program =
  let rec length (program : bytecode) acc =
    match program with
    | [] -> acc
    | (Instr _) :: tail   -> length tail (acc + 1)
    | (Literal l) :: tail -> length tail (acc + (String.length l))
  in
  length program 0

let dump (program : bytecode) =
  String.concat "" (List.map string_of_bytecode program)

(* let deploy (program : bytecode) =
 *   let prog_len = length program in
 *   let lit      = literal_of_int prog_len in
 *   let lit_len  = String.length lit in
 *   [
 *     Instr (Ops.push lit_len);
 *     Literal lit;
 *   ] *)


(* PUSH1 0 CALLDATALOAD SLOAD NOT PUSH1 9 JUMPI STOP JUMPDEST PUSH1 32 CALLDATALOAD PUSH1 0 CALLDATALOAD SSTORE *)

let example =
  [
    Instr PUSH1;
    Literal "00";
    Instr CALLDATALOAD;
    Instr SLOAD;
    Instr NOT;
    Instr PUSH1;
    Literal "09";
    Instr JUMPI;
    Instr STOP;
    Instr JUMPDEST;
    Instr PUSH1;
    Literal "20";
    Instr CALLDATALOAD;
    Instr PUSH1;
    Literal "00";
    Instr CALLDATALOAD;
    Instr SSTORE
  ]

let bytecode = dump example
