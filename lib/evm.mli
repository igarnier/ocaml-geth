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

module Ops : sig
  val push : int -> instr_code
  val dup : int -> instr_code
end

val parse_hexstring : Bitstr.Hex.t -> bytecode
val literal_of_int : int -> literal
val literal_width : literal -> int
val length : bytecode -> int
val dump : bytecode -> string
val deploy : bytecode -> bytecode
