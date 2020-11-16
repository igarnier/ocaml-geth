open CCFun
open Geth
open Types

module Genesis = struct
  (** See the following URL for more detail.
      https://gist.github.com/0mkara/b953cc2585b18ee098cd#file-genesis-md
  *)

  type config =
    { chain_id: int;
      (* block from which the protocol will be "homestead" *)
      homestead_block: block_id;
      (* https://github.com/ethereum/EIPs/issues/155 *)
      eip155_block: block_id;
      (* https://github.com/ethereum/EIPs/issues/158 *)
      eip158_block: block_id
          (* ... there are probably other such ad-hoc parameters, cf the DAO *)
    }

  type t =
    { config: config;  (** Protocol options. *)
      alloc: (Address.t * wei) list;
          (** List of accounts with pre-allocated money.  *)
      coinbase: Address.t;
          (** The 160-bit address to which all rewards (in Ether) collected from the
        successful mining of this block have been transferred. They are a sum of
        the mining eward itself and the Contract transaction execution refunds.
        Often named "beneficiary" in the specifications, sometimes "etherbase"
        in the online documentation. This can be anything in the Genesis Block
        since the value is set by the setting of the Miner when a new Block is
        created. *)
      difficulty: int;
          (** A scalar value corresponding to the difficulty level applied during the
        nonce discovering of this block. It defines the Mining Target, which
        can be calculated from the previous block’s difficulty level and the
        timestamp. The higher the difficulty, the statistically more
        calculations a Miner must perform to discover a valid block. This value
        is used to control the Block generation time of a Blockchain, keeping
        the Block generation frequency within a target range. On the test
        network, we keep this value low to avoid waiting during tests since the
        discovery of a valid Block is required to execute a transaction on the
        Blockchain.
    *)
      extra_data: string;  (** /!\ max 32 byte long /!\, arbitrary data. *)
      gas_limit: int;
          (** A scalar value equal to the current chain-wide limit of Gas expenditure
        per block. High in our case to avoid being limited by this threshold
        during tests. Note: this does not indicate that we should not pay
        attention to the Gas consumption of our Contracts. *)
      nonce: int;
          (** A scalar value equal to the number of transactions sent by the sender.
        A 64-bit hash which proves combined with the mix-hash that a
        sufficient amount of computation has been carried out on this block.
        The nonce is the cryptographically secure mining proof-of-work that
        proves beyond reasonable doubt that a particular amount of computation
        has been expended in the determination of this token value.
        (Yellowpager, 11.5. Mining Proof-of-Work).
        The final nonce value is the result of the the mining process iteration,
        in which the algorithm was able to discover a nonce value that satisfies
        the Mining Target. Just by using the nonce Proof-of-Work, the validity
        of a Block can verified very quickly.
    *)
      mix_hash: Hash256.t;
      parent_hash: Hash256.t;
          (** The Keccak256 hash of the entire parent block’s header (including
        its nonce and mixhash). Pointer to the parent block, thus effectively
        building the chain of blocks. In the case of the Genesis block, and only
        in this case, it's 0. *)
      (* timestamp of original block in seconds from Epoch *)
      timestamp: int }

  let hex_of_int i = Printf.sprintf "0x%x" i

  let to_json : t -> Yojson.Basic.t =
   fun block ->
    let config =
      ( "config",
        `Assoc
          [ ("chainId", `Int block.config.chain_id);
            ("homesteadBlock", `Int block.config.homestead_block);
            ("eip155Block", `Int block.config.eip155_block);
            ("eip158Block", `Int block.config.eip158_block) ] ) in
    let alloc =
      let balances =
        List.map
          (fun (addr, wei) ->
            let addr = Address.show addr in
            let bal = string_of_int wei in
            (addr, `Assoc [("balance", `String bal)]))
          block.alloc in
      ("alloc", `Assoc balances) in
    `Assoc
      [ config; alloc; ("coinbase", `String (Address.show block.coinbase));
        ("difficulty", `String (hex_of_int block.difficulty));
        ("extraData", `String block.extra_data);
        ("gasLimit", `String (hex_of_int block.gas_limit));
        ("nonce", `String (hex_of_int block.nonce));
        ("mixhash", `String (Hash256.show block.mix_hash));
        ("parentHash", `String (Hash256.show block.parent_hash));
        ("timestamp", `String (hex_of_int block.timestamp)) ]
end

(* Initializing an Ethereum network with Geth

   1. start bootnode -> get its enode address
   2. for each machine where we want to run a node:
      a. populate it with the genesis.json, a data dir and a source dir
      b. run the client with the good parameters (port & etc) and
         the enode address of the bootnode
*)

(* -------------------------------------------------------------------------- *)
(* Geth specific shell commands *)

module Geth = struct
  open Printf

  let bootnode_genkey bootnode_key =
    Shell.of_string (Printf.sprintf "bootnode --genkey=%s" bootnode_key)

  let bootnode_nodekey addr bootnode_key =
    let command =
      Printf.sprintf "bootnode --nodekey=%s -addr :%d" bootnode_key addr in
    Shell.of_string command

  type geth_option =
    | Datadir of string (* chain location *)
    | Bootnodes of string list (* enode list *)
    | NetworkId of int
    | Verbosity of int
    | Nodiscover
    | Exec of string (* javascript statement *)
    | IpcPath of string (* foo/bar/baz.ipc *)
    | Rpc of {rpcport: int; rpcaddr: string; rpcapis: string}
    | Port of int (* p2p port *)
    | DagDir of string

  type command =
    | Init of string
    (* json file *)
    | Attach of string option

  let string_of_option = function
    | Datadir s -> sprintf "--datadir \"%s\"" s
    | Bootnodes ls ->
        let enodes = ls |> List.map (sprintf "\"%s\"") |> String.concat "," in
        sprintf "--bootnodes %s" enodes
    | NetworkId id -> sprintf "--networkid %d" id
    | Verbosity d -> sprintf "--verbosity %d" d
    | Nodiscover -> "--nodiscover"
    | Exec s -> sprintf "--exec \"%s\"" s
    | IpcPath s -> sprintf "--ipcpath \"%s\"" s
    | Rpc {rpcport; rpcaddr; rpcapis} ->
        sprintf "--rpc --rpcaddr %s --rpcport %d --rpcapi %s" rpcaddr rpcport
          rpcapis
    | Port port -> sprintf "--port %d" port
    | DagDir path -> sprintf "--ethash.dagdir \"%s\"" path

  let make (options : geth_option list) (command : command option) =
    let option_str = options |> List.map string_of_option |> String.concat " " in
    let command_str =
      match command with
      | None -> ""
      | Some (Init json_file) -> sprintf "init \"%s\"" json_file
      | Some (Attach ipc_addr) -> (
        match ipc_addr with
        | None -> "attach"
        | Some ipc_addr -> sprintf "attach %s" ipc_addr ) in
    Shell.of_string (sprintf "geth %s %s" option_str command_str)
end

type geth_config =
  { genesis_block: Genesis.t;
    network_id: int;
    root_directory: string;
    data_subdir: string;
    source_subdir: string;
    rpc_port: int;
    p2p_port: int }

type deploy_target =
  {ip_address: string; ssh_port: int; login: string; password: string}

type network = deploy_target list

exception Deploy_error of deploy_target

(* Helper functions *)

(* Execute a remote command and display the result locally *)
let log_exec ?read_stderr ?read_timeout shell command =
  let res = Shell.execute ?read_stderr ?read_timeout shell command in
  Printf.eprintf "# %s > %s\n" (Shell.to_string command) res

(* Execute a remote command, display and return the result locally *)
let log_exec_return ?read_stderr ?read_timeout shell command =
  let res = Shell.execute ?read_stderr ?read_timeout shell command in
  Printf.eprintf "# %s > %s\n" (Shell.to_string command) res ;
  res

(* Execute a remote command, display the result and return the code *)
let log_exec_code ?read_stderr ?read_timeout shell command =
  let res =
    let open Shell in
    execute ?read_stderr ?read_timeout shell (seq command (of_string "$?"))
  in
  Printf.eprintf "# %s > %s\n" (Shell.to_string command) res ;
  try int_of_string res
  with Failure _ as e ->
    Printf.eprintf "log_exec_code: cannot convert \"%s\" to integer code" res ;
    raise e

(* -------------------------------------------------------------------------- *)
(* Functions pertaining to configuration of the hosts *)
(* -------------------------------------------------------------------------- *)

(* The genesis block specification is converted to json then to string, then
   to a local file, then sent via scp to the remote host. *)
let write_genesis genesis_block root_dir mode session =
  let local_genesis = Filename.temp_file "genesis" ".json" in
  CCIO.with_out ~flags:[Open_creat; Open_text] ~mode:0o644 local_genesis
    (fun oc ->
      let json_str =
        genesis_block |> Genesis.to_json |> Yojson.Basic.to_string in
      output_string oc json_str) ;
  Ssh_client.Easy.scp ~session ~src_path:local_genesis
    ~dst_path:(Filename.concat root_dir "genesis.json")
    ~mode

(* Login to a target and pass the resulting session to a provided function *)
let login_target target f =
  let open Ssh_client in
  let options =
    { Easy.host= target.ip_address;
      username= target.login;
      port= target.ssh_port;
      log_level= Types.SSH_LOG_WARNING } in
  Easy.with_password ~options ~password:target.password f

(* Parse enode back *)
let parse_enode raw_enode =
  let enode_start =
    try Str.search_forward (Str.regexp_string "enode") raw_enode 0
    with Not_found -> failwith ("Enode not found in " ^ raw_enode) in
  let enode_addr = Str.string_after raw_enode enode_start in
  let enode_end =
    try Str.search_forward (Str.regexp_string "?discport=0") enode_addr 0
    with Not_found -> String.length enode_addr in
  let enode_addr = Str.string_before enode_addr enode_end in
  (* Remove trailing "\n" if any *)
  Option.value ~default:enode_addr (CCString.chop_suffix ~suf:"\n" enode_addr)

(* Returns the process that runs on [port] on the remote host, if any.
   This should be MacOS-ok. *)
let port_is_free shell port =
  let command =
    Printf.sprintf "lsof -n -i:%d | cut -d \" \" -f1 | tail -n 1" port in
  let command = Shell.of_string command in
  let result = Shell.execute ~read_timeout:300 shell command in
  CCString.is_empty result

let add_peers (geth_config, target) peers =
  let add_peer enode =
    let rpc_addr = Printf.sprintf "http://localhost:%d" geth_config.rpc_port in
    let js_cmd = Printf.sprintf "admin.addPeer(%s)" enode in
    Geth.(make [Exec js_cmd] (Some (Attach (Some rpc_addr)))) in
  login_target target (fun session ->
      Ssh_client.Easy.with_shell_channel ~session (fun shell greet_string ->
          List.iter (log_exec shell % add_peer) peers))

let revert_deploy (geth_config, target) =
  login_target target (fun session ->
      Ssh_client.Easy.with_shell_channel ~session (fun shell greet_string ->
          (* I should rather get the PIDs of the running processes,
             this is just ugly. *)
          log_exec shell (Shell.of_string "killall geth") ;
          log_exec shell (Shell.rm_fr geth_config.root_directory)))

let prepare_target =
  let tools_are_available shell =
    let screen_res = log_exec_return shell (Shell.exists "screen") in
    let geth_res = log_exec_return shell (Shell.exists "geth") in
    screen_res <> "" && geth_res <> "" in
  let create_directories geth_cfg shell =
    let exec = log_exec shell in
    List.iter exec
      [ (* this is to print the shell - just a useful debug info *)
        Shell.of_string "echo -n $0"; Shell.mkdir geth_cfg.root_directory;
        Shell.cd geth_cfg.root_directory; Shell.mkdir geth_cfg.data_subdir;
        Shell.mkdir geth_cfg.source_subdir; Shell.touch "geth.ipc" ] in
  let geth_init geth_cfg =
    Geth.(make [Datadir geth_cfg.data_subdir] (Some (Init "genesis.json")))
  in
  let geth_start geth_cfg =
    Printf.eprintf "Starting Geth node...\n" ;
    (* ipc_file is used to attach the console *)
    let ipc_file = "geth.ipc" in
    let cmd =
      Geth.(
        make
          [ Datadir geth_cfg.data_subdir; Nodiscover;
            NetworkId geth_cfg.network_id; Verbosity 6; IpcPath ipc_file;
            Rpc
              { rpcport= geth_cfg.rpc_port;
                rpcaddr= "localhost";
                rpcapis= "admin,web3,eth,personal,miner,net,txpool" };
            Port geth_cfg.p2p_port (* DagDir "ethash" *) ]
          None) in
    Shell.screen cmd in
  let get_enode geth_cfg =
    let rpc_addr = Printf.sprintf "http://localhost:%d" geth_cfg.rpc_port in
    Geth.(make [Exec "admin.nodeInfo.enode"] (Some (Attach (Some rpc_addr))))
  in
  fun (geth_cfg, target) ->
    Printf.eprintf "Configuring target %s@%s\n%!" target.login target.ip_address ;
    let enode =
      login_target target (fun session ->
          Ssh_client.Easy.with_shell_channel ~session (fun shell greet_string ->
              Printf.eprintf
                "Checking whether remote host has required capabilities...\n" ;
              if not (tools_are_available shell) then
                raise (Deploy_error target) ;
              if not (port_is_free shell geth_cfg.p2p_port) then
                raise (Deploy_error target) ;
              Printf.eprintf "Cleaning existing Geth artifacts...\n" ;
              log_exec shell (Shell.of_string "killall geth") ;
              log_exec shell (Shell.rm_fr geth_cfg.root_directory) ;
              log_exec shell (Shell.rm_fr "~/.ethereum") ;
              Printf.eprintf "Creating directories...\n" ;
              create_directories geth_cfg shell ;
              Printf.eprintf "Uploading genesis file...\n" ;
              write_genesis geth_cfg.genesis_block geth_cfg.root_directory 0o640
                session ;
              Printf.eprintf "Initializing and starting Geth...\n" ;
              log_exec shell (geth_init geth_cfg) ;
              log_exec ~read_stderr:true shell (geth_start geth_cfg) ;
              log_exec_return ~read_timeout:500 shell (get_enode geth_cfg)))
    in
    (* remove initial and trailing noise *)
    let enode = parse_enode enode in
    (* substitute ip address back in enode *)
    Str.replace_first (Str.regexp_string "[::]") target.ip_address enode

let configure_step network =
  try List.map prepare_target network with
  | Deploy_error target ->
      Printf.eprintf "Configuration failed for host %s - aborting\n%!"
        target.ip_address ;
      List.iter revert_deploy network ;
      exit 1
  | exn ->
      Printf.eprintf
        "Unexpected exception caught during network configuration, aborting\n%!" ;
      List.iter revert_deploy network ;
      raise exn

let startup_step network map =
  try
    List.iter
      (fun (target, enode) ->
        let all_peers_except_target =
          map |> List.filter (fun (target', _) -> target' <> target) in
        add_peers target (List.map snd all_peers_except_target))
      map
  with Deploy_error target ->
    Printf.eprintf "Adding peers failed for host %s - aborting\n%!"
      target.ip_address ;
    (* List.iter (revert_add_peers geth_config) network; *)
    List.iter revert_deploy network ;
    exit 1

let deploy network =
  let enodes = configure_step network in
  Printf.printf "All nodes successfuly configured. Inode map:\n%!" ;
  let map = List.combine network enodes in
  List.iter
    (fun ((_, target), enode) ->
      Printf.printf "%s -> %s\n" target.ip_address enode)
    map ;
  Printf.printf "Adding peers...\n%!" ;
  startup_step network map
