open Batteries
open Ssh_client

(* An Ethereum address corresponding to a private key k_r is the 
   rightmost truncation to 160 bit of a 256 bit Keccak hash
   of the corresponding ECDSA public key. Cf Yellow Paper. *)
type address   = string
type long_hash = string
type wei       = int
type block_id  = int

module Genesis =
struct

  (** See the following URL for more detail.
      https://gist.github.com/0mkara/b953cc2585b18ee098cd#file-genesis-md 
  *)

  type config = {
    chain_id        : int;

    homestead_block : block_id;  (* block from which the protocol will be "homestead" *)
    eip155_block    : block_id;  (* https://github.com/ethereum/EIPs/issues/155 *)
    eip158_block    : block_id;  (* https://github.com/ethereum/EIPs/issues/158 *)
    (* ... there are probably other such ad-hoc parameters, cf the DAO *)
  }    


  type t = {

    (** Protocol options. *)
    config : config;

    (** List of accounts with pre-allocated money.  *)
    alloc : (address * wei) list;

    (** The 160-bit address to which all rewards (in Ether) collected from the
        successful mining of this block have been transferred. They are a sum of
        the mining eward itself and the Contract transaction execution refunds. 
        Often named "beneficiary" in the specifications, sometimes "etherbase" 
        in the online documentation. This can be anything in the Genesis Block 
        since the value is set by the setting of the Miner when a new Block is
        created. *)
    coinbase : address;

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
    difficulty : int;

    (** /!\ max 32 byte long /!\, arbitrary data. *)
    extra_data : string;


    (** A scalar value equal to the current chain-wide limit of Gas expenditure
        per block. High in our case to avoid being limited by this threshold
        during tests. Note: this does not indicate that we should not pay 
        attention to the Gas consumption of our Contracts. *)
    gas_limit : int;

    (** Parameters of Dagger-Hashimoto *)

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
    nonce : int;
    mix_hash : long_hash;

    (** The Keccak256 hash of the entire parent block’s header (including 
        its nonce and mixhash). Pointer to the parent block, thus effectively 
        building the chain of blocks. In the case of the Genesis block, and only
        in this case, it's 0. *)
    parent_hash : long_hash;

    (* timestamp of original block in seconds from Epoch *)
    timestamp : int
  }


  let to_json block =
    let config = ("config", `Assoc [
        ("chainId", `Int block.config.chain_id);
        ("homesteadBlock", `Int block.config.homestead_block);
        ("eip155Block", `Int block.config.eip155_block);
        ("eip158Block", `Int block.config.eip158_block)
      ]) in
    let alloc =
      let balances = List.map (fun (addr, wei) ->
          (addr, `Assoc [ ("balance", `String (string_of_int wei)) ])
        ) block.alloc
      in
      ("alloc", `Assoc balances)
    in
    `Assoc [
      config;
      alloc;
      ("coinbase", `String block.coinbase);
      ("difficulty", `String (Utils.hex_of_int block.difficulty));
      ("extraData", `String block.extra_data);
      ("gasLimit", `String (Utils.hex_of_int block.gas_limit));
      ("nonce", `String (Utils.hex_of_int block.nonce));
      ("mixhash", `String block.mix_hash);
      ("parentHash", `String block.parent_hash);
      ("timestamp", `String (Utils.hex_of_int block.timestamp))
    ]

end

type geth_config = {
  genesis_block  : Genesis.t;
  network_id     : int;
  root_directory : string;
  data_subdir    : string;
  source_subdir  : string;
}

(* Initializing an Ethereum network with Geth

   1. start bootnode -> get its enode address
   2. for each machine where we want to run a node:
      a. populate it with the genesis.json, a data dir and a source dir
      b. run the client with the good parameters (port & etc) and
         the enode address of the bootnode
*)


(* -------------------------------------------------------------------------- *)
(* Geth specific shell commands *)

let (//) = Filename.concat

module Geth =
struct

  open Printf


  let bootnode_genkey bootnode_key =
    Shell.of_string (Printf.sprintf "bootnode --genkey=%s" bootnode_key)

  let bootnode_nodekey addr bootnode_key =
    let command =
      Printf.sprintf "bootnode --nodekey=%s -addr :%d" bootnode_key addr
    in
    Shell.of_string command


  type geth_option =
    | Datadir of string (* chain location *)
    | Bootnodes of string list (* enode list *)
    | NetworkId of int
    | Verbosity of int
    | Nodiscover
    | Exec of string (* javascript statement *)
    | IpcPath of string (* foo/bar/baz.ipc *)

  type command =
    | Init of string (* json file *)
    | Attach

  let string_of_option = function
    | Datadir s    -> sprintf "--datadir \"%s\"" s
    | Bootnodes ls ->
      let enodes =
        ls |> List.map (sprintf "\"%s\"") |> String.concat ","
      in
      sprintf "--bootnodes %s" enodes
    | NetworkId id ->
      sprintf "--networkid %d" id
    | Verbosity d ->
      sprintf "--verbosity %d" d
    | Nodiscover -> "--nodiscover"
    | Exec s ->
      sprintf "--exec \"%s\"" s
    | IpcPath s ->
      sprintf "--ipcpath \"%s\"" s
        
  let make (options : geth_option list) (command : command option) =
    let option_str =
      options |> List.map string_of_option |> String.concat " "
    in
    let command_str =
      match command with
      | None -> ""
      | Some (Init json_file)->
        sprintf "init \"%s\"" json_file
      | Some Attach ->
        "attach"
    in
    Shell.of_string (sprintf "geth %s %s" option_str command_str)

end

type deploy_target = {
  ip_address : string;
  ssh_port   : int;
  login      : string;
  password   : string
}

type network = deploy_target list

exception Deploy_error of deploy_target


(* Helper functions *)

let log_exec shell command =
  let res = Shell.execute shell command in
  Printf.eprintf "# %s > %s\n" (Shell.to_string command) res

let log_exec_return shell command =
  let res = Shell.execute shell command in
  Printf.eprintf "# %s > %s\n" (Shell.to_string command) res;
  res


(* let start_node ~target ~geth_conf ~boot_enode =
 *   Easy.with_shell_channel ~session (fun shell greet_string ->
 *       let exec = log_exec shell in
 *       List.iter exec [
 *         Shell.mkdir geth_conf.root_directory;
 *         Shell.cd geth_conf.root_directory;
 *         Shell.of_string "pwd";
 *         Shell.mkdir geth_conf.data_subdir;
 *         Shell.mkdir geth_conf.source_subdir
 *       ]
 *     ) ssh_session;
 *   write_genesis conf.genesis_block conf.root_directory 0o666 ssh_session;
 *   Easy.with_shell_channel (fun channel ->
 *       Shell.flush_stdout channel;
 *       let exec = log_exec channel in
 *       List.iter exec [
 *         Shell.cd conf.root_directory;
 *         geth_init conf.data_subdir;
 *         Shell.of_string "pwd";
 *         geth_start boot_enode
 *       ]
 *     ) ssh_session *)

(* -------------------------------------------------------------------------- *)
(* Functions pertaining to configuration of the hosts *)
(* -------------------------------------------------------------------------- *)

(* The genesis block specification is converted to json then to string, then
   to a local file, then sent via scp to the remote host. *)

let write_genesis genesis_block root_dir mode session =
  let local_genesis = Filename.temp_file "genesis" ".json" in
  File.with_file_out
    ~mode:[`create;`text]
    ~perm:(File.unix_perm mode) local_genesis (fun fd ->
        let json_str = genesis_block |> Genesis.to_json |> Yojson.to_string in
        output_string fd json_str
      );
  Easy.scp
    ~session
    ~src_path:local_genesis
    ~dst_path:(root_dir // "genesis.json")
    ~mode

(* let start_node ~session ~conf ~boot_enode =
 *   Easy.with_shell_channel ~session (fun channel greet_string  ->
 *       let exec = log_exec channel in
 *       List.iter exec [
 *         Shell.mkdir conf.root_directory;
 *         Shell.cd conf.root_directory;
 *         Shell.of_string "pwd";
 *         Shell.mkdir conf.data_subdir;
 *         Shell.mkdir conf.source_subdir
 *       ]
 *     );
 *   write_genesis conf.genesis_block conf.root_directory 0o666 session;
 *   Easy.with_shell (fun channel ->
 *       Shell.flush_stdout channel;
 *       let exec = log_exec channel in
 *       List.iter exec [
 *         Shell.cd conf.root_directory;
 *         geth_init conf.data_subdir;
 *         Shell.of_string "pwd";
 *         geth_start boot_enode
 *       ]
 *     ) ssh_session *)

let login_target target f =
  let options =
    {
      Easy.host = target.ip_address;
      username  = target.login;
      port      = target.ssh_port;
      log_level = Types.SSH_LOG_WARNING
    } in
  Easy.with_password ~options ~password:target.password f

let start_no_bootnode geth_cfg target =
  let create_directories shell =
    let exec = log_exec shell in
    List.iter exec [
      Shell.mkdir geth_cfg.root_directory;
      Shell.cd geth_cfg.root_directory;
      Shell.of_string "pwd";
      Shell.mkdir geth_cfg.data_subdir;
      Shell.mkdir geth_cfg.source_subdir
    ]
  in
  let init =
    Geth.(make [Datadir geth_cfg.data_subdir] (Some (Init "genesis.json")))
  in
  let boot =
    Geth.(make [Datadir geth_cfg.data_subdir;
                Nodiscover;
                NetworkId geth_cfg.network_id;
                Verbosity 6] None)
  in
  let get_enode =
    Geth.(make [Exec "admin.nodeInfo.enode"] (Some Attach))
  in
  login_target target (fun session ->
      Easy.with_shell_channel ~session (fun shell greet_string ->
          create_directories shell;
          write_genesis geth_cfg.genesis_block geth_cfg.root_directory 0o640 session;
          log_exec shell init;
          log_exec shell boot;
          log_exec shell (Shell.of_string "ls");
          log_exec_return shell get_enode
        )
    )

let add_peers _ _ = ()

let revert_configure _ _ = ()

let revert_add_peers _ _ = ()

let deploy geth_config network =
  let enodes =
    try
      List.map (start_no_bootnode geth_config) network
    with
    | Deploy_error target ->
      begin
        Printf.eprintf "Configuration failed for host %s - aborting\n%!" target.ip_address;
        List.iter (revert_configure geth_config) network;
        exit 1
      end
    | _ ->
      failwith "Unexpected exception caught during network configuration - aborting\n%!"
  in
  Printf.printf "All nodes successfuly configured. Inode map:\n%!";
  let map = List.combine network enodes in
  List.iter (fun (target, enode) ->
      Printf.printf "%s -> %s\n" target.ip_address enode
    ) map;
  Printf.printf "Adding peers...\n%!";
  try
    List.iter (fun (target, enode) ->
        let all_peers_except_target =
          map
          |> List.filter (fun (target', _) -> (target' <> target))
        in
        add_peers target all_peers_except_target
      ) map
  with
  | Deploy_error target ->
    begin
      Printf.eprintf "Adding peers failed for host %s - aborting\n%!" target.ip_address;      
      List.iter (revert_add_peers geth_config) network;
      List.iter (revert_configure geth_config) network;
      exit 1
    end
    
(* let geth_init datadir =
 *   let command =
 *     Printf.sprintf "geth --datadir=\"%s\" init \"genesis.json\"" datadir
 *   in
 *   Shell.of_string command
 * 
 * let geth_start ?(verbosity=6) boot_enode =
 *   let command =
 *     Printf.sprintf "geth --bootnodes=\"%s\" --verbosity=%d 2> geth.error > geth.log" boot_enode verbosity
 *   in
 *   Shell.of_string command
 * 
 * let geth_start ?(verbosity=6) boot_enode =
 *   let command =
 *     Printf.sprintf "geth --bootnodes=\"%s\" --verbosity=%d 2> geth.error > geth.log" boot_enode verbosity
 *   in
 *   Shell.of_string command *)


(* -------------------------------------------------------------------------- *)
(* Wrap Shell.execute so that we have a feedback on what has been executed and
   what was produced on the remote stdout. *)


(* -------------------------------------------------------------------------- *)
(* Populating remote hosts with config files & starting geth nodes are taken
   care of by the following functions. *)

(* Returns the process that runs on [port] on the remote host, if any.
   This should be MacOS-ok. *)
(* let port_is_free channel port =
 *   let command =
 *     Printf.sprintf "lsof -n -i:%d | cut -d \" \" -f1 | tail -n 1" port
 *   in
 *   let command = Shell.of_string command in
 *   let result  = Shell.execute ~timeout:300 channel command in
 *   if String.is_empty result then
 *     None
 *   else
 *     Some result *)

(* let port_is_free_on_host ~ssh_session ~port =
 *   Ssh.Client.with_shell (fun channel ->
 *       Shell.flush_stdout channel;
 *       port_is_free channel port
 *     ) ssh_session *)


(* let extract_enode string =
 *   let enode_start =
 *     try Str.search_forward (Str.regexp_string "enode") string 0
 *     with Not_found ->
 *       failwith ("Enode not found in " ^ string)
 *   in
 *   let enode_addr = Str.string_after string enode_start in
 *   (\* Remove trailing "\n" *\)
 *   String.strip enode_addr *)

(* Starts a bootnode and returns the bootnode address.
   Assume [bootnode_port] is free. *)
(* let start_bootnode ~ssh_session ~root_directory ~bootnode_port =
 *   let bootnode_key = "bootnode.key" in (\* file containing private key *\)
 *   Ssh.Client.with_shell (fun channel ->
 *       Shell.flush_stdout channel;
 *       let exec = log_exec channel in
 *       List.iter exec [
 *         Shell.mkdir root_directory;
 *         Shell.of_string "pwd";
 *         Shell.cd root_directory;
 *         Shell.of_string "pwd";
 *         bootnode_genkey bootnode_key
 *       ];
 *       let bootnode_info =
 *         Shell.execute
 *           ~read_stderr:true
 *           ~timeout:300
 *           channel
 *           (bootnode_nodekey bootnode_port bootnode_key)
 *       in
 *       extract_enode bootnode_info
 *     ) ssh_session
 * 
 * let killall ~ssh_session ~process =
 *   Ssh.Client.with_shell (fun channel ->
 *       Shell.flush_stdout channel;
 *       log_exec channel (Shell.of_string ("killall "^process))
 *     ) ssh_session *)
