open Batteries


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
  root_directory : string;
  data_subdir    : string;
  source_subdir  : string
}

(* Initializing an Ethereum network: *)

let mkdir dir =
  Printf.sprintf "mkdir -p %s" dir

let cd dir =
  Printf.sprintf "cd %s" dir

let geth_init datadir genesis_json =
  Printf.sprintf "get --datadir=\"%s\" init \"%s\"" datadir genesis_json

let bootnode_init addr =
  Printf.sprintf "bootnode -genkey bootnode.key -addr :%d" addr

let (//) dir1 dir2 = dir1^"/"^dir2

let write_genesis genesis_block target_file session =
  let local_genesis = Filename.temp_file "genesis" ".json" in
  File.with_file_out ~mode:[`create;`text] ~perm:(File.unix_perm 0o660) local_genesis (fun fd ->
      let json_str = genesis_block |> Genesis.to_json |> Yojson.to_string in
      output_string fd json_str;
      Ssh.Client.scp local_genesis target_file session
    )

let log_exec ~command session =
  let res = Ssh.Client.exec ~command session in
  Printf.eprintf "# %s > %s\n" command res

let initialize_node ~(ssh_host : Host.t) ~username ~(conf : geth_config) =
  let opts =
    let open Ssh.Client in
    { host  = ssh_host.Host.hostname;
      username;
      port      = ssh_host.Host.port;
      log_level = SSH_LOG_WARNING;
      auth      = Interactive
    } in
  Ssh.Client.with_session (fun session ->
      let open Ssh.Client in
      log_exec ~command:(mkdir conf.root_directory) session;
      log_exec ~command:("pwd") session;
      log_exec ~command:("ls") session;
      log_exec ~command:(cd conf.root_directory) session;
      log_exec ~command:("pwd") session;
      log_exec ~command:(mkdir conf.data_subdir) session;
      log_exec ~command:(mkdir conf.source_subdir) session;
      write_genesis conf.genesis_block "genesis.json" session;
      exec
        ~command:(geth_init
                    ("~"//conf.root_directory//conf.data_subdir)
                    ("~"//conf.root_directory//"genesis.json")) session |> ignore
    ) opts

(* Returns the bootnode address *)
let start_bootnode ~(ssh_host : Host.t) ~username ~bootnode_port =
  let opts =
    let open Ssh.Client in
    { host  = ssh_host.Host.hostname;
      username;
      port      = ssh_host.Host.port;
      log_level = SSH_LOG_WARNING;
      auth      = Interactive
    } in
  Ssh.Client.with_session (fun session ->
      let open Ssh.Client in
      let result = exec ~command:(bootnode_init bootnode_port) session in
      Printf.printf "RESULT: %s\n" result;
      let start = Str.search_forward (Str.regexp_string "enode") result 0 in
      Str.string_after result start
    ) opts


(* ssh root@101.102.103.104
 * mkdir ucsfnet
 * cd ucsfnet
 * mkdir data
 * mkdir source *)

  
(* {
 * "config": {
 *         "chainId": 15,
 *         "homesteadBlock": 0,
 *         "eip155Block": 0,
 *         "eip158Block": 0
 *     },
 * 
 *   "alloc"      : {
 *   "0x0000000000000000000000000000000000000001": {"balance": "111111111"},
 *   "0x0000000000000000000000000000000000000002": {"balance": "222222222"}
 *     },
 * 
 *   "coinbase"   : "0x0000000000000000000000000000000000000000",
 *   "difficulty" : "0x00001",
 *   "extraData"  : "",
 *   "gasLimit"   : "0x2fefd8",
 *   "nonce"      : "0x0000000000000107",
 *   "mixhash"    : "0x0000000000000000000000000000000000000000000000000000000000000000",
 *   "parentHash" : "0x0000000000000000000000000000000000000000000000000000000000000000",
 *   "timestamp"  : "0x00"
 * } *)

