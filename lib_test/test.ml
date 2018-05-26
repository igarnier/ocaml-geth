open Mlparity

let genesis =
  GethInit.Genesis.({
      config = {
        chain_id        = 0;
        homestead_block = 0;
        eip155_block    = 0;
        eip158_block    = 0
      };

      alloc = [];

      coinbase    = "0x0000000000000000000000000000000000000000";
      difficulty  = 1;
      extra_data  = "";
      gas_limit   = 0x2fefd8;
      nonce       = 0x107;
      mix_hash    = "0x0000000000000000000000000000000000000000000000000000000000000000";
      parent_hash = "0x0000000000000000000000000000000000000000000000000000000000000000";
      timestamp   = 0;
  })

let ssh_host =
  { Host.hostname = "localhost";
    Host.port     = 22 }

let conf =
  let open GethInit in
  {
    genesis_block  = genesis;
    root_directory = "priveth";
    data_subdir    = "data";
    source_subdir  = "source"
  }


let bootnode_conf =
  let open GethInit in
  {
    genesis_block  = genesis;
    root_directory = "priveth_boot";
    data_subdir    = "data";
    source_subdir  = "source"
  }

let ssh_opts =
  { Ssh.Client.host = ssh_host.hostname;
    username = "ilias";
    port = 22;
    log_level = SSH_LOG_NOLOG; (* SSH_LOG_FUNCTIONS; *)
    auth = Ssh.Client.Interactive
  }

(* let _ =
 *   let open Ssh.Client in
 *   with_session (fun session ->
 *       with_shell (fun chan ->
 *           Channel.read_timeout chan 100 |> (function s -> Printf.printf "result: %s\n%!" s);
 *           ignore (Channel.write chan "mkdir B\n");
 *           ignore (Channel.write chan "mkdir A\n");
 *           ignore (Channel.write chan "echo -n ABC\n")
 *         ) session
 *     ) ssh_opts *)
  

let _ =
  let boot_enode =
    GethInit.start_bootnode ~ssh_host ~username:"ilias" ~root_directory:"priveth_boot" ~bootnode_port:30301
  in
  Printf.printf "bootnode enode: %s\n" boot_enode
  (* GethInit.initialize_node ~ssh_host ~username:"ilias" ~conf; *)
  
