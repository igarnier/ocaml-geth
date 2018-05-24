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
    root_directory = "preth";
    data_subdir    = "data";
    source_subdir  = "source"
  }


let _ =
  GethInit.initialize_node ~ssh_host ~username:"ilias" ~conf(* ;
   * let enode = GethInit.start_bootnode ~ssh_host ~username:"ilias" ~bootnode_port:30301 in
   * () *)
