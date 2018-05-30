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

let conf =
  let open GethInit in
  {
    genesis_block  = genesis;
    network_id     = 8798798;
    root_directory = "priveth";
    data_subdir    = "data";
    source_subdir  = "source"
  }

let password =
  Printf.printf "password: %!";
  Ssh_client.Easy.read_secret ()

let ilias_on_xps13 =
  { GethInit.ip_address = "127.0.0.1";
    ssh_port = 22;
    login = "ilias";
    password
  }


let _ =
  let enode = GethInit.start_no_bootnode conf ilias_on_xps13 in
  Printf.printf "enode: %s\n" enode
  
  
