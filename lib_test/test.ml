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
  { Host.hostname = "127.0.0.1";
    Host.port     = 22 }

let conf =
  let open GethInit in
  {
    genesis_block  = genesis;
    root_directory = "priveth";
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

let _ =
  (* Init ssh session structure *)
  let is_port_free =
    Ssh.Common.with_session (fun ssh_session ->
        Ssh.Client.connect_and_auth ssh_opts ssh_session;
        GethInit.port_is_free_on_host ~ssh_session ~port:30301
      )
  in
  begin match is_port_free with
  | None -> ()
  | Some process ->
    (Printf.printf "port 30301 is already taken by proc. %s - retype password to engage killall\n%!" process;
     Ssh.Common.with_session (fun ssh_session ->
         Ssh.Client.connect_and_auth ssh_opts ssh_session;
         GethInit.killall ~ssh_session ~process
       )
    )
  end;
  Printf.eprintf "port 30301 is free on host - starting bootnode\n%!";
  let boot_enode =
    Ssh.Common.with_session (fun ssh_session ->
        Ssh.Client.connect_and_auth ssh_opts ssh_session;
        GethInit.start_bootnode ~ssh_session ~root_directory:"priveth_boot" ~bootnode_port:30301
      )
  in
  Printf.eprintf "bootnode enode: %s\n%!" boot_enode;
  (* connect to regular node (here it's still localhost ...) *)
  Ssh.Common.with_session (fun ssh_session ->
      Ssh.Client.connect_and_auth ssh_opts ssh_session;
      GethInit.start_node ~ssh_session ~conf ~boot_enode
    )
  
  
