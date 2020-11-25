open Ocaml_geth

(* TODO obsolete!!! *)

let genesis =
  GethInit.Genesis.
    { config=
        { chain_id= 8888;
          (* never put 0!!! *)
          homestead_block= 0;
          eip155_block= 0;
          eip158_block= 0 };
      alloc= [];
      coinbase=
        Types.address_from_string "0x0000000000000000000000000000000000000000";
      difficulty= 1;
      extra_data= "";
      gas_limit= 0x80000000;
      nonce= 0x107;
      mix_hash=
        Types.hash256_from_string
          "0x0000000000000000000000000000000000000000000000000000000000000000";
      parent_hash=
        Types.hash256_from_string
          "0x0000000000000000000000000000000000000000000000000000000000000000";
      timestamp= 0 }

let conf =
  let open GethInit in
  { genesis_block= genesis;
    network_id= 8798798;
    root_directory= "priveth";
    data_subdir= "data";
    source_subdir= "source" }

let pepito_on_laptop () =
  { GethInit.ip_address= "127.0.0.1";
    ssh_port= 22;
    eth_port= 30303;
    login= "pepito";
    password=
      Ssh_client.Easy.input_password ~host:"127.0.0.1" ~username:"pepito" }

let pepito_on_server () =
  { GethInit.ip_address= "129.x.y.z";
    ssh_port= 22;
    eth_port= 30303;
    login= "pepito";
    password=
      Ssh_client.Easy.input_password ~host:"just for display purposes"
        ~username:"pepito" }

let conchita_on_osx () =
  { GethInit.ip_address= "82.x.y.z";
    ssh_port= 22;
    eth_port= 30303;
    login= "conchita";
    password=
      Ssh_client.Easy.input_password ~host:"conchitas_apple_II"
        ~username:"conchita" }

let network = [pepito_on_laptop (); pepito_on_server (); conchita_on_osx ()]
let _ = GethInit.deploy conf network
