open Astring
open Examples
open Lwt.Infix
open Capnp_rpc_lwt

module Test_utils = Testbed.Test_utils

module Vat = Capnp_rpc_unix.Vat
module CapTP = Capnp_rpc_unix.CapTP
module Unix_flow = Capnp_rpc_unix.Unix_flow
module Tls_wrapper = Capnp_rpc_lwt.Auth.Tls_wrapper(Unix_flow)

type cs = {
  client : Vat.t;
  server : Vat.t;
  server_key : Auth.Secret_key.t option;
}

let ( >|*= ) x f =
  x >|= function
  | Error (`Msg e) -> failwith e
  | Ok y -> f y

let expect_some msg = function
  | None -> Alcotest.fail msg
  | Some x -> x

(* Have the client ask the server for its bootstrap object, and return the
   resulting client-side proxy to it. *)
let get_bootstrap ~switch cs =
  let _address, auth = Vat.public_address cs.server |> expect_some "No public address!" in
  let server_socket, client_socket = Unix_flow.socketpair ~switch () in
  let _server =
    Tls_wrapper.connect_as_server ~switch server_socket cs.server_key >|*=
    Vat.connect cs.server
  in
  Tls_wrapper.connect_as_client ~switch client_socket auth >|*= fun ep ->
  let conn = Vat.connect cs.client ep in
  CapTP.bootstrap conn

module Utils = struct
  [@@@ocaml.warning "-32"]

  let dump cs =
    Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Vat.dump cs.client);
    Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Vat.dump cs.server)
end

let server_key = Auth.Secret_key.generate ()
let client_key = Auth.Secret_key.generate ()

let make_vats ?server_key ~switch ~service () =
  let auth =
    match server_key with
    | Some key -> Capnp_rpc_lwt.Auth.Secret_key.digest key
    | None -> Capnp_rpc_lwt.Auth.Digest.insecure
  in
  let address = (`Unix "/tmp/socket", auth) in
  {
    client = Vat.create ~switch ();
    server = Vat.create ~switch ~bootstrap:service ~address ();
    server_key;
  }

(* Generic Lwt running for Alcotest. *)
let run_lwt fn () =
  let warnings_at_start = Logs.(err_count () + warn_count ()) in
  Logs.info (fun f -> f "Start test-case");
  let async_ex, async_waker = Lwt.wait () in
  let handle_exn ex =
    Logs.info (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
    if Lwt.state async_ex = Lwt.Sleep then
      Lwt.wakeup_exn async_waker ex
  in
  Lwt.async_exception_hook := handle_exn;
  Lwt_main.run begin
    Lwt_switch.with_switch (fun sw ->
        let finished = ref false in
        Lwt_switch.add_hook (Some sw) (fun () ->
            if not !finished then handle_exn (Failure "Switch turned off early");
            Lwt.return_unit
          );
        Lwt.pick [
          async_ex;
          fn sw >|= fun () -> finished := true;
        ] >|= fun () ->
        Gc.full_major ()
      )
  end;
  Lwt.wakeup_paused ();
  Gc.full_major ();
  Lwt.wakeup_paused ();
  Gc.full_major ();
  let warnings_at_end = Logs.(err_count () + warn_count ()) in
  Alcotest.(check int) "Check log for warnings" 0 (warnings_at_end - warnings_at_start)

let test_simple switch ~server_key =
  let cs = make_vats ~switch ?server_key ~service:(Echo.local ()) () in
  get_bootstrap ~switch cs >>= fun service ->
  Echo.ping service "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service;
  Lwt.return ()

let test_bad_crypto switch =
  let cs = make_vats ~switch ~server_key:server_key ~service:(Echo.local ()) () in
  let cs = {cs with server_key = Some client_key} in (* (bad config) *)
  Lwt.try_bind
    (fun () -> get_bootstrap ~switch cs)
    (fun _ -> Alcotest.fail "Wrong TLS key should have been rejected")
    (function
      | Failure msg ->
        assert (String.is_prefix ~affix:"TLS connection failed: authentication failure" msg);
        Lwt.return ()
      | ex ->
        Lwt.fail ex
    )

let test_parallel switch =
  let cs = make_vats ~switch ~service:(Echo.local ()) () in
  get_bootstrap ~switch cs >>= fun service ->
  let reply1 = Echo.ping service ~slow:true "ping1" in
  Echo.ping service "ping2" >|= Alcotest.(check string) "Ping2 response" "got:1:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  Echo.unblock service >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:0:ping1" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return ()

let test_registry switch =
  let registry_impl = Registry.local () in
  let cs = make_vats ~switch ~service:registry_impl () in
  get_bootstrap ~switch cs >>= fun registry ->
  let echo_service = Registry.echo_service registry in
  Registry.unblock registry >>= fun () ->
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Lwt.return ()

let test_embargo switch =
  let registry_impl = Registry.local () in
  let local_echo = Echo.local () in
  let cs = make_vats ~switch ~service:registry_impl () in
  get_bootstrap ~switch cs >>= fun registry ->
  Registry.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service registry in
  let reply1 = Echo.ping echo_service "ping" in
  Registry.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_resolve switch =
  let registry_impl = Registry.local () in
  let local_echo = Echo.local () in
  let cs = make_vats ~switch ~service:registry_impl () in
  get_bootstrap ~switch cs >>= fun registry ->
  Registry.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service_promise registry in
  let reply1 = Echo.ping echo_service "ping" in
  Registry.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_cancel switch =
  let cs = make_vats ~switch ~service:(Echo.local ()) () in
  get_bootstrap ~switch cs >>= fun service ->
  let reply1 = Echo.ping service ~slow:true "ping1" in
  assert (Lwt.state reply1 = Lwt.Sleep);
  Lwt.cancel reply1;
  Lwt.try_bind
    (fun () -> reply1)
    (fun _ -> Alcotest.fail "Should have been cancelled!")
    (function
      | Lwt.Canceled -> Lwt.return ()
      | ex -> Lwt.fail ex
    )
  >>= fun () ->
  Echo.unblock service >|= fun () ->
  Capability.dec_ref service

let float = Alcotest.testable Fmt.float (=)

let test_calculator switch =
  let open Calc in
  let cs = make_vats ~switch ~service:Calc.local () in
  get_bootstrap ~switch cs >>= fun c ->
  Calc.evaluate c (Float 1.) |> Value.final_read >|= Alcotest.check float "Simple calc" 1. >>= fun () ->
  let local_add = Calc.Fn.add in
  let expr = Expr.(Call (local_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read >|= Alcotest.check float "Complex with local fn" 3. >>= fun () ->
  let remote_add = Calc.getOperator c `Add in
  Calc.Fn.call remote_add [5.; 3.] >|= Alcotest.check float "Check fn" 8. >>= fun () ->
  let expr = Expr.(Call (remote_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read >|= Alcotest.check float "Complex with remote fn" 3. >>= fun () ->
  Capability.dec_ref remote_add;
  Capability.dec_ref c;
  Lwt.return ()

let test_indexing switch =
  let registry_impl = Registry.local () in
  let cs = make_vats ~switch ~service:registry_impl () in
  get_bootstrap ~switch cs >>= fun registry ->
  let echo_service, version = Registry.complex registry in
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Registry.Version.read version >|= Alcotest.(check string) "Version response" "0.1" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Capability.dec_ref version;
  Lwt.return ()

let cmd_result t =
  let pp f = function
    | `Error _ -> Fmt.string f "error"
    | `Help -> Fmt.string f "help"
    | `Version -> Fmt.string f "version"
    | `Ok x -> Alcotest.pp t f x
  in
  let equal a b =
    match a, b with
    | `Ok a, `Ok b -> Alcotest.equal t a b
    | _ -> a = b
  in
  Alcotest.testable pp equal

let vat_config = Alcotest.testable Capnp_rpc_unix.Vat_config.pp Capnp_rpc_unix.Vat_config.equal

let config_result = cmd_result vat_config

let test_options () =
  let term = (Capnp_rpc_unix.Vat_config.cmd, Cmdliner.Term.info "main") in
  let expected = `Ok { Capnp_rpc_unix.Vat_config.
                       backlog = 5;
                       secret_key = None;
                       listen_address = `Unix "/run/socket";
                       public_address = `Unix "/run/socket";
                     } in
  Cmdliner.Term.eval ~argv:[| "main"; "--secret-key-type=none"; "--listen-address"; "unix:/run/socket" |] term
  |> Alcotest.check config_result "No key" expected;
  let expected = `Ok { Capnp_rpc_unix.Vat_config.
                       backlog = 5;
                       secret_key = None;
                       listen_address = `TCP ("0.0.0.0", 7000);
                       public_address = `TCP ("1.2.3.4", 7001);
                     } in
  Cmdliner.Term.eval ~argv:[| "main"; "--secret-key-type=none";
                              "--public-address"; "tcp:1.2.3.4:7001";
                              "--listen-address"; "tcp:0.0.0.0:7000" |] term
  |> Alcotest.check config_result "Using TCP" expected

let expect_ok = function
  | Error (`Msg m) -> Alcotest.fail m
  | Ok x -> x

let test_sturdy_ref () =
  let module Sturdy_ref = Capnp_rpc_unix.Sturdy_ref in
  let sturdy_ref = Alcotest.testable Sturdy_ref.pp_with_secrets Sturdy_ref.equal in
  let check msg expected_uri sr =
    let uri = Sturdy_ref.to_uri_with_secrets sr in
    Alcotest.(check string) msg expected_uri (Uri.to_string uri);
    let sr2 = Sturdy_ref.of_uri uri |> expect_ok in
    Alcotest.(check sturdy_ref) msg sr sr2
  in
  let sr = Sturdy_ref.v ~address:(`Unix "/sock", Auth.Digest.insecure) ~service:`Bootstrap in
  check "Insecure Unix" "capnp://insecure@/sock" sr;
  let sr = Sturdy_ref.v ~address:(`TCP ("localhost", 7000), Auth.Digest.insecure) ~service:`Bootstrap in
  check "Insecure TCP" "capnp://insecure@localhost:7000" sr;
  let test_uri = Uri.of_string "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/" in
  let auth = Auth.Digest.from_uri test_uri |> expect_ok in
  let sr = Sturdy_ref.v ~address:(`TCP ("localhost", 7000), auth) ~service:`Bootstrap in
  check "Secure TCP" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@localhost:7000" sr;
  let sr = Sturdy_ref.v ~address:(`Unix "/sock", auth) ~service:`Bootstrap in
  check "Secure Unix" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/sock" sr

let rpc_tests = [
  "Simple",     `Quick, run_lwt (test_simple ~server_key:None);
  "Crypto",     `Quick, run_lwt (test_simple ~server_key:(Some server_key));
  "Bad crypto", `Quick, run_lwt test_bad_crypto;
  "Parallel",   `Quick, run_lwt test_parallel;
  "Embargo",    `Quick, run_lwt test_embargo;
  "Resolve",    `Quick, run_lwt test_resolve;
  "Registry",   `Quick, run_lwt test_registry;
  "Calculator", `Quick, run_lwt test_calculator;
  "Cancel",     `Quick, run_lwt test_cancel;
  "Indexing",   `Quick, run_lwt test_indexing;
  "Options",    `Quick, test_options;
  "Sturdy ref", `Quick, test_sturdy_ref;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "lwt", rpc_tests;
  ]
