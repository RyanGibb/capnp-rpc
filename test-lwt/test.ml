open Astring
open Examples
open Lwt.Infix
open Capnp_rpc_lwt

module Test_utils = Testbed.Test_utils

module Vat = Capnp_rpc_unix.Vat
module CapTP = Capnp_rpc_unix.CapTP
module Unix_flow = Capnp_rpc_unix.Unix_flow
module Tls_wrapper = Capnp_rpc_lwt.Tls_wrapper.Make(Unix_flow)
module Exception = Capnp_rpc.Exception

type cs = {
  client : Vat.t;
  server : Vat.t;
  client_key : Auth.Secret_key.t;
  server_key : Auth.Secret_key.t;
  serve_tls : bool;
  server_switch : Lwt_switch.t;
}

let expect_some msg = function
  | None -> Alcotest.fail msg
  | Some x -> x

(* Have the client ask the server for its bootstrap object, and return the
   resulting client-side proxy to it. *)
let get_bootstrap cs =
  let id = Restorer.Id.public "" in
  let sr = Vat.sturdy_ref cs.server id in
  Vat.connect_exn cs.client sr

module Utils = struct
  [@@@ocaml.warning "-32"]

  let dump cs =
    Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Vat.dump cs.client);
    Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Vat.dump cs.server)
end

let server_key = Auth.Secret_key.generate ()
let client_key = Auth.Secret_key.generate ()

let server_pem = `PEM (Auth.Secret_key.to_pem_data server_key)

let make_vats ?(serve_tls=false) ~switch ~service () =
  let id = Restorer.Id.public "" in
  let restore = Restorer.single id service in
  let server_config =
    let socket_path = Filename.concat (Sys.getcwd ()) "server" in
    Capnp_rpc_unix.Vat_config.create ~secret_key:server_pem ~serve_tls (`Unix socket_path)
  in
  let server_switch = Lwt_switch.create () in
  Capnp_rpc_unix.serve ~switch:server_switch ~tags:Test_utils.server_tags ~restore server_config >>= fun server ->
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt_switch.turn_off server_switch);
  Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref service; Lwt.return_unit);
  Lwt.return {
    client = Vat.create ~switch ~tags:Test_utils.client_tags ~secret_key:(lazy client_key) ();
    server;
    client_key;
    server_key;
    serve_tls;
    server_switch;
  }

(* Generic Lwt running for Alcotest. *)
let run_lwt ?(expected_warnings=0) fn () =
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
  Alcotest.(check int) "Check log for warnings" expected_warnings (warnings_at_end - warnings_at_start)

let test_simple switch ~serve_tls =
  make_vats ~switch ~serve_tls ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service;
  Lwt.return ()

let test_bad_crypto switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let id = Restorer.Id.public "" in
  let (addr, _digest) = Vat.public_address cs.server |> expect_some "Has address" in
  let address = (addr, Auth.Secret_key.digest ~hash:`SHA256 client_key) in (* (should be server_key) *)
  let sr = Capnp_rpc_unix.Sturdy_ref.v ~address ~service:id in
  let old_warnings = Logs.warn_count () in
  Vat.connect cs.client sr >>= function
  | Ok _ -> Alcotest.fail "Wrong TLS key should have been rejected"
  | Error e ->
    let msg = Fmt.to_to_string Capnp_rpc.Exception.pp e in
    assert (String.is_prefix ~affix:"Failed: TLS connection failed: authentication failure" msg);
    (* Wait for server to log warning *)
    let rec wait () =
      if Logs.warn_count () = old_warnings then Lwt.pause () >>= wait
      else Lwt.return_unit
    in
    wait ()

let test_parallel switch =
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  let reply1 = Echo.ping service ~slow:true "ping1" in
  Echo.ping service "ping2" >|= Alcotest.(check string) "Ping2 response" "got:1:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  Echo.unblock service >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:0:ping1" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return ()

let test_registry switch =
  let registry_impl = Registry.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
  let echo_service = Registry.echo_service registry in
  Registry.unblock registry >>= fun () ->
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Lwt.return ()

let test_embargo switch =
  let registry_impl = Registry.local () in
  let local_echo = Echo.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
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
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
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
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
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
  make_vats ~switch ~service:Calc.local () >>= fun cs ->
  get_bootstrap cs >>= fun c ->
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
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
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
  let config = Cmdliner.Term.eval
      ~argv:[| "main"; "--secret-key-file=key.pem"; "--listen-address"; "unix:/run/socket" |] term in
  let expected = `Ok (Capnp_rpc_unix.Vat_config.create
                        ~secret_key:(`File "key.pem")
                        (`Unix "/run/socket")
                     ) in
  Alcotest.check config_result "Unix, same address" expected config;
  let expected = `Ok (Capnp_rpc_unix.Vat_config.create
                       ~secret_key:(`File "key.pem")
                       ~public_address:(`TCP ("1.2.3.4", 7001))
                       (`TCP ("0.0.0.0", 7000))
                     ) in
  Cmdliner.Term.eval ~argv:[| "main";
                              "--secret-key-file=key.pem";
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
  let public_bootstrap = Restorer.Id.public "" in
  let public_main = Restorer.Id.public "main" in
  let sr = Sturdy_ref.v ~address:(`Unix "/sock", Auth.Digest.insecure) ~service:public_bootstrap in
  check "Insecure Unix" "capnp://insecure@/sock/" sr;
  let sr = Sturdy_ref.v ~address:(`TCP ("localhost", 7000), Auth.Digest.insecure) ~service:public_bootstrap in
  check "Insecure TCP" "capnp://insecure@localhost:7000" sr;
  let test_uri = Uri.of_string "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/" in
  let auth = Auth.Digest.from_uri test_uri |> expect_ok in
  let sr = Sturdy_ref.v ~address:(`TCP ("localhost", 7000), auth) ~service:public_main in
  check "Secure TCP" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@localhost:7000/bWFpbg" sr;
  let sr = Sturdy_ref.v ~address:(`Unix "/sock", auth) ~service:public_main in
  check "Secure Unix" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/sock/bWFpbg" sr

let cap_equal_exn a b =
  match Capability.equal a b with
  | Ok x -> x
  | Error `Unsettled -> Alcotest.failf "Can't compare %a and %a: not settled!"
                          Capability.pp a
                          Capability.pp b

let cap = Alcotest.testable Capability.pp cap_equal_exn

let expect_non_exn = function
  | Ok x -> x
  | Error ex -> Alcotest.failf "expect_non_exn: %a" Capnp_rpc.Exception.pp ex

let except = Alcotest.testable Capnp_rpc.Exception.pp (=)

let test_table_restorer _switch =
  let table = Restorer.Table.create () in
  let echo_id = Restorer.Id.public "echo" in
  let registry_id = Restorer.Id.public "registry" in
  let broken_id = Restorer.Id.public "broken" in
  let unknown_id = Restorer.Id.public "unknown" in
  Restorer.Table.add table echo_id @@ Echo.local ();
  Restorer.Table.add table registry_id @@ Registry.local ();
  Restorer.Table.add table broken_id @@ Capability.broken (Capnp_rpc.Exception.v "broken");
  let r = Restorer.of_table table in
  Restorer.restore r echo_id >|= expect_non_exn >>= fun a1 ->
  Echo.ping a1 "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Restorer.restore r echo_id >|= expect_non_exn >>= fun a2 ->
  Alcotest.check cap "Same cap" a1 a2;
  Restorer.restore r registry_id >|= expect_non_exn >>= fun r1 ->
  assert (a1 <> r1);
  Restorer.restore r broken_id >|= expect_non_exn >>= fun x ->
  let expected = Some (Capnp_rpc.Exception.v "broken") in
  Alcotest.(check (option except)) "Broken response" expected (Capability.problem x);
  Restorer.restore r unknown_id >>= fun x ->
  let expected = Error (Capnp_rpc.Exception.v "Unknown persistent service ID") in
  Alcotest.(check (result reject except)) "Missing mapping" expected x;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  Capability.dec_ref r1;
  Restorer.Table.remove table echo_id;
  Restorer.Table.clear table;
  Lwt.return ()

let test_fn_restorer _switch =
  let cap = Alcotest.testable Capability.pp (=) in
  let a = Restorer.Id.public "a" in
  let b = Restorer.Id.public "b" in
  let c = Restorer.Id.public "c" in
  let current_c = ref (Restorer.reject (Exception.v "Broken C")) in
  let delay = Lwt_condition.create () in
  let hash = `SHA256 in
  let digest = Restorer.Id.digest hash in
  let load id =
    if id = digest a then Lwt.return @@ Restorer.grant @@ Echo.local ()
    else if id = digest b then Lwt_condition.wait delay >|= fun () -> Restorer.grant @@ Echo.local ()
    else if id = digest c then Lwt_condition.wait delay >|= fun () -> !current_c
    else Lwt.return @@ Restorer.unknown_service_id
  in
  let table = Restorer.Table.of_loader ~hash load in
  let restorer = Restorer.of_table table in
  let restore x = Restorer.restore restorer x in
  (* Check that restoring the same ID twice caches the capability. *)
  restore a >|= expect_non_exn >>= fun a1 ->
  restore a >|= expect_non_exn >>= fun a2 ->
  Alcotest.check cap "Restore cached" a1 a2;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  (* But if it's released, the next lookup loads a fresh one. *)
  restore a >|= expect_non_exn >>= fun a3 ->
  if a1 = a3 then Alcotest.fail "Returned released cap!";
  Capability.dec_ref a3;
  (* Doing two lookups in parallel only does one load. *)
  let b1 = restore b in
  let b2 = restore b in
  assert (Lwt.state b1 = Lwt.Sleep);
  Lwt_condition.broadcast delay ();
  b1 >|= expect_non_exn >>= fun b1 ->
  b2 >|= expect_non_exn >>= fun b2 ->
  Alcotest.check cap "Restore delayed cached" b1 b2;
  Restorer.Table.clear table;   (* (should have no effect) *)
  Capability.dec_ref b1;
  Capability.dec_ref b2;
  (* Failed lookups aren't cached. *)
  let c1 = restore c in
  Lwt_condition.broadcast delay ();
  c1 >>= fun c1 ->
  let reject = Alcotest.result cap except in
  Alcotest.check reject "C initially fails" (Error (Exception.v "Broken C")) c1;
  let c2 = restore c in
  let c_service = Echo.local () in
  current_c := Restorer.grant c_service;
  Lwt_condition.broadcast delay ();
  c2 >|= expect_non_exn >>= fun c2 ->
  Alcotest.check cap "C now works" c_service c2;
  Capability.dec_ref c2;
  (* Two users; one frees the cap immediately *)
  let b1 =
    restore b >|= expect_non_exn >|= fun b1 ->
    Capability.dec_ref b1;
    b1
  in
  let b2 = restore b in
  Lwt_condition.broadcast delay ();
  b1 >>= fun b1 ->
  b2 >|= expect_non_exn >>= fun b2 ->
  Alcotest.check cap "Cap not freed" b1 b2;
  Capability.dec_ref b2;
  Lwt.return_unit

let test_broken switch =
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  let problem, set_problem = Lwt.wait () in
  Capability.when_broken (fun x -> Lwt.wakeup set_problem x) service;
  Alcotest.check (Alcotest.option except) "Still OK" None @@ Capability.problem service;
  assert (Lwt.state problem = Lwt.Sleep);
  Logs.info (fun f -> f "Turning off server...");
  Lwt_switch.turn_off cs.server_switch >>= fun () ->
  problem >>= fun problem ->
  let expected = Exception.v ~ty:`Disconnected "Vat shut down" in
  Alcotest.check except "Broken callback ran" expected problem;
  assert (Capability.problem service <> None);
  Lwt.catch
    (fun () -> Echo.ping service "ping" >|= fun _ -> Alcotest.fail "Should have failed!")
    (fun _ -> Lwt.return ())
  >|= fun () ->
  Capability.dec_ref service

(* [when_broken] follows promises. *)
let test_broken2 () =
  let promise, resolver = Capability.promise () in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) promise;
  let p2, r2 = Capability.promise () in
  Capability.resolve_ok resolver p2;
  Alcotest.check (Alcotest.option except) "No problem yet" None !problem;
  let ex = Exception.v "Test" in
  Capability.resolve_ok r2 (Capability.broken ex);
  Alcotest.check (Alcotest.option except) "Now broken" (Some ex) !problem;
  ()

let test_broken3 () =
  let ex = Exception.v "Test" in
  let c = Capability.broken ex in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) c;
  Alcotest.check (Alcotest.option except) "Broken immediately" (Some ex) !problem

let test_broken4 () =
  let promise, _resolver = Capability.promise () in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) promise;
  Capability.dec_ref promise;
  Alcotest.check (Alcotest.option except) "Released, not called" None !problem

let test_parallel_connect switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let service = get_bootstrap cs in
  let service2 = get_bootstrap cs in
  service >>= fun service ->
  service2 >>= fun service2 ->
  Capability.wait_until_settled service >>= fun () ->
  Capability.wait_until_settled service2 >>= fun () ->
  Alcotest.check cap "Shared connection" service service2;
  Capability.dec_ref service;
  Capability.dec_ref service2;
  Lwt.return_unit

let test_parallel_fails switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let service = get_bootstrap cs in
  let service2 = get_bootstrap cs in
  service >>= fun service ->
  service2 >>= fun service2 ->
  Lwt_switch.turn_off cs.server_switch >>= fun () ->
  Capability.wait_until_settled service >>= fun () ->
  Capability.wait_until_settled service2 >>= fun () ->
  Alcotest.check cap "Shared failure" service service2;
  Capability.dec_ref service;
  Capability.dec_ref service2;
  (* Restart server (ignore new client) *)
  Lwt.pause () >>= fun () ->
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun _cs2 ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return_unit

let test_crossed_calls switch =
  (* Would be good to control the ordering here, to test the various cases.
     Currently, it's not certain which path is actually tested. *)
  let id = Restorer.Id.public "" in
  let make_vat ~secret_key ~tags addr =
    let service = Echo.local () in
    let restore = Restorer.(single id) service in
    let config =
      let secret_key = `PEM (Auth.Secret_key.to_pem_data secret_key) in
      let socket_path = Filename.concat (Sys.getcwd ()) addr in
      Capnp_rpc_unix.Vat_config.create ~secret_key (`Unix socket_path)
    in
    Capnp_rpc_unix.serve ~switch ~tags ~restore config >>= fun vat ->
    Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref service; Lwt.return_unit);
    Lwt.return vat
  in
  make_vat ~secret_key:client_key ~tags:Test_utils.client_tags "client" >>= fun client ->
  make_vat ~secret_key:server_key ~tags:Test_utils.server_tags "server" >>= fun server ->
  let sr_to_client = Capnp_rpc_unix.Vat.sturdy_ref client id in
  let sr_to_server = Capnp_rpc_unix.Vat.sturdy_ref server id in
  let to_client = Capnp_rpc_unix.Vat.connect_exn server sr_to_client in
  let to_server = Capnp_rpc_unix.Vat.connect_exn client sr_to_server in
  to_client >>= fun to_client ->
  to_server >>= fun to_server ->
  Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Capnp_rpc_unix.Vat.dump client);
  Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Capnp_rpc_unix.Vat.dump server);
  Echo.ping to_client "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Echo.ping to_server "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref to_client;
  Capability.dec_ref to_server;
  Lwt.return_unit

let rpc_tests = [
  "Simple",     `Quick, run_lwt (test_simple ~serve_tls:false);
  "Crypto",     `Quick, run_lwt (test_simple ~serve_tls:true);
  "Bad crypto", `Quick, run_lwt test_bad_crypto ~expected_warnings:1;
  "Parallel",   `Quick, run_lwt test_parallel;
  "Embargo",    `Quick, run_lwt test_embargo;
  "Resolve",    `Quick, run_lwt test_resolve;
  "Registry",   `Quick, run_lwt test_registry;
  "Calculator", `Quick, run_lwt test_calculator;
  "Cancel",     `Quick, run_lwt test_cancel;
  "Indexing",   `Quick, run_lwt test_indexing;
  "Options",    `Quick, test_options;
  "Sturdy ref", `Quick, test_sturdy_ref;
  "Table restorer", `Quick, run_lwt test_table_restorer;
  "Fn restorer", `Quick, run_lwt test_fn_restorer;
  "Broken ref", `Quick, run_lwt test_broken;
  "Broken ref 2", `Quick, test_broken2;
  "Broken ref 3", `Quick, test_broken3;
  "Broken ref 4", `Quick, test_broken4;
  "Parallel connect", `Quick, run_lwt test_parallel_connect;
  "Parallel fails", `Quick, run_lwt test_parallel_fails;
  "Crossed calls", `Quick, run_lwt test_crossed_calls;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "lwt", rpc_tests;
  ]
