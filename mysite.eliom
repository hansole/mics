[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

let msg_pr_sec : int = 100
let sleep_time = 1.0 /. (float_of_int msg_pr_sec)
let%client print_every_n_message = 10 * ~%msg_pr_sec

let%server send_ev_total = ref 0

let%server timer_th send_e () =
  let rec loop () =
    Lwt_unix.sleep sleep_time >>= fun () ->
    let () = incr send_ev_total in
    let s = Printf.sprintf "msg number %d " !send_ev_total in
    let () = send_e (s) in
    loop ()
  in
  loop ()

let%client got_ev_count = ref 0
let%client got_ev_total = ref 0

let%server setup_react () =
  let e, send_e = React.E.create () in
  Lwt.async(timer_th send_e);
  let client_ev = Eliom_react.Down.of_react
                    ~size: 100
                    ~scope:Eliom_common.default_process_scope
                    e
  in
  ignore [%client
    ((React.E.map (fun (msg) ->
      let () = incr got_ev_count in
      if !got_ev_count >= print_every_n_message then
        let () = got_ev_total := !got_ev_total + !got_ev_count in
        let () = got_ev_count := 0 in
        let () = Printf.printf "got this %s total %d" msg !got_ev_total in
        let () = flush stdout in
        ()
     ) ~%client_ev)
     : unit React.E.t)
    ];
  Lwt.return send_e

module Mysite_app =
  Eliom_registration.App (
    struct
      let application_name = "mysite"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server page () =
  setup_react () >>= fun e ->
  Lwt.return Eliom_content.Html.F.[div  [pcdata "See console log"]]

let () =
  Mysite_app.register
    ~service:main_service
    (fun () () ->
      let%lwt p = page () in
      Lwt.return
        (Eliom_tools.D.html
           ~title:"mysite"
           ~css:[["css";"mysite.css"]]
           (body p)
        )
    )
