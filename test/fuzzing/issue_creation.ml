
module C = Cohttp
module L = Lwt
module Lm = Lwt_main
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix

module CC = C.Code
module CH = C.Header 
module CluC = Clu.Client
module CluR = Clu.Response
module ClB = Cl.Body

type issuedata = 
  { title : string;
    body : string;
    authtoken : string; 
    destrepo : string}

let http_post_req 
    ?(headers = CH.init ()) 
    ?(body = ClB.empty) url =
  Lm.run (
    L.bind 
      ( CluC.post ~body ~headers 
          (Uri.of_string url))
      ( 
        fun (resp, body) ->
          let s = 
            CC.code_of_status 
              (CluR.status resp) 
          in
          L.map 
            (fun body -> s, body) 
            (ClB.to_string body)
      )
  )

let mk_auth_header token = 
  let h = CH.init () in 
  CH.add h "authorization" ("token "^token)

let post_issue {title; body; authtoken; destrepo} =
  let headers =
    mk_auth_header authtoken 
  in 
  let bstr = 
    Format.sprintf 
      "{\"title\":\"%s\", \"body\":\"%s\"}"
      title body
  in
  let body = ClB.of_string bstr in 
  http_post_req ~headers ~body destrepo

let () =
  if not (Array.length Sys.argv = 5)
  then 
    failwith
      "Expected 4 arguments: ./issuecreation.exe authtoken-filepath ae-filepath smt2-filepath link_to_destrepo@.";

  let authtoken_fp = Sys.argv.(1) in 
  let ic = open_in authtoken_fp in 
  let authtoken = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let ae_fp = Sys.argv.(2) in 
  let ic = open_in ae_fp in 
  let aefd = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let smt2_fp = Sys.argv.(3) in 
  let ic = open_in smt2_fp in 
  let smt2fp = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let destrepo = Sys.argv.(4) in 

  let title = 
    Format.sprintf 
      "Automatically generated issue issue %f "
      (Unix.gettimeofday ())
  in 

  let body = 
    Format.sprintf 
      "This is an issue!\nAlt-Ergo code:\n```\n%s\n```\nsmt-lib 2 (Version 2.6) code:\n```\n%s\n```\nThe two codes are supposed to be equivalent but when I run Alt-Ergo and CVC5 respectevly on the two codes, I get conflicting answers, it might be caused by and unsoundness bug."
      aefd smt2fp
  in
  let s, body = 
    post_issue {title; body; authtoken; destrepo}
  in 
  Format.printf "%d\n%s\n" s body

