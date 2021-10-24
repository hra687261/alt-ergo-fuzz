
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
  if not (Array.length Sys.argv = 6)
  then
    failwith
      "Expected 4 arguments: ./issuecreation.exe authtoken-filepath ae-filepath smt2-filepath username reponame@.";

  let authtoken_fp = Sys.argv.(1) in
  let ic = open_in authtoken_fp in
  let authtoken = really_input_string ic ((in_channel_length ic) - 1) in
  close_in ic;

  let ae_fp = Sys.argv.(2) in
  let ic = open_in ae_fp in
  let aefd = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let smt2_fp = Sys.argv.(3) in
  let ic = open_in smt2_fp in
  let smt2fd = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let username = Sys.argv.(4) in
  let reponame = Sys.argv.(5) in

  let destrepo =
    Format.sprintf
      "https://api.github.com/repos/%s/%s/issues"
      username reponame
  in

  let title =
    Format.sprintf
      "Automatically generated issue %f "
      (Unix.gettimeofday ())
  in
  ignore (aefd, smt2fd);

  let rep = fun _ -> "\\n" in
  let regexp = Str.regexp "\n" in
  let aefd, smt2fd =
    Str.global_substitute regexp rep aefd,
    Str.global_substitute regexp rep smt2fd
  in
  let body =
    Format.sprintf
      "This is an issue!\\nAlt-Ergo code:\\n```%s```\\nsmt-lib 2 (Version 2.6) code:\\n```%s```\\nThe two codes are supposed to be equivalent but when I run Alt-Ergo and CVC5 on the two codes (respectevly), I get conflicting answers, it might be caused by an unsoundness bug."
      aefd smt2fd
  in
  Format.printf
    "\n%s\n%s\n%s\n" title authtoken destrepo;

  ignore body;
  let s, body =
    post_issue {title; body; authtoken; destrepo}
  in
  Format.printf "\n%d\n%s\n" s body
