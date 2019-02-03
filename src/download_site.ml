(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Str

let compiled_url_regex = Str.regex "http";;

(* Regex search for urls in an html page *)
let scan_for_urls (rawfile: string) (pos: int) (found: string list): string list =
    try
        (* Find next match *)
        let next_position = Str.search_forward compiled_url_regex rawfile pos in
        
        (* Get the string of the match *)
        let string_found = Str.matched_string rawfile in
        
        (* Print what we found) *)
        Printf.printf string_found;
    
        (* Prepend match to found list *)
        let new_found = string_found :: found in
        
        (* Recurse *)
        scan_for_urls rawfile (next_position + 1) new_found;;
        
    (* If nothing else was found, this is our list *)
    with Not_found -> found;;
   
(* Download one page *)
let download_one_page (url: string): string =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "Body of length: %d\n" (String.length body);
    body
  
let () =
    let page = download_one_page "http://www.spence.net" in
    print_endline ("Received body\n" ^ page)
  
  
