(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Str

let full_compiled_url_regex = Str.regexp "\\(http|ftp|https\\)://\\([\\w_-]+\\(?:\\(?:\\.[\\w_-]+\\)+\\)\\)\\([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-]\\)?";;
let old_compiled_url_regex = Str.regexp "\"\\(http\\|ftp\\|https\\)://.*\"";;
let compiled_url_regex = Str.regexp "href=\\(\".*?\"\|\'.*?\'\\)";;

(* Regex search for urls in an html page *)
let rec scan_for_urls (rawfile: string) (pos: int) (found: string list): string list =
    try
        (* Find next match *)
        let next_position = Str.search_forward compiled_url_regex rawfile pos in

        (* Get the string of the match *)
        let string_found = Str.matched_string rawfile in

        (* Print what we found) *)
        Printf.printf "Found %s\n" string_found;

        (* Prepend match to found list *)
        let new_found = string_found :: found in

        (* Recurse *)
        Printf.printf "Searching for more at position %d\n" next_position;
        scan_for_urls rawfile (next_position + 1) new_found

    (* If nothing else was found, this is our list *)
    with Not_found -> found;;

(* Download one page using LWT async library *)
let download_one_page (url: string): string Lwt.t =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun bodystring ->
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    Printf.printf "Body of length: %d\n" (String.length bodystring);
    bodystring

let () =
    let page_contents = Lwt_main.run (download_one_page "http://www.spence.net") in
    let url_list = scan_for_urls page_contents 0 [] in
    Printf.printf "Done"
