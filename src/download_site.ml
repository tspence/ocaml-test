(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Str
open Core_kernel

let full_compiled_url_regex = Str.regexp "\\(http|ftp|https\\)://\\([\\w_-]+\\(?:\\(?:\\.[\\w_-]+\\)+\\)\\)\\([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-]\\)?";;
let old_compiled_url_regex = Str.regexp "\"\\(http\\|ftp\\|https\\)://.*\"";;
let compiled_url_regex = Str.regexp "href=\\(\"[^\"]+?\"\\|\'[^\']+?\'\\)";;

(* Get "len" number of characters at pos "pos" *)
let substring (str: string) (pos: int) (len: int): string =
    first_chars (string_after str pos) len;;

(* Regex search for urls in an html page, using the str.regexp library *)
let rec scan_for_urls ?(pos: int=0) ?(found: string list=[]) (rawfile: string) : string list =
    try
        (* Find next match *)
        let next_position = Str.search_forward compiled_url_regex rawfile pos in

        (* Get the string of the match *)
        let string_found = Str.matched_string rawfile in

        (* Trim the href=" off the left and the " off the right *)
        let string_trimmed = substring string_found 6 ((String.length string_found) - 7) in

        (* Prepend match to found list *)
        let new_found = string_trimmed :: found in

        (* Recurse *)
        scan_for_urls ~pos:(next_position + 1) ~found:new_found rawfile

    (* If nothing else was found, this is our list *)
    with Not_found -> found;;

(* Download one page using LWT async library *)
let download_one_page (url: string): string Lwt.t =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun bodystring ->
    (*
     * Not sure we need these - they were useful for debugging
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    Printf.printf "Body of length: %d\n" (String.length bodystring);
     *)
    bodystring;;

(* Merge a single URL *)
let convert_relative_url (base_url: string) (ref_url: string): string =
    if ((first_chars ref_url 1) = "/") then base_url ^ ref_url else ref_url;;

(* Convert relative urls to full URLs *)
let merge_url_with_refs (url: string) (reference_list: string list): string list =
    List.map ~f:(fun str: string -> convert_relative_url url str) reference_list;;

(* All the information we collect about a page *)
type page_record = {
    url: string;
    contents: string;
    references: string list;
};;

(* Function to download an html page, then scan it for further url references *)
let scan_page_for_references (url: string): page_record =
    let page_contents = Lwt_main.run (download_one_page url) in
    let reference_list = scan_for_urls page_contents in
    let merged_refs = merge_url_with_refs url reference_list in
    {
      url = url;
      contents = page_contents;
      references = merged_refs;
    };;

(* Program *)
let () =
    let p = scan_page_for_references "http://www.spence.net" in
    Printf.printf "Page scanned: %s\n" p.url;
    Printf.printf "Size: %d\n" (String.length p.contents);
    Printf.printf "References: %d\n" (List.count ~f:(fun x -> true) p.references);
    Printf.printf "Done\n\n\n";;
