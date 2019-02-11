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

    (* If nothing else was found, this is our list.  Note two different
       exceptions since one is deprecated *)
    with _ -> found;;

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
    let l = String.length ref_url in
    if ((l > 0) && ((first_chars ref_url 1) = "/")) then
        base_url ^ ref_url
    else
        ref_url;;

(* Convert relative urls to full URLs *)
let merge_url_with_refs (url: string) (reference_list: string list): string list =
    List.map ~f:(fun str: string -> convert_relative_url url str) reference_list;;

(* All the information we collect about a page *)
type page_record = {
    url: string;
    contents: string;
    references: string list;
};;

(* Print out information about a page *)
let print_page_record (page: page_record) =
    Printf.printf "Page scanned: %s\n" page.url;
    Printf.printf "Size: %d\n" (String.length page.contents);
    Printf.printf "References: %d\n" (List.count ~f:(fun x -> true) page.references);
    Printf.printf "Done\n\n\n";;

(* Keep track of all references yet to scan *)
let download_results = Hashtbl.create (module String);;

(* Keep track of all references yet to scan *)
let pending_urls = Queue.create ();;

let trim_trailing_slash (url: string): string =
    let l = String.length url in
    if ((l > 0) && (string_after url (l - 1)) = "/") then
        (first_chars url (l - 1))
    else
        url;;

(* Function to download an html page, then scan it for further url references *)
let scan_page (url: string): page_record =

    (* Trim the trailing slash, if any *)
    let trimmed_url = trim_trailing_slash url in

    (* Have we already scanned this page? If so, skip it *)
    let existing = Hashtbl.find download_results trimmed_url in
    match existing with
        | Some s ->
            Printf.printf "Page %s already in cache.\n" trimmed_url;
            s
        | None ->

            (* Okay, let's can this page *)
            Printf.printf "Downloading %s...\n" trimmed_url;
            let page_contents = Lwt_main.run (download_one_page trimmed_url) in
            let reference_list = scan_for_urls page_contents in
            let merged_refs = merge_url_with_refs trimmed_url reference_list in
            let page = {
                url = trimmed_url;
                contents = page_contents;
                references = merged_refs;
            } in

            (* Store this page so we don't have to do it again *)
            Hashtbl.add_exn download_results ~key:trimmed_url ~data:page;

            (* Store all the references in the queue *)
            let iterfun (s: string) =
                (*Printf.printf "Queueing %s...\n" s;*)
                Queue.enqueue pending_urls s
                in
            List.iter ~f:iterfun merged_refs;

            (* Here's the information about the page *)
            (*print_page_record page;*)
            page;;

(* Determine if a URL is within a site *)
let within_site (base_url: string) (url: string): bool =
    let length_to_scan = min (String.length base_url) (String.length url) in
    let left_snippet = first_chars base_url length_to_scan in
    let right_snippet = first_chars url length_to_scan in
    String.equal left_snippet right_snippet;;

(* Scan a site, and all sites within it *)
let scan_site (url: string) =
    Queue.enqueue pending_urls url;
    while (Queue.length pending_urls) > 0 do
        flush stdout;
        match Queue.dequeue pending_urls with
          | Some next_url ->

              (* We only want to scan it if it's underneath the main site *)
              if within_site url next_url then begin
                  let page = scan_page next_url in
                  Printf.printf "Queue length now %d.\n" (Queue.length pending_urls);
                  Some page;

              (* If it's not within the main site, skip it *)
              end else begin
                  Printf.printf "Skipping %s; it's not within the site.\n" next_url;
                  None;
              end;

          | None -> None
    done;;

(* Program *)
scan_site "http://www.spence.net";;

    (*in
    Printf.printf "Page scanned: %s\n" p.url;
    Printf.printf "Size: %d\n" (String.length p.contents);
    Printf.printf "References: %d\n" (List.count ~f:(fun x -> true) p.references);
    Printf.printf "Done\n\n\n";;
*)
