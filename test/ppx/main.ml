open Test
open Lwt

(* Used for the "structure let" test, below. This is wrapped up by the PPX in a
   call to Lwt_main.run, which is executed at module load time. We can't use a
   local module inside the tester function, because that function is run inside
   an outer call to Lwt_main.run, and nested calls to Lwt_main.run are not
   allowed. *)
let%lwt structure_let_result = Lwt.return true

let suite = suite "ppx" [
  test "let"
    (fun () ->
       let%lwt x = return 3 in
       return (x + 1 = 4)
    ) ;

  test "nested let"
    (fun () ->
       let%lwt x = return 3 in
       let%lwt y = return 4 in
       return (x + y = 7)
    ) ;

  test "and let"
    (fun () ->
       let%lwt x = return 3
       and y = return 4 in
       return (x + y = 7)
    ) ;

  test "and let + cancel"
    (fun () ->
       let t1, _ = Lwt.task () in
       let t2, _ = Lwt.task () in
       let c1 = ref false in
       let c2 = ref false in
       let t =
         let%lwt x =
           try%lwt
             let%lwt () = t1 in return 3
           with Lwt.Canceled -> c1 := true; Lwt.return 1
         and y =
           try%lwt
             let%lwt () = t2 in return 4
           with Lwt.Canceled -> c2 := true; Lwt.return 2
         in
         return (x + y)
       in
       Lwt.cancel t;
       return (!c1 && !c2)) ;

  test "match"
    (fun () ->
       let x = Lwt.return (Some 3) in
       match%lwt x with
       | Some x -> return (x + 1 = 4)
       | None -> return false
    ) ;

  test "match-exn"
    (fun () ->
       let x = Lwt.return (Some 3) in
       let x' = Lwt.fail Not_found in
       let%lwt a =
         match%lwt x with
         | exception Not_found -> return false
         | Some x -> return (x = 3)
         | None -> return false
       and b =
         match%lwt x' with
         | exception Not_found -> return true
         | _ -> return false
       in
       Lwt.return (a && b)
    ) ;

  test "if"
    (fun () ->
       let x = Lwt.return true in
       let%lwt a =
         if%lwt x then Lwt.return_true else Lwt.return_false
       in
       let%lwt b =
         if%lwt x>|= not then Lwt.return_false else Lwt.return_true
       in
       (if%lwt x >|= not then Lwt.return_unit) >>= fun () ->
       Lwt.return (a && b)
    ) ;

  test "for" (* Test for proper sequencing *)
    (fun () ->
       let r = ref [] in
       let f x =
         let%lwt () = Lwt_unix.sleep 0.2 in Lwt.return (r := x :: !r)
       in
       let%lwt () =
         for%lwt x = 3 to 5 do f x done
       in return (!r = [5 ; 4 ; 3])
    ) ;

  test "while" (* Test for proper sequencing *)
    (fun () ->
       let r = ref [] in
       let f x =
         let%lwt () = Lwt_unix.sleep 0.2 in Lwt.return (r := x :: !r)
       in
       let%lwt () =
         let c = ref 2 in
         while%lwt !c < 5 do incr c ; f !c done
       in return (!r = [5 ; 4 ; 3])
    ) ;

  test "assert"
    (fun () ->
       let%lwt () = assert%lwt true
       in return true
    ) ;

  test "try"
    (fun () ->
       try%lwt
         Lwt.fail Not_found
       with _ -> return true
    ) [@warning("@8@11")] ;

  test "try raise"
    (fun () ->
       try%lwt
         raise Not_found
       with _ -> return true
    ) [@warning("@8@11")] ;

  test "try fallback"
    (fun () ->
       try%lwt
         try%lwt
           Lwt.fail Not_found
         with Failure _ -> return false
       with Not_found -> return true
    ) [@warning("@8@11")] ;

  test "finally body"
    (fun () ->
       let x = ref false in
       begin
         (try%lwt
           return_unit
         with
         | _ -> return_unit
         ) [%finally x := true; return_unit]
       end >>= fun () ->
       return !x
    ) ;

  test "finally exn"
    (fun () ->
       let x = ref false in
       begin
         (try%lwt
           raise Not_found
         with
         | _ -> return_unit
         ) [%finally x := true; return_unit]
       end >>= fun () ->
       return !x
    ) ;

  test "finally exn default"
    (fun () ->
       let x = ref false in
       try%lwt
         ( raise Not_found )[%finally x := true; return_unit]
         >>= fun () ->
         return false
       with Not_found ->
         return !x
    ) ;

  test "structure let"
    (fun () ->
       Lwt.return structure_let_result
    ) ;
]

let _ = Test.run "ppx" [ suite ]
