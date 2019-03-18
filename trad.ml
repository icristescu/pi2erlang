type id = string

(* !a(x).P *)
type alpi = Par of alpi*alpi
	    |Inp of id *id*alpi
	    |BInp of id *id*alpi
	    |Out of id *id
	    |New of id *alpi
	    |Emp

let rec bangs_first p =
  match p with
    |BInp (a, x, pa) -> Inp(a, x, Par(BInp (a, x, pa), pa))
    |New (a, pa) -> New (a, (bangs_first pa))
    |Par (pa, pb) -> Par ((bangs_first pa), (bangs_first pb))
    |other -> other

let rec translate st =
  let len = String.length st in
  if (st.[0] = '&') then (*new*)
    let nb = String.index st '.' in
    New ((String.sub st 1 (nb-1)), translate (String.sub st (nb+1) (len-nb-1)))
  else
    if (st.[0] = '(') then (*par*)
      let nb = try String.index st '|'
               with Not_found -> -1 in
      if (nb = -1) then translate (String.sub st 1 (len -2))
      else
        let st1 = String.sub st 1 (nb-1) in
	let st2 =
          String.init (len-nb) (fun i -> if i = 0 then '(' else st.[nb+i]) in
    	    Par((translate st2), (translate st1))
    else
      if (st.[0] = '-') then (*out*)
	let nb = String.index st '(' in
	 Out((String.sub st 1 (nb-1)), (String.sub st (nb+1) (len-nb-2)))
      else
	if (st.[0] = '!') then (* bang inp*)
	  let nb = String.index st '(' in
	  let nb1 = try String.index st '.'
	  with Not_found -> 0 in
	    if (nb1 = 0) then
   	      BInp ((String.sub st 1 (nb-1)), (String.sub st (nb+1) (len-nb-2)), Emp)
	    else
	      BInp ((String.sub st 1 (nb-1)), (String.sub st (nb+1) (nb1-nb-2)),
		   (translate (String.sub st (nb1+1) (len-nb1-1))))
	else (* inp*)
	  let nb = String.index st '(' in
	  let nb1 = try String.index st '.'
	  with Not_found -> 0 in
	    if (nb1 = 0) then
   	      Inp ((String.sub st 0 nb), (String.sub st (nb+1) (len-nb-2)), Emp)
	    else
	      Inp ((String.sub st 0 nb), (String.sub st (nb+1) (nb1-nb-2)),
		   (translate (String.sub st (nb1+1) (len-nb1-1))))

(* each id newly created has associated a number, a process id and a function for its next process*)

let print_comma outc ls =
  let nb = List.length ls in
  let _ = if (nb != 0) then Printf.fprintf outc ",\n" in
    nb

let rec write_forevers outc p ls width depth ls_n =
  match p with
    |New (a, pa) -> write_forevers outc pa ls width depth ls_n
    |Par (pa, pb) -> let _ = write_forevers outc pa ls width depth ls_n in
	write_forevers outc pb ls width depth ls_n
    |BInp (a, x, pb) ->
       let () = Printf.fprintf outc "forever%d%d()-> " width depth in
       let (_, nb) = try List.find (fun (pa, _)-> pa = a) ls
       with Not_found -> ("x", -1) in
       let _ =
	 if (nb = -1) then
	 (*if the process is not in the list then is given as argument and it's in the list ls_n *)
	   let (_, nb_n) = List.find (fun (pa, _)-> pa = a) ls_n in
	     Printf.fprintf outc "V%s = get(%d), ( V%s ! {input, V%s, f%d%d})" a nb_n a a width depth
	 else Printf.fprintf outc "(p%d ! {input, p%d, f%d%d}) " nb nb width depth in
	 Printf.fprintf outc ", forever%d%d() .\n" width depth

    | _ -> Printf.fprintf outc ""

(* elements of ls = (process_name, number, argument, arg_renamed) *)
(* if inp or out before creating the name - error *)
let rec parse outc p ls depth width ls_n=
  match p with
    |New (a, pa) ->
       let nb = print_comma outc ls in
       let () = Printf.fprintf outc "register(p%d, spawn(b, channel, [0, 0]))" nb in
	 parse outc pa ((a, nb)::ls) depth width ls_n
    |Par (pa, pb) -> parse outc pb (parse outc pa ls depth width ls_n) depth (width+1) ls_n

    |Inp (a, x, pb) ->
       let _ = print_comma outc ls in
       let (_, nb) = try List.find (fun (pa, _)-> pa = a) ls
       with Not_found -> ("x", -1) in
       let _ = if (nb = -1) then
	   (*if the process is not in the list then is given as argument and it's in the list ls_n *)
	 let (_, nb_n) = List.find (fun (pa, _)-> pa = a) ls_n in
	   Printf.fprintf outc "V%s = get(%d), ( V%s ! {input, V%s, f%d%d})" a nb_n a a width depth
       else Printf.fprintf outc "(p%d ! {input, p%d, f%d%d}) " nb nb width depth in
	   ls

    |BInp (a, x, pb) ->
       let _ = print_comma outc ls in
       let () = Printf.fprintf outc "D = get(), spawn (b, beforeforever, [forever%d%d, D]) "  width depth in
	 ls

    |Out (a, x) ->
       let _ = print_comma outc ls in
       (*** x *)
       let (_, nb_x)= try List.find (fun (pa, _)-> pa = x) ls
       with Not_found -> ("x", -1) in
       let (_, nb_xn) = try List.find (fun (pa, _)-> pa = x) ls_n
       with Not_found -> ("x", -1) in
       let _ =  if (nb_x = -1) then
	 if (nb_xn = -1) then Printf.fprintf outc ""
	 else  Printf.fprintf outc " V%s = get(%d)," x nb_xn
       else Printf.fprintf outc "" in

	 (*** a *)
       let (_, nb)= try List.find (fun (pa, _)-> pa = a) ls
       with Not_found -> ("x", -1) in
       let _ = if (nb = -1) then
	 (*if the a is not in the list then is given as argument and it's in the list ls_n *)
	 let (_, nb_n) = List.find (fun (pa, _)-> pa = a) ls_n in
	   Printf.fprintf outc "V%s = get(%d), ( V%s ! {output,"  a nb_n a
       else Printf.fprintf outc "(p%d ! {output," nb in

       (*** x *)
	 (*the name sent can either be a name not created, a name already created (i.e on the list)
	   or a name received as an arg*)
       let _ =
	 if (nb_x = -1) then
	   if (nb_xn = -1) then Printf.fprintf outc " %s})" x
	   else
	     Printf.fprintf outc " V%s})" x
	 else Printf.fprintf outc " p%d })" nb_x in

	 ls
    |Emp -> ls

(* add x as a new bound name if not already in *)
let add_bound_name x ls ls_n =
  let len = List.length ls_n in
  let (_, nb_x)= try List.find (fun (pa, _)-> pa = x) ls
  with Not_found -> ("x", -1) in
    if (nb_x = -1) then
      let (_, nb_xn) = try List.find (fun (pa, _)-> pa = x) ls_n
      with Not_found -> ("x", -1) in
	if (nb_xn = -1) then
	    (x, len)::ls_n
	else (x, len)::ls_n
    else (x, len)::ls_n

(* for each input process create the functions for its subprocesses *)
let rec create_functions outc p ls depth width ls_n =
  match p with
    |New (a, pa) -> create_functions outc pa ls depth width ls_n
    |Par (pa, pb) ->
      create_functions outc pb
        (create_functions outc pa ls depth width ls_n) depth (width +1) ls_n
    |Inp (a, x, pa)|BInp (a, x, pa) ->
       let ls_n1 = add_bound_name x ls ls_n in
       let () = Printf.fprintf outc "f%d%d() -> done " width depth in
       let ls1 = parse outc pa ls (depth + 1) width ls_n1 in
       let () = Printf.fprintf outc ".\n " in
       let () = write_forevers outc pa ls1 width (depth + 1) ls_n1 in
       let _ = create_functions outc pa ls1 (depth + 1) width ls_n1 in
	 ls

    |Emp -> ls
    |Out(a, x) -> ls

let build outc p =
  let () = Printf.fprintf outc "start() -> " in
  let ls = parse outc p [] 0 0 [] in
  let () = Printf.fprintf outc ". \n" in
  let () =  write_forevers outc p ls 0 0 [] in
    create_functions outc p ls 0 0 []

let write_erlang_file filename original =
    let outc = open_out filename in
    let () = Printf.fprintf outc "## translating %s\n\n" original in
    outc


let () =
  let test1 = translate (Tests.p2 ()) in
  let outc = write_erlang_file "test2" (Tests.p2 ()) in
  let _ = build outc test1 in
  flush outc; close_out outc
