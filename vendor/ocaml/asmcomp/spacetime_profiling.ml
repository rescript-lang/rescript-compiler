(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let node_num_header_words = 2 (* [Node_num_header_words] in the runtime. *)
let index_within_node = ref node_num_header_words
(* The [lazy]s are to ensure that we don't create [Ident.t]s at toplevel
   when not using Spacetime profiling.  (This could cause stamps to differ
   between bytecode and native .cmis when no .mli is present, e.g.
   arch.ml.) *)
let spacetime_node = ref (lazy (Cmm.Cvar (Ident.create "dummy")))
let spacetime_node_ident = ref (lazy (Ident.create "dummy"))
let current_function_label = ref ""
let direct_tail_call_point_indexes = ref []

let reverse_shape = ref ([] : Mach.spacetime_shape)

let something_was_instrumented () =
  !index_within_node > node_num_header_words

let next_index_within_node ~part_of_shape ~label =
  let index = !index_within_node in
  begin match part_of_shape with
  | Mach.Direct_call_point _ ->
    incr index_within_node;
    if Config.enable_call_counts then begin
      incr index_within_node
    end
  | Mach.Indirect_call_point ->
    incr index_within_node
  | Mach.Allocation_point ->
    incr index_within_node;
    incr index_within_node;
    incr index_within_node
  end;
  reverse_shape := (part_of_shape, label) :: !reverse_shape;
  index

let reset ~spacetime_node_ident:ident ~function_label =
  index_within_node := node_num_header_words;
  spacetime_node := lazy (Cmm.Cvar ident);
  spacetime_node_ident := lazy ident;
  direct_tail_call_point_indexes := [];
  current_function_label := function_label;
  reverse_shape := []

let code_for_function_prologue ~function_name ~node_hole =
  let node = Ident.create "node" in
  let new_node = Ident.create "new_node" in
  let must_allocate_node = Ident.create "must_allocate_node" in
  let is_new_node = Ident.create "is_new_node" in
  let no_tail_calls = List.length !direct_tail_call_point_indexes < 1 in
  let dbg = Debuginfo.none in
  let open Cmm in
  let initialize_direct_tail_call_points_and_return_node =
    let new_node_encoded = Ident.create "new_node_encoded" in
    (* The callee node pointers within direct tail call points must initially
       point back at the start of the current node and be marked as per
       [Encode_tail_caller_node] in the runtime. *)
    let indexes = !direct_tail_call_point_indexes in
    let body =
      List.fold_left (fun init_code index ->
          (* Cf. [Direct_callee_node] in the runtime. *)
          let offset_in_bytes = index * Arch.size_addr in
          Csequence (
            Cop (Cstore (Word_int, Lambda.Assignment),
              [Cop (Caddi, [Cvar new_node; Cconst_int offset_in_bytes], dbg);
               Cvar new_node_encoded], dbg),
            init_code))
        (Cvar new_node)
        indexes
    in
    match indexes with
    | [] -> body
    | _ ->
      Clet (new_node_encoded,
        (* Cf. [Encode_tail_caller_node] in the runtime. *)
        Cop (Cor, [Cvar new_node; Cconst_int 1], dbg),
        body)
  in
  let pc = Ident.create "pc" in
  Clet (node, Cop (Cload (Word_int, Asttypes.Mutable), [Cvar node_hole], dbg),
    Clet (must_allocate_node,
      Cop (Cand, [Cvar node; Cconst_int 1], dbg),
      Cifthenelse (
        Cop (Ccmpi Cne, [Cvar must_allocate_node; Cconst_int 1], dbg),
        Cvar node,
        Clet (is_new_node,
          Clet (pc, Cconst_symbol function_name,
            Cop (Cextcall ("caml_spacetime_allocate_node",
                [| Int |], false, None),
              [Cconst_int (1 (* header *) + !index_within_node);
               Cvar pc;
               Cvar node_hole;
              ],
              dbg)),
            Clet (new_node,
              Cop (Cload (Word_int, Asttypes.Mutable), [Cvar node_hole], dbg),
              if no_tail_calls then Cvar new_node
              else
                Cifthenelse (
                  Cop (Ccmpi Ceq, [Cvar is_new_node; Cconst_int 0], dbg),
                  Cvar new_node,
                  initialize_direct_tail_call_points_and_return_node))))))

let code_for_blockheader ~value's_header ~node ~dbg =
  let num_words = Nativeint.shift_right_logical value's_header 10 in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let existing_count = Ident.create "existing_count" in
  let profinfo = Ident.create "profinfo" in
  let address_of_profinfo = Ident.create "address_of_profinfo" in
  let label = Cmm.new_label () in
  let index_within_node =
    next_index_within_node ~part_of_shape:Mach.Allocation_point ~label
  in
  let offset_into_node = Arch.size_addr * index_within_node in
  let open Cmm in
  let generate_new_profinfo =
    (* This will generate a static branch to a function that should usually
       be in the cache, which hopefully gives a good code size/performance
       balance.
       The "Some label" is important: it provides the link between the shape
       table, the allocation point, and the frame descriptor table---enabling
       the latter table to be used for resolving a program counter at such
       a point to a location.
    *)
    Cop (Cextcall ("caml_spacetime_generate_profinfo", [| Int |],
        false, Some label),
      [Cvar address_of_profinfo;
       Cconst_int (index_within_node + 1)],
      dbg)
  in
  (* Check if we have already allocated a profinfo value for this allocation
     point with the current backtrace.  If so, use that value; if not,
     allocate a new one. *)
  Clet (address_of_profinfo,
    Cop (Caddi, [
      Cvar node;
      Cconst_int offset_into_node;
    ], dbg),
    Clet (existing_profinfo,
        Cop (Cload (Word_int, Asttypes.Mutable), [Cvar address_of_profinfo],
          dbg),
      Clet (profinfo,
        Cifthenelse (
          Cop (Ccmpi Cne, [Cvar existing_profinfo; Cconst_int 1 (* () *)], dbg),
          Cvar existing_profinfo,
          generate_new_profinfo),
        Clet (existing_count,
          Cop (Cload (Word_int, Asttypes.Mutable), [
            Cop (Caddi,
              [Cvar address_of_profinfo; Cconst_int Arch.size_addr], dbg)
          ], dbg),
          Csequence (
            Cop (Cstore (Word_int, Lambda.Assignment),
              [Cop (Caddi,
                [Cvar address_of_profinfo; Cconst_int Arch.size_addr], dbg);
                Cop (Caddi, [
                  Cvar existing_count;
                  (* N.B. "*2" since the count is an OCaml integer.
                     The "1 +" is to count the value's header. *)
                  Cconst_int (2 * (1 + Nativeint.to_int num_words));
                ], dbg);
              ], dbg),
            (* [profinfo] looks like a black [Infix_tag] header.  Instead of
               having to mask [profinfo] before ORing it with the desired
               header, we can use an XOR trick, to keep code size down. *)
            let value's_header =
              Nativeint.logxor value's_header
                (Nativeint.logor
                  ((Nativeint.logor (Nativeint.of_int Obj.infix_tag)
                    (Nativeint.shift_left 3n (* <- Caml_black *) 8)))
                  (Nativeint.shift_left
                    (* The following is the [Infix_offset_val], in words. *)
                    (Nativeint.of_int (index_within_node + 1)) 10))
            in
            Cop (Cxor, [Cvar profinfo; Cconst_natint value's_header], dbg))))))

type callee =
  | Direct of string
  | Indirect of Cmm.expression

let code_for_call ~node ~callee ~is_tail ~label =
  (* We treat self recursive calls as tail calls to avoid blow-ups in the
     graph. *)
  let is_self_recursive_call =
    match callee with
    | Direct callee -> callee = !current_function_label
    | Indirect _ -> false
  in
  let is_tail = is_tail || is_self_recursive_call in
  let index_within_node =
    match callee with
    | Direct callee ->
      next_index_within_node
        ~part_of_shape:(Mach.Direct_call_point { callee; })
        ~label
    | Indirect _ ->
      next_index_within_node ~part_of_shape:Mach.Indirect_call_point ~label
  in
  begin match callee with
    (* If this is a direct tail call point, we need to note down its index,
       so the correct initialization code can be emitted in the prologue. *)
    | Direct _ when is_tail ->
      direct_tail_call_point_indexes :=
        index_within_node::!direct_tail_call_point_indexes
    | Direct _ | Indirect _ -> ()
  end;
  let place_within_node = Ident.create "place_within_node" in
  let dbg = Debuginfo.none in
  let open Cmm in
  Clet (place_within_node,
    Cop (Caddi, [node; Cconst_int (index_within_node * Arch.size_addr)], dbg),
    (* The following code returns the address that is to be moved into the
       (hard) node hole pointer register immediately before the call.
       (That move is inserted in [Selectgen].) *)
    match callee with
    | Direct _callee ->
      if Config.enable_call_counts then begin
        let count_addr = Ident.create "call_count_addr" in
        let count = Ident.create "call_count" in
        Clet (count_addr,
          Cop (Caddi, [Cvar place_within_node; Cconst_int Arch.size_addr], dbg),
          Clet (count,
            Cop (Cload (Word_int, Asttypes.Mutable), [Cvar count_addr], dbg),
            Csequence (
              Cop (Cstore (Word_int, Lambda.Assignment),
                (* Adding 2 really means adding 1; the count is encoded
                   as an OCaml integer. *)
                [Cvar count_addr; Cop (Caddi, [Cvar count; Cconst_int 2], dbg)],
                dbg),
              Cvar place_within_node)))
      end else begin
        Cvar place_within_node
      end
    | Indirect callee ->
      let caller_node =
        if is_tail then node
        else Cconst_int 1  (* [Val_unit] *)
      in
      Cop (Cextcall ("caml_spacetime_indirect_node_hole_ptr",
          [| Int |], false, None),
        [callee; Cvar place_within_node; caller_node],
        dbg))

class virtual instruction_selection = object (self)
  inherit Selectgen.selector_generic as super

  (* [disable_instrumentation] ensures that we don't try to instrument the
     instrumentation... *)
  val mutable disable_instrumentation = false

  method private instrument_direct_call ~env ~func ~is_tail ~label_after =
    let instrumentation =
      code_for_call
        ~node:(Lazy.force !spacetime_node)
        ~callee:(Direct func)
        ~is_tail
        ~label:label_after
    in
    match self#emit_expr env instrumentation with
    | None -> assert false
    | Some reg -> Some reg

  method private instrument_indirect_call ~env ~callee ~is_tail
      ~label_after =
    (* [callee] is a pseudoregister, so we have to bind it in the environment
       and reference the variable to which it is bound. *)
    let callee_ident = Ident.create "callee" in
    let env = Selectgen.env_add callee_ident [| callee |] env in
    let instrumentation =
      code_for_call
        ~node:(Lazy.force !spacetime_node)
        ~callee:(Indirect (Cmm.Cvar callee_ident))
        ~is_tail
        ~label:label_after
    in
    match self#emit_expr env instrumentation with
    | None -> assert false
    | Some reg -> Some reg

  method private can_instrument () =
    Config.spacetime && not disable_instrumentation

  method! about_to_emit_call env desc arg =
    if not (self#can_instrument ()) then None
    else
      let module M = Mach in
      match desc with
      | M.Iop (M.Icall_imm { func; label_after; }) ->
        assert (Array.length arg = 0);
        self#instrument_direct_call ~env ~func ~is_tail:false ~label_after
      | M.Iop (M.Icall_ind { label_after; }) ->
        assert (Array.length arg = 1);
        self#instrument_indirect_call ~env ~callee:arg.(0)
          ~is_tail:false ~label_after
      | M.Iop (M.Itailcall_imm { func; label_after; }) ->
        assert (Array.length arg = 0);
        self#instrument_direct_call ~env ~func ~is_tail:true ~label_after
      | M.Iop (M.Itailcall_ind { label_after; }) ->
        assert (Array.length arg = 1);
        self#instrument_indirect_call ~env ~callee:arg.(0)
          ~is_tail:true ~label_after
      | M.Iop (M.Iextcall { func; alloc = true; label_after; }) ->
        (* N.B. No need to instrument "noalloc" external calls. *)
        assert (Array.length arg = 0);
        self#instrument_direct_call ~env ~func ~is_tail:false ~label_after
      | _ -> None

  method private instrument_blockheader ~env ~value's_header ~dbg =
    let instrumentation =
      code_for_blockheader
        ~node:(Lazy.force !spacetime_node_ident)
        ~value's_header ~dbg
    in
    self#emit_expr env instrumentation

  method private emit_prologue f ~node_hole ~env =
    (* We don't need the prologue unless we inserted some instrumentation.
       This corresponds to adding the prologue if the function contains one
       or more call or allocation points. *)
    if something_was_instrumented () then begin
      let prologue_cmm =
        code_for_function_prologue ~function_name:f.Cmm.fun_name ~node_hole
      in
      disable_instrumentation <- true;
      let node_temp_reg =
        match self#emit_expr env prologue_cmm with
        | None ->
          Misc.fatal_error "Spacetime prologue instruction \
              selection did not yield a destination register"
        | Some node_temp_reg -> node_temp_reg
      in
      disable_instrumentation <- false;
      let node = Lazy.force !spacetime_node_ident in
      let node_reg = Selectgen.env_find node env in
      self#insert_moves node_temp_reg node_reg
    end

  method! emit_blockheader env n dbg =
    if self#can_instrument () then begin
      disable_instrumentation <- true;
      let result = self#instrument_blockheader ~env ~value's_header:n ~dbg in
      disable_instrumentation <- false;
      result
    end else begin
      super#emit_blockheader env n dbg
    end

  method! select_allocation words =
    if self#can_instrument () then begin
      (* Leave space for a direct call point.  We cannot easily insert any
         instrumentation code, so the fields are filled in instead by
         [caml_spacetime_caml_garbage_collection]. *)
      let label = Cmm.new_label () in
      let index =
        next_index_within_node
          ~part_of_shape:(Mach.Direct_call_point { callee = "caml_call_gc"; })
          ~label
      in
      Mach.Ialloc {
        words;
        label_after_call_gc = Some label;
        spacetime_index = index;
      }
    end else begin
      super#select_allocation words
    end

  method! select_allocation_args env =
    if self#can_instrument () then begin
      let regs = Selectgen.env_find (Lazy.force !spacetime_node_ident) env in
      match regs with
      | [| reg |] -> [| reg |]
      | _ -> failwith "Expected one register only for spacetime_node_ident"
    end else begin
      super#select_allocation_args env
    end

  method! select_checkbound () =
    (* This follows [select_allocation], above. *)
    if self#can_instrument () then begin
      let label = Cmm.new_label () in
      let index =
        next_index_within_node
          ~part_of_shape:(
            Mach.Direct_call_point { callee = "caml_ml_array_bound_error"; })
          ~label
      in
      Mach.Icheckbound {
        label_after_error = Some label;
        spacetime_index = index;
      }
    end else begin
      super#select_checkbound ()
    end

  method! select_checkbound_extra_args () =
    if self#can_instrument () then begin
      (* This follows [select_allocation_args], above. *)
      [Cmm.Cvar (Lazy.force !spacetime_node_ident)]
    end else begin
      super#select_checkbound_extra_args ()
    end

  method! initial_env () =
    let env = super#initial_env () in
    if Config.spacetime then
      Selectgen.env_add (Lazy.force !spacetime_node_ident)
        (self#regs_for Cmm.typ_int) env
    else
      env

  method! emit_fundecl f =
    if Config.spacetime then begin
      disable_instrumentation <- false;
      let node = Ident.create "spacetime_node" in
      reset ~spacetime_node_ident:node ~function_label:f.Cmm.fun_name
    end;
    super#emit_fundecl f

  method! insert_prologue f ~loc_arg ~rarg ~spacetime_node_hole ~env =
    let fun_spacetime_shape =
      super#insert_prologue f ~loc_arg ~rarg ~spacetime_node_hole ~env
    in
    (* CR-soon mshinwell: add check to make sure the node size doesn't exceed
       the chunk size of the allocator *)
    if not Config.spacetime then fun_spacetime_shape
    else begin
      let node_hole, node_hole_reg =
        match spacetime_node_hole with
        | None -> assert false
        | Some (node_hole, reg) -> node_hole, reg
      in
      self#insert_moves [| Proc.loc_spacetime_node_hole |] node_hole_reg;
      self#emit_prologue f ~node_hole ~env;
      match !reverse_shape with
      | [] -> None
      (* N.B. We do not reverse the shape list, since the function that
         reconstructs it (caml_spacetime_shape_table) reverses it again. *)
      | reverse_shape -> Some reverse_shape
    end
end
