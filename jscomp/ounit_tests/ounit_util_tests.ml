
let ((>::),
     (>:::)) = OUnit.((>::),(>:::))


let (=~) = 
  OUnit.assert_equal
  ~printer:Ext_obj.dump
let suites = 
  __FILE__ >:::
  [
    __LOC__ >:: begin fun _ -> 
      Ext_pervasives.nat_of_string_exn "003" =~ 3;
      (try Ext_pervasives.nat_of_string_exn "0a" |> ignore ; 2 with _ -> -1)  =~ -1;
    end;
    __LOC__ >:: begin fun _ -> 
      let cursor = ref 0 in 
      let v = Ext_pervasives.parse_nat_of_string "123a" cursor in 
      (v, !cursor) =~ (123,3);
      cursor := 0;
      let v = Ext_pervasives.parse_nat_of_string "a" cursor in 
      (v,!cursor) =~ (0,0)
    end;

    __LOC__ >:: begin fun _ -> 
      for i = 0 to 0xff do 
        let buf = Ext_buffer.create 0 in 
        Ext_buffer.add_int_1 buf i;
        let s = Ext_buffer.contents buf in 
        s =~ String.make 1 (Char.chr i);
        Ext_string.get_int_1 s 0 =~ i
      done 
    end;

    __LOC__ >:: begin fun _ -> 
      for i = 0x100 to 0xff_ff do 
        let buf = Ext_buffer.create 0 in 
        Ext_buffer.add_int_2 buf i;
        let s = Ext_buffer.contents buf in         
        Ext_string.get_int_2 s 0 =~ i
      done ;
      let buf = Ext_buffer.create 0 in 
      Ext_buffer.add_int_3 buf 0x1_ff_ff;
      Ext_string.get_int_3 (Ext_buffer.contents buf) 0 =~ 0x1_ff_ff
      ;
      let buf = Ext_buffer.create 0 in 
      Ext_buffer.add_int_4 buf 0x1_ff_ff_ff;
      Ext_string.get_int_4 (Ext_buffer.contents buf) 0 =~ 0x1_ff_ff_ff
    end

  ]