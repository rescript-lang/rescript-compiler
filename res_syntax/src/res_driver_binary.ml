let printEngine =
  Res_driver.
    {
      printImplementation =
        (fun ~width:_ ~filename ~comments:_ structure ->
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout filename;
          output_value stdout structure);
      printInterface =
        (fun ~width:_ ~filename ~comments:_ signature ->
          output_string stdout Config.ast_intf_magic_number;
          output_value stdout filename;
          output_value stdout signature);
    }
