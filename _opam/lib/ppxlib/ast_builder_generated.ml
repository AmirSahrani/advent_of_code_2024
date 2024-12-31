open Import
module M =
  struct
    let attribute ~loc  =
      ((fun ~name ->
          ((fun ~payload ->
              { attr_name = name; attr_payload = payload; attr_loc = loc })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let binding_op ~loc  =
      ((fun ~op ->
          ((fun ~pat ->
              ((fun ~exp ->
                  {
                    pbop_op = op;
                    pbop_pat = pat;
                    pbop_exp = exp;
                    pbop_loc = loc
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let case ~lhs  =
      ((fun ~guard ->
          ((fun ~rhs -> { pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let pcl_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_structure ~loc  =
      ((fun x0 ->
          { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_structure x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_fun ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      {
                        pcl_attributes = [];
                        pcl_loc = loc;
                        pcl_desc = (Pcl_fun (x0, x1, x2, x3))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_let ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcl_attributes = [];
                    pcl_loc = loc;
                    pcl_desc = (Pcl_let (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_extension ~loc  =
      ((fun x0 ->
          { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_inherit ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcf_attributes = [];
                    pcf_loc = loc;
                    pcf_desc = (Pcf_inherit (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_val ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_val x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_method ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_method x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_constraint ~loc  =
      ((fun x0 ->
          {
            pcf_attributes = [];
            pcf_loc = loc;
            pcf_desc = (Pcf_constraint x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_initializer ~loc  =
      ((fun x0 ->
          {
            pcf_attributes = [];
            pcf_loc = loc;
            pcf_desc = (Pcf_initializer x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_attribute ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_attribute x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_extension ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let class_infos ~loc  =
      ((fun ~virt ->
          ((fun ~params ->
              ((fun ~name ->
                  ((fun ~expr ->
                      {
                        pci_virt = virt;
                        pci_params = params;
                        pci_name = name;
                        pci_expr = expr;
                        pci_loc = loc;
                        pci_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let class_signature ~self  =
      ((fun ~fields -> { pcsig_self = self; pcsig_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let class_structure ~self  =
      ((fun ~fields -> { pcstr_self = self; pcstr_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let pcty_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_signature ~loc  =
      ((fun x0 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_signature x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_arrow ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcty_attributes = [];
                    pcty_loc = loc;
                    pcty_desc = (Pcty_arrow (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_extension ~loc  =
      ((fun x0 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_inherit ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_inherit x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_val ~loc  =
      ((fun x0 ->
          { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_val x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_method ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_method x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_constraint ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_constraint x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_attribute ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_attribute x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_extension ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let constructor_declaration ~loc  =
      ((fun ~name ->
          ((fun ~vars ->
              ((fun ~args ->
                  ((fun ~res ->
                      {
                        pcd_name = name;
                        pcd_vars = vars;
                        pcd_args = args;
                        pcd_res = res;
                        pcd_loc = loc;
                        pcd_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_any ~loc  =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = Ptyp_any
      }
    let ptyp_var ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_var x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_arrow ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    ptyp_loc_stack = [];
                    ptyp_attributes = [];
                    ptyp_loc = loc;
                    ptyp_desc = (Ptyp_arrow (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_tuple ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_object ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_object (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_class ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_class (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_alias ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_alias (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    ptyp_loc_stack = [];
                    ptyp_attributes = [];
                    ptyp_loc = loc;
                    ptyp_desc = (Ptyp_variant (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_poly ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_poly (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_package ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_package x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_extension ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_string ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_string x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_int ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pdira_loc = loc; pdira_desc = (Pdir_int (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_ident ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_ident x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_bool ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_bool x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_ident ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constant ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_constant x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_let ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_let (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_function ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_function x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_fun ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      {
                        pexp_loc_stack = [];
                        pexp_attributes = [];
                        pexp_loc = loc;
                        pexp_desc = (Pexp_fun (x0, x1, x2, x3))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_match ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_match (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_try ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_try (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_tuple ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_construct ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_construct (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_variant (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_record ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_record (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_field ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_field (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setfield ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_setfield (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_array ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_array x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_ifthenelse ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_ifthenelse (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_sequence ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_sequence (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_while ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_while (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_for ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      ((fun x4 ->
                          {
                            pexp_loc_stack = [];
                            pexp_attributes = [];
                            pexp_loc = loc;
                            pexp_desc = (Pexp_for (x0, x1, x2, x3, x4))
                          })
                      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ][@ppxlib.migration.stop_taking
                                                              ][@ppxlib.migration.stop_taking
                                                                 ][@ppxlib.migration.stop_taking
                                                                    ]))
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_coerce ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_coerce (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_send ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_send (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_new ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_new x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setinstvar ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_setinstvar (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_override ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_override x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letmodule ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_letmodule (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letexception ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_letexception (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_assert ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_assert x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_lazy ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_lazy x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_poly ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_poly (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_object ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_object x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_newtype ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_newtype (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_pack ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_pack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letop ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_letop x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_extension ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_unreachable ~loc  =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = Pexp_unreachable
      }
    let extension_constructor ~loc  =
      ((fun ~name ->
          ((fun ~kind ->
              {
                pext_name = name;
                pext_kind = kind;
                pext_loc = loc;
                pext_attributes = []
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let include_infos ~loc  =
      ((fun mod_ ->
          { pincl_mod = mod_; pincl_loc = loc; pincl_attributes = [] })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let label_declaration ~loc  =
      ((fun ~name ->
          ((fun ~mutable_ ->
              ((fun ~type_ ->
                  {
                    pld_name = name;
                    pld_mutable = mutable_;
                    pld_type = type_;
                    pld_loc = loc;
                    pld_attributes = []
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let letop ~let_  =
      ((fun ~ands -> ((fun ~body -> { let_; ands; body })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let location ~start  =
      ((fun ~end_ ->
          ((fun ~ghost ->
              { loc_start = start; loc_end = end_; loc_ghost = ghost })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let module_binding ~loc  =
      ((fun ~name ->
          ((fun ~expr ->
              {
                pmb_name = name;
                pmb_expr = expr;
                pmb_attributes = [];
                pmb_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_declaration ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              {
                pmd_name = name;
                pmd_type = type_;
                pmd_attributes = [];
                pmd_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_ident ~loc  =
      ((fun x0 ->
          { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_structure ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_structure x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_functor ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_functor (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_unpack ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_unpack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_extension ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_substitution ~loc  =
      ((fun ~name ->
          ((fun ~manifest ->
              {
                pms_name = name;
                pms_manifest = manifest;
                pms_attributes = [];
                pms_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_ident ~loc  =
      ((fun x0 ->
          { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_signature ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_signature x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_functor ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmty_attributes = [];
                pmty_loc = loc;
                pmty_desc = (Pmty_functor (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_with ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmty_attributes = [];
                pmty_loc = loc;
                pmty_desc = (Pmty_with (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_typeof ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_typeof x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_extension ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_alias ~loc  =
      ((fun x0 ->
          { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_alias x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_type_declaration ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              {
                pmtd_name = name;
                pmtd_type = type_;
                pmtd_attributes = [];
                pmtd_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let otag ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pof_attributes = [];
                pof_loc = loc;
                pof_desc = (Otag (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let oinherit ~loc  =
      ((fun x0 ->
          { pof_attributes = []; pof_loc = loc; pof_desc = (Oinherit x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let open_infos ~loc  =
      ((fun ~expr ->
          ((fun ~override ->
              {
                popen_expr = expr;
                popen_override = override;
                popen_loc = loc;
                popen_attributes = []
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_any ~loc  =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = Ppat_any
      }
    let ppat_var ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_var x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_alias ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_alias (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constant ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_constant x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_interval ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_interval (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_tuple ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_construct ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_construct (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_variant (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_record ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_record (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_array ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_array x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_or ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_or (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_type ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_type x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_lazy ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_lazy x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_unpack ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_unpack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_exception ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_exception x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_extension ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let position ~fname  =
      ((fun ~lnum ->
          ((fun ~bol ->
              ((fun ~cnum ->
                  {
                    pos_fname = fname;
                    pos_lnum = lnum;
                    pos_bol = bol;
                    pos_cnum = cnum
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let rtag ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    prf_attributes = [];
                    prf_loc = loc;
                    prf_desc = (Rtag (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let rinherit ~loc  =
      ((fun x0 ->
          { prf_attributes = []; prf_loc = loc; prf_desc = (Rinherit x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_value ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_value x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_type ~loc  =
      ((fun x0 ->
          ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_type (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typesubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_typesubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typext ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_typext x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_exception ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_exception x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_module ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_module x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modsubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modsubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_recmodule ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_recmodule x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modtype ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modtype x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modtypesubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modtypesubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_open ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_open x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_include ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_include x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_class ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_class x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_class_type ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_class_type x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_attribute ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_attribute x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_extension ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              { psig_loc = loc; psig_desc = (Psig_extension (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_eval ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_eval (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_value ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_value (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_primitive ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_primitive x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_type ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_type (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_typext ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_typext x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_exception ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_exception x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_module ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_module x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_recmodule ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_recmodule x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_modtype ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_modtype x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_open ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_open x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_class ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_class x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_class_type ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_class_type x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_include ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_include x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_attribute ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_attribute x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_extension ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              { pstr_loc = loc; pstr_desc = (Pstr_extension (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let toplevel_directive ~loc  =
      ((fun ~name ->
          ((fun ~arg -> { pdir_name = name; pdir_arg = arg; pdir_loc = loc })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_declaration ~loc  =
      ((fun ~name ->
          ((fun ~params ->
              ((fun ~cstrs ->
                  ((fun ~kind ->
                      ((fun ~private_ ->
                          ((fun ~manifest ->
                              {
                                ptype_name = name;
                                ptype_params = params;
                                ptype_cstrs = cstrs;
                                ptype_kind = kind;
                                ptype_private = private_;
                                ptype_manifest = manifest;
                                ptype_attributes = [];
                                ptype_loc = loc
                              })
                          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                            ]))
                      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                        ]))
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_exception ~loc  =
      ((fun constructor ->
          {
            ptyexn_constructor = constructor;
            ptyexn_loc = loc;
            ptyexn_attributes = []
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_extension ~loc  =
      ((fun ~path ->
          ((fun ~params ->
              ((fun ~constructors ->
                  ((fun ~private_ ->
                      {
                        ptyext_path = path;
                        ptyext_params = params;
                        ptyext_constructors = constructors;
                        ptyext_private = private_;
                        ptyext_loc = loc;
                        ptyext_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let value_binding ~loc  =
      ((fun ~pat ->
          ((fun ~expr ->
              {
                pvb_pat = pat;
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let value_description ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              ((fun ~prim ->
                  {
                    pval_name = name;
                    pval_type = type_;
                    pval_prim = prim;
                    pval_attributes = [];
                    pval_loc = loc
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
  end
module Make(Loc:sig val loc : Location.t end) =
  struct
    let loc = Loc.loc
    let attribute ~name  =
      ((fun ~payload ->
          { attr_name = name; attr_payload = payload; attr_loc = loc })
      [@ppxlib.migration.stop_taking ])
    let binding_op ~op  =
      ((fun ~pat ->
          ((fun ~exp ->
              { pbop_op = op; pbop_pat = pat; pbop_exp = exp; pbop_loc = loc
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let case ~lhs  =
      ((fun ~guard ->
          ((fun ~rhs -> { pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let pcl_constr x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_structure x0 =
      { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_structure x0) }
    let pcl_fun x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  {
                    pcl_attributes = [];
                    pcl_loc = loc;
                    pcl_desc = (Pcl_fun (x0, x1, x2, x3))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_apply x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_let x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_let (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_constraint x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_extension x0 =
      { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_extension x0) }
    let pcl_open x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_inherit x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcf_attributes = [];
                pcf_loc = loc;
                pcf_desc = (Pcf_inherit (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_val x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_val x0) }
    let pcf_method x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_method x0) }
    let pcf_constraint x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_constraint x0) }
    let pcf_initializer x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_initializer x0) }
    let pcf_attribute x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_attribute x0) }
    let pcf_extension x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_extension x0) }
    let class_infos ~virt  =
      ((fun ~params ->
          ((fun ~name ->
              ((fun ~expr ->
                  {
                    pci_virt = virt;
                    pci_params = params;
                    pci_name = name;
                    pci_expr = expr;
                    pci_loc = loc;
                    pci_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let class_signature ~self  =
      ((fun ~fields -> { pcsig_self = self; pcsig_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let class_structure ~self  =
      ((fun ~fields -> { pcstr_self = self; pcstr_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let pcty_constr x0 =
      ((fun x1 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_signature x0 =
      { pcty_attributes = []; pcty_loc = loc; pcty_desc = (Pcty_signature x0)
      }
    let pcty_arrow x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_arrow (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_extension x0 =
      { pcty_attributes = []; pcty_loc = loc; pcty_desc = (Pcty_extension x0)
      }
    let pcty_open x0 =
      ((fun x1 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_inherit x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_inherit x0) }
    let pctf_val x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_val x0) }
    let pctf_method x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_method x0) }
    let pctf_constraint x0 =
      {
        pctf_attributes = [];
        pctf_loc = loc;
        pctf_desc = (Pctf_constraint x0)
      }
    let pctf_attribute x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_attribute x0)
      }
    let pctf_extension x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_extension x0)
      }
    let constructor_declaration ~name  =
      ((fun ~vars ->
          ((fun ~args ->
              ((fun ~res ->
                  {
                    pcd_name = name;
                    pcd_vars = vars;
                    pcd_args = args;
                    pcd_res = res;
                    pcd_loc = loc;
                    pcd_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let ptyp_any =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = Ptyp_any
      }
    let ptyp_var x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_var x0)
      }
    let ptyp_arrow x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_arrow (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_tuple x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_tuple x0)
      }
    let ptyp_constr x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_object x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_object (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_class x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_class (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_alias x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_alias (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_variant x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_variant (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_poly x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_poly (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_package x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_package x0)
      }
    let ptyp_extension x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_extension x0)
      }
    let pdir_string x0 = { pdira_loc = loc; pdira_desc = (Pdir_string x0) }
    let pdir_int x0 =
      ((fun x1 -> { pdira_loc = loc; pdira_desc = (Pdir_int (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_ident x0 = { pdira_loc = loc; pdira_desc = (Pdir_ident x0) }
    let pdir_bool x0 = { pdira_loc = loc; pdira_desc = (Pdir_bool x0) }
    let pexp_ident x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_ident x0)
      }
    let pexp_constant x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_constant x0)
      }
    let pexp_let x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_let (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_function x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_function x0)
      }
    let pexp_fun x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_fun (x0, x1, x2, x3))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_apply x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_match x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_match (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_try x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_try (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_tuple x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_tuple x0)
      }
    let pexp_construct x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_construct (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_variant x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_variant (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_record x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_record (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_field x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_field (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setfield x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_setfield (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_array x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_array x0)
      }
    let pexp_ifthenelse x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_ifthenelse (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_sequence x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_sequence (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_while x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_while (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_for x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  ((fun x4 ->
                      {
                        pexp_loc_stack = [];
                        pexp_attributes = [];
                        pexp_loc = loc;
                        pexp_desc = (Pexp_for (x0, x1, x2, x3, x4))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constraint x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_coerce x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_coerce (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_send x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_send (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_new x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_new x0)
      }
    let pexp_setinstvar x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_setinstvar (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_override x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_override x0)
      }
    let pexp_letmodule x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_letmodule (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letexception x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_letexception (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_assert x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_assert x0)
      }
    let pexp_lazy x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_lazy x0)
      }
    let pexp_poly x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_poly (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_object x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_object x0)
      }
    let pexp_newtype x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_newtype (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_pack x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_pack x0)
      }
    let pexp_open x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letop x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_letop x0)
      }
    let pexp_extension x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_extension x0)
      }
    let pexp_unreachable =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = Pexp_unreachable
      }
    let extension_constructor ~name  =
      ((fun ~kind ->
          {
            pext_name = name;
            pext_kind = kind;
            pext_loc = loc;
            pext_attributes = []
          })
      [@ppxlib.migration.stop_taking ])
    let include_infos mod_ =
      { pincl_mod = mod_; pincl_loc = loc; pincl_attributes = [] }
    let label_declaration ~name  =
      ((fun ~mutable_ ->
          ((fun ~type_ ->
              {
                pld_name = name;
                pld_mutable = mutable_;
                pld_type = type_;
                pld_loc = loc;
                pld_attributes = []
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let letop ~let_  =
      ((fun ~ands -> ((fun ~body -> { let_; ands; body })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let location ~start  =
      ((fun ~end_ ->
          ((fun ~ghost ->
              { loc_start = start; loc_end = end_; loc_ghost = ghost })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let module_binding ~name  =
      ((fun ~expr ->
          {
            pmb_name = name;
            pmb_expr = expr;
            pmb_attributes = [];
            pmb_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let module_declaration ~name  =
      ((fun ~type_ ->
          {
            pmd_name = name;
            pmd_type = type_;
            pmd_attributes = [];
            pmd_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let pmod_ident x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_ident x0) }
    let pmod_structure x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_structure x0)
      }
    let pmod_functor x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_functor (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_apply x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_constraint x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_unpack x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_unpack x0) }
    let pmod_extension x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_extension x0)
      }
    let module_substitution ~name  =
      ((fun ~manifest ->
          {
            pms_name = name;
            pms_manifest = manifest;
            pms_attributes = [];
            pms_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let pmty_ident x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_ident x0) }
    let pmty_signature x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_signature x0)
      }
    let pmty_functor x0 =
      ((fun x1 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_functor (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_with x0 =
      ((fun x1 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_with (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_typeof x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_typeof x0) }
    let pmty_extension x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_extension x0)
      }
    let pmty_alias x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_alias x0) }
    let module_type_declaration ~name  =
      ((fun ~type_ ->
          {
            pmtd_name = name;
            pmtd_type = type_;
            pmtd_attributes = [];
            pmtd_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let otag x0 =
      ((fun x1 ->
          { pof_attributes = []; pof_loc = loc; pof_desc = (Otag (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let oinherit x0 =
      { pof_attributes = []; pof_loc = loc; pof_desc = (Oinherit x0) }
    let open_infos ~expr  =
      ((fun ~override ->
          {
            popen_expr = expr;
            popen_override = override;
            popen_loc = loc;
            popen_attributes = []
          })
      [@ppxlib.migration.stop_taking ])
    let ppat_any =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = Ppat_any
      }
    let ppat_var x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_var x0)
      }
    let ppat_alias x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_alias (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constant x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_constant x0)
      }
    let ppat_interval x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_interval (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_tuple x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_tuple x0)
      }
    let ppat_construct x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_construct (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_variant x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_variant (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_record x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_record (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_array x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_array x0)
      }
    let ppat_or x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_or (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constraint x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_type x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_type x0)
      }
    let ppat_lazy x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_lazy x0)
      }
    let ppat_unpack x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_unpack x0)
      }
    let ppat_exception x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_exception x0)
      }
    let ppat_extension x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_extension x0)
      }
    let ppat_open x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let position ~fname  =
      ((fun ~lnum ->
          ((fun ~bol ->
              ((fun ~cnum ->
                  {
                    pos_fname = fname;
                    pos_lnum = lnum;
                    pos_bol = bol;
                    pos_cnum = cnum
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let rtag x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                prf_attributes = [];
                prf_loc = loc;
                prf_desc = (Rtag (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let rinherit x0 =
      { prf_attributes = []; prf_loc = loc; prf_desc = (Rinherit x0) }
    let psig_value x0 = { psig_loc = loc; psig_desc = (Psig_value x0) }
    let psig_type x0 =
      ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_type (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typesubst x0 =
      { psig_loc = loc; psig_desc = (Psig_typesubst x0) }
    let psig_typext x0 = { psig_loc = loc; psig_desc = (Psig_typext x0) }
    let psig_exception x0 =
      { psig_loc = loc; psig_desc = (Psig_exception x0) }
    let psig_module x0 = { psig_loc = loc; psig_desc = (Psig_module x0) }
    let psig_modsubst x0 = { psig_loc = loc; psig_desc = (Psig_modsubst x0) }
    let psig_recmodule x0 =
      { psig_loc = loc; psig_desc = (Psig_recmodule x0) }
    let psig_modtype x0 = { psig_loc = loc; psig_desc = (Psig_modtype x0) }
    let psig_modtypesubst x0 =
      { psig_loc = loc; psig_desc = (Psig_modtypesubst x0) }
    let psig_open x0 = { psig_loc = loc; psig_desc = (Psig_open x0) }
    let psig_include x0 = { psig_loc = loc; psig_desc = (Psig_include x0) }
    let psig_class x0 = { psig_loc = loc; psig_desc = (Psig_class x0) }
    let psig_class_type x0 =
      { psig_loc = loc; psig_desc = (Psig_class_type x0) }
    let psig_attribute x0 =
      { psig_loc = loc; psig_desc = (Psig_attribute x0) }
    let psig_extension x0 =
      ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_extension (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_eval x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_eval (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_value x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_value (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_primitive x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_primitive x0) }
    let pstr_type x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_type (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_typext x0 = { pstr_loc = loc; pstr_desc = (Pstr_typext x0) }
    let pstr_exception x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_exception x0) }
    let pstr_module x0 = { pstr_loc = loc; pstr_desc = (Pstr_module x0) }
    let pstr_recmodule x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_recmodule x0) }
    let pstr_modtype x0 = { pstr_loc = loc; pstr_desc = (Pstr_modtype x0) }
    let pstr_open x0 = { pstr_loc = loc; pstr_desc = (Pstr_open x0) }
    let pstr_class x0 = { pstr_loc = loc; pstr_desc = (Pstr_class x0) }
    let pstr_class_type x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_class_type x0) }
    let pstr_include x0 = { pstr_loc = loc; pstr_desc = (Pstr_include x0) }
    let pstr_attribute x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_attribute x0) }
    let pstr_extension x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_extension (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let toplevel_directive ~name  =
      ((fun ~arg -> { pdir_name = name; pdir_arg = arg; pdir_loc = loc })
      [@ppxlib.migration.stop_taking ])
    let type_declaration ~name  =
      ((fun ~params ->
          ((fun ~cstrs ->
              ((fun ~kind ->
                  ((fun ~private_ ->
                      ((fun ~manifest ->
                          {
                            ptype_name = name;
                            ptype_params = params;
                            ptype_cstrs = cstrs;
                            ptype_kind = kind;
                            ptype_private = private_;
                            ptype_manifest = manifest;
                            ptype_attributes = [];
                            ptype_loc = loc
                          })
                      [@ppxlib.migration.stop_taking ]))
                  [@ppxlib.migration.stop_taking ]))
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let type_exception constructor =
      {
        ptyexn_constructor = constructor;
        ptyexn_loc = loc;
        ptyexn_attributes = []
      }
    let type_extension ~path  =
      ((fun ~params ->
          ((fun ~constructors ->
              ((fun ~private_ ->
                  {
                    ptyext_path = path;
                    ptyext_params = params;
                    ptyext_constructors = constructors;
                    ptyext_private = private_;
                    ptyext_loc = loc;
                    ptyext_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let value_binding ~pat  =
      ((fun ~expr ->
          {
            pvb_pat = pat;
            pvb_expr = expr;
            pvb_attributes = [];
            pvb_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let value_description ~name  =
      ((fun ~type_ ->
          ((fun ~prim ->
              {
                pval_name = name;
                pval_type = type_;
                pval_prim = prim;
                pval_attributes = [];
                pval_loc = loc
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
  end
