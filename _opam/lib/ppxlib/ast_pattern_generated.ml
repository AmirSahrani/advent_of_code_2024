open Import
open Ast_pattern0
let nolabel =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Nolabel -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Nolabel")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let labelled (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Labelled x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "Labelled")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let optional (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Optional x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "Optional")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let attribute ~name:(T name)  =
  ((fun ~payload:(T payload) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       let k = name ctx (x.attr_name).loc (x.attr_name).txt k in
                       let k = payload ctx loc x.attr_payload k in k)
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let binding_op ~op:(T op)  =
  ((fun ~pat:(T pat) ->
      ((fun ~exp:(T exp) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           let k = op ctx (x.pbop_op).loc (x.pbop_op).txt k in
                           let k = pat ctx loc x.pbop_pat k in
                           let k = exp ctx loc x.pbop_exp k in k)
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let case ~lhs:(T lhs)  =
  ((fun ~guard:(T guard) ->
      ((fun ~rhs:(T rhs) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           let k = lhs ctx loc x.pc_lhs k in
                           let k = guard ctx loc x.pc_guard k in
                           let k = rhs ctx loc x.pc_rhs k in k)
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let pcl_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcl_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcl_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcl_loc in
             let k = f1 ctx loc x.pcl_attributes k in
             let x = { x with pcl_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcl_constr (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcl_attributes;
                       (let loc = x.pcl_loc in
                        let x = x.pcl_desc in
                        match x with
                        | Pcl_constr (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constr"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcl_structure (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcl_attributes;
                   (let loc = x.pcl_loc in
                    let x = x.pcl_desc in
                    match x with
                    | Pcl_structure x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "structure"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcl_fun (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          ((fun (T f3) ->
              T
                (fun ctx ->
                   ((fun _loc ->
                       ((fun x ->
                           ((fun k ->
                               Common.assert_no_attributes x.pcl_attributes;
                               (let loc = x.pcl_loc in
                                let x = x.pcl_desc in
                                match x with
                                | Pcl_fun (x0, x1, x2, x3) ->
                                    (ctx.matched <- (ctx.matched + 1);
                                     (let k = f0 ctx loc x0 k in
                                      let k = f1 ctx loc x1 k in
                                      let k = f2 ctx loc x2 k in
                                      let k = f3 ctx loc x3 k in k))
                                | _ -> fail loc "fun"))
                           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
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
                                                           ][@ppxlib.migration.stop_taking
                                                              ])))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcl_apply (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcl_attributes;
                       (let loc = x.pcl_loc in
                        let x = x.pcl_desc in
                        match x with
                        | Pcl_apply (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "apply"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcl_let (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pcl_attributes;
                           (let loc = x.pcl_loc in
                            let x = x.pcl_desc in
                            match x with
                            | Pcl_let (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "let"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcl_constraint (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcl_attributes;
                       (let loc = x.pcl_loc in
                        let x = x.pcl_desc in
                        match x with
                        | Pcl_constraint (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constraint"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcl_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcl_attributes;
                   (let loc = x.pcl_loc in
                    let x = x.pcl_desc in
                    match x with
                    | Pcl_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcl_open (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcl_attributes;
                       (let loc = x.pcl_loc in
                        let x = x.pcl_desc in
                        match x with
                        | Pcl_open (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "open"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcf_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcf_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcf_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcf_loc in
             let k = f1 ctx loc x.pcf_attributes k in
             let x = { x with pcf_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcf_inherit (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pcf_attributes;
                           (let loc = x.pcf_loc in
                            let x = x.pcf_desc in
                            match x with
                            | Pcf_inherit (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "inherit"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcf_val (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_val x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "val"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcf_method (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_method x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "method"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcf_constraint (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_constraint x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "constraint"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcf_initializer (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_initializer x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "initializer"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcf_attribute (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_attribute x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "attribute"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcf_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcf_attributes;
                   (let loc = x.pcf_loc in
                    let x = x.pcf_desc in
                    match x with
                    | Pcf_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let cfk_virtual (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Cfk_virtual x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "virtual")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let cfk_concrete (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Cfk_concrete (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "concrete")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let class_infos_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pci_loc in
             let k = f1 ctx loc x.pci_attributes k in
             let x = { x with pci_attributes = [] } in
             let k = f2 ctx loc x k in k)
let class_infos ~virt:(T virt)  =
  ((fun ~params:(T params) ->
      ((fun ~name:(T name) ->
          ((fun ~expr:(T expr) ->
              T
                (fun ctx ->
                   ((fun loc ->
                       ((fun x ->
                           ((fun k ->
                               Common.assert_no_attributes x.pci_attributes;
                               (let k = virt ctx loc x.pci_virt k in
                                let k = params ctx loc x.pci_params k in
                                let k =
                                  name ctx (x.pci_name).loc (x.pci_name).txt
                                    k in
                                let k = expr ctx loc x.pci_expr k in k))
                           [@ppxlib.migration.stop_taking ]))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ])))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let class_signature ~self:(T self)  =
  ((fun ~fields:(T fields) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       let k = self ctx loc x.pcsig_self k in
                       let k = fields ctx loc x.pcsig_fields k in k)
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let class_structure ~self:(T self)  =
  ((fun ~fields:(T fields) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       let k = self ctx loc x.pcstr_self k in
                       let k = fields ctx loc x.pcstr_fields k in k)
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let pcty_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcty_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pcty_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcty_loc in
             let k = f1 ctx loc x.pcty_attributes k in
             let x = { x with pcty_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pcty_constr (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcty_attributes;
                       (let loc = x.pcty_loc in
                        let x = x.pcty_desc in
                        match x with
                        | Pcty_constr (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constr"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcty_signature (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcty_attributes;
                   (let loc = x.pcty_loc in
                    let x = x.pcty_desc in
                    match x with
                    | Pcty_signature x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "signature"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcty_arrow (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pcty_attributes;
                           (let loc = x.pcty_loc in
                            let x = x.pcty_desc in
                            match x with
                            | Pcty_arrow (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "arrow"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcty_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pcty_attributes;
                   (let loc = x.pcty_loc in
                    let x = x.pcty_desc in
                    match x with
                    | Pcty_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcty_open (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pcty_attributes;
                       (let loc = x.pcty_loc in
                        let x = x.pcty_desc in
                        match x with
                        | Pcty_open (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "open"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pctf_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pctf_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pctf_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pctf_loc in
             let k = f1 ctx loc x.pctf_attributes k in
             let x = { x with pctf_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pctf_inherit (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_inherit x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "inherit"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pctf_val (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_val x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "val"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pctf_method (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_method x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "method"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pctf_constraint (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_constraint x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "constraint"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pctf_attribute (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_attribute x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "attribute"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pctf_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pctf_attributes;
                   (let loc = x.pctf_loc in
                    let x = x.pctf_desc in
                    match x with
                    | Pctf_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let closed =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Closed -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Closed")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let open_ =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Open -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Open")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let pconst_integer (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pconst_integer (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "integer")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pconst_char (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Pconst_char x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "char")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pconst_string (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           match x with
                           | Pconst_string (x0, x1, x2) ->
                               (ctx.matched <- (ctx.matched + 1);
                                (let k = f0 ctx loc x0 k in
                                 let k = f1 ctx loc x1 k in
                                 let k = f2 ctx loc x2 k in k))
                           | _ -> fail loc "string")
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pconst_float (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pconst_float (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "float")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pcstr_tuple (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Pcstr_tuple x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "tuple")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pcstr_record (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Pcstr_record x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "record")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let constructor_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pcd_loc in
             let k = f1 ctx loc x.pcd_attributes k in
             let x = { x with pcd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let constructor_declaration ~name:(T name)  =
  ((fun ~vars:(T vars) ->
      ((fun ~args:(T args) ->
          ((fun ~res:(T res) ->
              T
                (fun ctx ->
                   ((fun loc ->
                       ((fun x ->
                           ((fun k ->
                               Common.assert_no_attributes x.pcd_attributes;
                               (let k =
                                  name ctx (x.pcd_name).loc (x.pcd_name).txt
                                    k in
                                let k = vars ctx loc x.pcd_vars k in
                                let k = args ctx loc x.pcd_args k in
                                let k = res ctx loc x.pcd_res k in k))
                           [@ppxlib.migration.stop_taking ]))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ])))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let ptyp_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ptyp_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let ptyp_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ptyp_loc in
             let k = f1 ctx loc x.ptyp_attributes k in
             let x = { x with ptyp_attributes = [] } in
             let k = f2 ctx loc x k in k)
let ptyp_any =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyp_attributes;
                   (let loc = x.ptyp_loc in
                    let x = x.ptyp_desc in
                    match x with
                    | Ptyp_any -> (ctx.matched <- (ctx.matched + 1); k)
                    | _ -> fail loc "any"))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let ptyp_var (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyp_attributes;
                   (let loc = x.ptyp_loc in
                    let x = x.ptyp_desc in
                    match x with
                    | Ptyp_var x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "var"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptyp_arrow (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.ptyp_attributes;
                           (let loc = x.ptyp_loc in
                            let x = x.ptyp_desc in
                            match x with
                            | Ptyp_arrow (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "arrow"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_tuple (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyp_attributes;
                   (let loc = x.ptyp_loc in
                    let x = x.ptyp_desc in
                    match x with
                    | Ptyp_tuple x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "tuple"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptyp_constr (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ptyp_attributes;
                       (let loc = x.ptyp_loc in
                        let x = x.ptyp_desc in
                        match x with
                        | Ptyp_constr (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constr"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_object (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ptyp_attributes;
                       (let loc = x.ptyp_loc in
                        let x = x.ptyp_desc in
                        match x with
                        | Ptyp_object (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "object"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_class (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ptyp_attributes;
                       (let loc = x.ptyp_loc in
                        let x = x.ptyp_desc in
                        match x with
                        | Ptyp_class (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "class"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_alias (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ptyp_attributes;
                       (let loc = x.ptyp_loc in
                        let x = x.ptyp_desc in
                        match x with
                        | Ptyp_alias (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "alias"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_variant (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.ptyp_attributes;
                           (let loc = x.ptyp_loc in
                            let x = x.ptyp_desc in
                            match x with
                            | Ptyp_variant (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "variant"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_poly (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ptyp_attributes;
                       (let loc = x.ptyp_loc in
                        let x = x.ptyp_desc in
                        match x with
                        | Ptyp_poly (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "poly"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ptyp_package (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyp_attributes;
                   (let loc = x.ptyp_loc in
                    let x = x.ptyp_desc in
                    match x with
                    | Ptyp_package x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "package"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptyp_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyp_attributes;
                   (let loc = x.ptyp_loc in
                    let x = x.ptyp_desc in
                    match x with
                    | Ptyp_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let upto =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Upto -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Upto")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let downto_ =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Downto -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Downto")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let pdira_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pdira_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pdir_string (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pdira_loc in
                   let x = x.pdira_desc in
                   match x with
                   | Pdir_string x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "string")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pdir_int (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.pdira_loc in
                       let x = x.pdira_desc in
                       match x with
                       | Pdir_int (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "int")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pdir_ident (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pdira_loc in
                   let x = x.pdira_desc in
                   match x with
                   | Pdir_ident x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "ident")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pdir_bool (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pdira_loc in
                   let x = x.pdira_desc in
                   match x with
                   | Pdir_bool x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "bool")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pexp_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pexp_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pexp_loc in
             let k = f1 ctx loc x.pexp_attributes k in
             let x = { x with pexp_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pexp_ident (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_ident x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "ident"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_constant (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_constant x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "constant"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_let (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pexp_attributes;
                           (let loc = x.pexp_loc in
                            let x = x.pexp_desc in
                            match x with
                            | Pexp_let (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "let"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_function (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_function x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "function"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_fun (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          ((fun (T f3) ->
              T
                (fun ctx ->
                   ((fun _loc ->
                       ((fun x ->
                           ((fun k ->
                               Common.assert_no_attributes x.pexp_attributes;
                               (let loc = x.pexp_loc in
                                let x = x.pexp_desc in
                                match x with
                                | Pexp_fun (x0, x1, x2, x3) ->
                                    (ctx.matched <- (ctx.matched + 1);
                                     (let k = f0 ctx loc x0 k in
                                      let k = f1 ctx loc x1 k in
                                      let k = f2 ctx loc x2 k in
                                      let k = f3 ctx loc x3 k in k))
                                | _ -> fail loc "fun"))
                           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
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
                                                           ][@ppxlib.migration.stop_taking
                                                              ])))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_apply (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_apply (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "apply"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_match (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_match (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "match"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_try (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_try (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "try"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_tuple (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_tuple x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "tuple"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_construct (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_construct (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "construct"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_variant (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_variant (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "variant"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_record (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_record (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "record"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_field (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_field (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx x1.loc x1.txt k in k))
                        | _ -> fail loc "field"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_setfield (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pexp_attributes;
                           (let loc = x.pexp_loc in
                            let x = x.pexp_desc in
                            match x with
                            | Pexp_setfield (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx x1.loc x1.txt k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "setfield"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_array (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_array x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "array"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_ifthenelse (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pexp_attributes;
                           (let loc = x.pexp_loc in
                            let x = x.pexp_desc in
                            match x with
                            | Pexp_ifthenelse (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "ifthenelse"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_sequence (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_sequence (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "sequence"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_while (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_while (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "while"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_for (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          ((fun (T f3) ->
              ((fun (T f4) ->
                  T
                    (fun ctx ->
                       ((fun _loc ->
                           ((fun x ->
                               ((fun k ->
                                   Common.assert_no_attributes
                                     x.pexp_attributes;
                                   (let loc = x.pexp_loc in
                                    let x = x.pexp_desc in
                                    match x with
                                    | Pexp_for (x0, x1, x2, x3, x4) ->
                                        (ctx.matched <- (ctx.matched + 1);
                                         (let k = f0 ctx loc x0 k in
                                          let k = f1 ctx loc x1 k in
                                          let k = f2 ctx loc x2 k in
                                          let k = f3 ctx loc x3 k in
                                          let k = f4 ctx loc x4 k in k))
                                    | _ -> fail loc "for"))
                               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                                 ][@ppxlib.migration.stop_taking
                                                                    ]
                               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                                 ][@ppxlib.migration.stop_taking
                                                                    ]))
                           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                             ][@ppxlib.migration.stop_taking
                                                                ][@ppxlib.migration.stop_taking
                                                                   ][@ppxlib.migration.stop_taking
                                                                    ]
                           [@ppxlib.migration.stop_taking ]))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ][@ppxlib.migration.stop_taking
                                                                  ][@ppxlib.migration.stop_taking
                                                                    ])))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ][@ppxlib.migration.stop_taking
                                                         ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_constraint (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_constraint (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constraint"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_coerce (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pexp_attributes;
                           (let loc = x.pexp_loc in
                            let x = x.pexp_desc in
                            match x with
                            | Pexp_coerce (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx loc x0 k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "coerce"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_send (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_send (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx x1.loc x1.txt k in k))
                        | _ -> fail loc "send"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_new (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_new x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "new"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_setinstvar (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_setinstvar (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "setinstvar"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_override (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_override x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "override"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_letmodule (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pexp_attributes;
                           (let loc = x.pexp_loc in
                            let x = x.pexp_desc in
                            match x with
                            | Pexp_letmodule (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx x0.loc x0.txt k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "letmodule"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_letexception (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_letexception (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "letexception"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_assert (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_assert x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "assert"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_lazy (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_lazy x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "lazy"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_poly (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_poly (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "poly"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_object (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_object x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "object"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_newtype (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_newtype (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "newtype"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_pack (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_pack x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "pack"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_open (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pexp_attributes;
                       (let loc = x.pexp_loc in
                        let x = x.pexp_desc in
                        match x with
                        | Pexp_open (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "open"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pexp_letop (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_letop x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "letop"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pexp_unreachable =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pexp_attributes;
                   (let loc = x.pexp_loc in
                    let x = x.pexp_desc in
                    match x with
                    | Pexp_unreachable ->
                        (ctx.matched <- (ctx.matched + 1); k)
                    | _ -> fail loc "unreachable"))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let extension_constructor_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pext_loc in
             let k = f1 ctx loc x.pext_attributes k in
             let x = { x with pext_attributes = [] } in
             let k = f2 ctx loc x k in k)
let extension_constructor ~name:(T name)  =
  ((fun ~kind:(T kind) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pext_attributes;
                       (let k =
                          name ctx (x.pext_name).loc (x.pext_name).txt k in
                        let k = kind ctx loc x.pext_kind k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let pext_decl (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           match x with
                           | Pext_decl (x0, x1, x2) ->
                               (ctx.matched <- (ctx.matched + 1);
                                (let k = f0 ctx loc x0 k in
                                 let k = f1 ctx loc x1 k in
                                 let k = f2 ctx loc x2 k in k))
                           | _ -> fail loc "decl")
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pext_rebind (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Pext_rebind x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx x0.loc x0.txt k in k))
                   | _ -> fail loc "rebind")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let unit =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Unit -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Unit")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let named (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Named (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "Named")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let include_infos_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pincl_loc in
             let k = f1 ctx loc x.pincl_attributes k in
             let x = { x with pincl_attributes = [] } in
             let k = f2 ctx loc x k in k)
let include_infos ~mod_:(T mod_)  =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pincl_attributes;
                   (let k = mod_ ctx loc x.pincl_mod k in k))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let injective =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Injective -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Injective")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let noinjectivity =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | NoInjectivity -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "NoInjectivity")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let label_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pld_loc in
             let k = f1 ctx loc x.pld_attributes k in
             let x = { x with pld_attributes = [] } in
             let k = f2 ctx loc x k in k)
let label_declaration ~name:(T name)  =
  ((fun ~mutable_:(T mutable_) ->
      ((fun ~type_:(T type_) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pld_attributes;
                           (let k =
                              name ctx (x.pld_name).loc (x.pld_name).txt k in
                            let k = mutable_ ctx loc x.pld_mutable k in
                            let k = type_ ctx loc x.pld_type k in k))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let letop ~let_:(T let_)  =
  ((fun ~ands:(T ands) ->
      ((fun ~body:(T body) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           let k = let_ ctx loc x.let_ k in
                           let k = ands ctx loc x.ands k in
                           let k = body ctx loc x.body k in k)
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let location ~start:(T start)  =
  ((fun ~end_:(T end_) ->
      ((fun ~ghost:(T ghost) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           let k = start ctx loc x.loc_start k in
                           let k = end_ ctx loc x.loc_end k in
                           let k = ghost ctx loc x.loc_ghost k in k)
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let lident (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Lident x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "Lident")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ldot (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Ldot (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "Ldot")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let lapply (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Lapply (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "Lapply")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let module_binding_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmb_loc in
             let k = f1 ctx loc x.pmb_attributes k in
             let x = { x with pmb_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_binding ~name:(T name)  =
  ((fun ~expr:(T expr) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmb_attributes;
                       (let k = name ctx (x.pmb_name).loc (x.pmb_name).txt k in
                        let k = expr ctx loc x.pmb_expr k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let module_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmd_loc in
             let k = f1 ctx loc x.pmd_attributes k in
             let x = { x with pmd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_declaration ~name:(T name)  =
  ((fun ~type_:(T type_) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmd_attributes;
                       (let k = name ctx (x.pmd_name).loc (x.pmd_name).txt k in
                        let k = type_ ctx loc x.pmd_type k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let pmod_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmod_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pmod_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmod_loc in
             let k = f1 ctx loc x.pmod_attributes k in
             let x = { x with pmod_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pmod_ident (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmod_attributes;
                   (let loc = x.pmod_loc in
                    let x = x.pmod_desc in
                    match x with
                    | Pmod_ident x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "ident"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmod_structure (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmod_attributes;
                   (let loc = x.pmod_loc in
                    let x = x.pmod_desc in
                    match x with
                    | Pmod_structure x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "structure"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmod_functor (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmod_attributes;
                       (let loc = x.pmod_loc in
                        let x = x.pmod_desc in
                        match x with
                        | Pmod_functor (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "functor"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pmod_apply (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmod_attributes;
                       (let loc = x.pmod_loc in
                        let x = x.pmod_desc in
                        match x with
                        | Pmod_apply (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "apply"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pmod_constraint (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmod_attributes;
                       (let loc = x.pmod_loc in
                        let x = x.pmod_desc in
                        match x with
                        | Pmod_constraint (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constraint"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pmod_unpack (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmod_attributes;
                   (let loc = x.pmod_loc in
                    let x = x.pmod_desc in
                    match x with
                    | Pmod_unpack x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "unpack"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmod_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmod_attributes;
                   (let loc = x.pmod_loc in
                    let x = x.pmod_desc in
                    match x with
                    | Pmod_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let module_substitution_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pms_loc in
             let k = f1 ctx loc x.pms_attributes k in
             let x = { x with pms_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_substitution ~name:(T name)  =
  ((fun ~manifest:(T manifest) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pms_attributes;
                       (let k = name ctx (x.pms_name).loc (x.pms_name).txt k in
                        let k =
                          manifest ctx (x.pms_manifest).loc
                            (x.pms_manifest).txt k in
                        k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let pmty_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmty_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pmty_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmty_loc in
             let k = f1 ctx loc x.pmty_attributes k in
             let x = { x with pmty_attributes = [] } in
             let k = f2 ctx loc x k in k)
let pmty_ident (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmty_attributes;
                   (let loc = x.pmty_loc in
                    let x = x.pmty_desc in
                    match x with
                    | Pmty_ident x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "ident"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmty_signature (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmty_attributes;
                   (let loc = x.pmty_loc in
                    let x = x.pmty_desc in
                    match x with
                    | Pmty_signature x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "signature"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmty_functor (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmty_attributes;
                       (let loc = x.pmty_loc in
                        let x = x.pmty_desc in
                        match x with
                        | Pmty_functor (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "functor"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pmty_with (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmty_attributes;
                       (let loc = x.pmty_loc in
                        let x = x.pmty_desc in
                        match x with
                        | Pmty_with (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "with"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pmty_typeof (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmty_attributes;
                   (let loc = x.pmty_loc in
                    let x = x.pmty_desc in
                    match x with
                    | Pmty_typeof x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "typeof"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmty_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmty_attributes;
                   (let loc = x.pmty_loc in
                    let x = x.pmty_desc in
                    match x with
                    | Pmty_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pmty_alias (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pmty_attributes;
                   (let loc = x.pmty_loc in
                    let x = x.pmty_desc in
                    match x with
                    | Pmty_alias x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "alias"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let module_type_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pmtd_loc in
             let k = f1 ctx loc x.pmtd_attributes k in
             let x = { x with pmtd_attributes = [] } in
             let k = f2 ctx loc x k in k)
let module_type_declaration ~name:(T name)  =
  ((fun ~type_:(T type_) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pmtd_attributes;
                       (let k =
                          name ctx (x.pmtd_name).loc (x.pmtd_name).txt k in
                        let k = type_ ctx loc x.pmtd_type k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let immutable =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Immutable -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Immutable")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let mutable_ =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Mutable -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Mutable")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let pof_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pof_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pof_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pof_loc in
             let k = f1 ctx loc x.pof_attributes k in
             let x = { x with pof_attributes = [] } in
             let k = f2 ctx loc x k in k)
let otag (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pof_attributes;
                       (let loc = x.pof_loc in
                        let x = x.pof_desc in
                        match x with
                        | Otag (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "Otag"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let oinherit (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.pof_attributes;
                   (let loc = x.pof_loc in
                    let x = x.pof_desc in
                    match x with
                    | Oinherit x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "Oinherit"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let open_infos_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.popen_loc in
             let k = f1 ctx loc x.popen_attributes k in
             let x = { x with popen_attributes = [] } in
             let k = f2 ctx loc x k in k)
let open_infos ~expr:(T expr)  =
  ((fun ~override:(T override) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.popen_attributes;
                       (let k = expr ctx loc x.popen_expr k in
                        let k = override ctx loc x.popen_override k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let override =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Override -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Override")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let fresh =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Fresh -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Fresh")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let ppat_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ppat_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let ppat_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ppat_loc in
             let k = f1 ctx loc x.ppat_attributes k in
             let x = { x with ppat_attributes = [] } in
             let k = f2 ctx loc x k in k)
let ppat_any =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_any -> (ctx.matched <- (ctx.matched + 1); k)
                    | _ -> fail loc "any"))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let ppat_var (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_var x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "var"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_alias (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_alias (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx x1.loc x1.txt k in k))
                        | _ -> fail loc "alias"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_constant (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_constant x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "constant"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_interval (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_interval (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "interval"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_tuple (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_tuple x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "tuple"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_construct (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_construct (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "construct"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_variant (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_variant (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "variant"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_record (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_record (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "record"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_array (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_array x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "array"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_or (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_or (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "or"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_constraint (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_constraint (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx loc x0 k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "constraint"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let ppat_type (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_type x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "type"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_lazy (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_lazy x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "lazy"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_unpack (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_unpack x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx x0.loc x0.txt k in k))
                    | _ -> fail loc "unpack"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_exception (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_exception x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "exception"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_extension (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ppat_attributes;
                   (let loc = x.ppat_loc in
                    let x = x.ppat_desc in
                    match x with
                    | Ppat_extension x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "extension"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat_open (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.ppat_attributes;
                       (let loc = x.ppat_loc in
                        let x = x.ppat_desc in
                        match x with
                        | Ppat_open (x0, x1) ->
                            (ctx.matched <- (ctx.matched + 1);
                             (let k = f0 ctx x0.loc x0.txt k in
                              let k = f1 ctx loc x1 k in k))
                        | _ -> fail loc "open"))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pstr (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | PStr x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "PStr")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | PSig x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "PSig")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptyp (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | PTyp x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "PTyp")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ppat (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | PPat (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "PPat")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let position ~fname:(T fname)  =
  ((fun ~lnum:(T lnum) ->
      ((fun ~bol:(T bol) ->
          ((fun ~cnum:(T cnum) ->
              T
                (fun ctx ->
                   ((fun loc ->
                       ((fun x ->
                           ((fun k ->
                               let k = fname ctx loc x.pos_fname k in
                               let k = lnum ctx loc x.pos_lnum k in
                               let k = bol ctx loc x.pos_bol k in
                               let k = cnum ctx loc x.pos_cnum k in k)
                           [@ppxlib.migration.stop_taking ]))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ])))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let private_ =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Private -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Private")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let public =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Public -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Public")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let nonrecursive =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Nonrecursive -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Nonrecursive")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let recursive =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Recursive -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Recursive")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let prf_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.prf_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let prf_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.prf_loc in
             let k = f1 ctx loc x.prf_attributes k in
             let x = { x with prf_attributes = [] } in
             let k = f2 ctx loc x k in k)
let rtag (T f0) =
  ((fun (T f1) ->
      ((fun (T f2) ->
          T
            (fun ctx ->
               ((fun _loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.prf_attributes;
                           (let loc = x.prf_loc in
                            let x = x.prf_desc in
                            match x with
                            | Rtag (x0, x1, x2) ->
                                (ctx.matched <- (ctx.matched + 1);
                                 (let k = f0 ctx x0.loc x0.txt k in
                                  let k = f1 ctx loc x1 k in
                                  let k = f2 ctx loc x2 k in k))
                            | _ -> fail loc "Rtag"))
                       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                         ][@ppxlib.migration.stop_taking
                                                            ][@ppxlib.migration.stop_taking
                                                               ]))
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ])))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let rinherit (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.prf_attributes;
                   (let loc = x.prf_loc in
                    let x = x.prf_desc in
                    match x with
                    | Rinherit x0 ->
                        (ctx.matched <- (ctx.matched + 1);
                         (let k = f0 ctx loc x0 k in k))
                    | _ -> fail loc "Rinherit"))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.psig_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let psig_value (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_value x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "value")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_type (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.psig_loc in
                       let x = x.psig_desc in
                       match x with
                       | Psig_type (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "type")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let psig_typesubst (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_typesubst x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "typesubst")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_typext (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_typext x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "typext")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_exception (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_exception x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "exception")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_module (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_module x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "module")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_modsubst (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_modsubst x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "modsubst")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_recmodule (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_recmodule x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "recmodule")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_modtype (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_modtype x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "modtype")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_modtypesubst (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_modtypesubst x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "modtypesubst")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_open (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_open x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "open")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_include (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_include x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "include")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_class (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_class x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "class")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_class_type (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_class_type x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "class_type")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_attribute (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.psig_loc in
                   let x = x.psig_desc in
                   match x with
                   | Psig_attribute x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "attribute")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let psig_extension (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.psig_loc in
                       let x = x.psig_desc in
                       match x with
                       | Psig_extension (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "extension")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pstr_loc (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pstr_loc in
             let k = f1 ctx loc loc k in let k = f2 ctx loc x k in k)
let pstr_eval (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.pstr_loc in
                       let x = x.pstr_desc in
                       match x with
                       | Pstr_eval (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "eval")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pstr_value (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.pstr_loc in
                       let x = x.pstr_desc in
                       match x with
                       | Pstr_value (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "value")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pstr_primitive (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_primitive x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "primitive")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_type (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.pstr_loc in
                       let x = x.pstr_desc in
                       match x with
                       | Pstr_type (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "type")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pstr_typext (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_typext x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "typext")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_exception (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_exception x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "exception")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_module (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_module x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "module")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_recmodule (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_recmodule x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "recmodule")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_modtype (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_modtype x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "modtype")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_open (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_open x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "open")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_class (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_class x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "class")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_class_type (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_class_type x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "class_type")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_include (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_include x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "include")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_attribute (T f0) =
  T
    (fun ctx ->
       ((fun _loc ->
           ((fun x ->
               ((fun k ->
                   let loc = x.pstr_loc in
                   let x = x.pstr_desc in
                   match x with
                   | Pstr_attribute x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "attribute")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let pstr_extension (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun _loc ->
               ((fun x ->
                   ((fun k ->
                       let loc = x.pstr_loc in
                       let x = x.pstr_desc in
                       match x with
                       | Pstr_extension (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx loc x0 k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "extension")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let toplevel_directive ~name:(T name)  =
  ((fun ~arg:(T arg) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       let k = name ctx (x.pdir_name).loc (x.pdir_name).txt k in
                       let k = arg ctx loc x.pdir_arg k in k)
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let ptop_def (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptop_def x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "def")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptop_dir (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptop_dir x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "dir")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let type_declaration_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ptype_loc in
             let k = f1 ctx loc x.ptype_attributes k in
             let x = { x with ptype_attributes = [] } in
             let k = f2 ctx loc x k in k)
let type_declaration ~name:(T name)  =
  ((fun ~params:(T params) ->
      ((fun ~cstrs:(T cstrs) ->
          ((fun ~kind:(T kind) ->
              ((fun ~private_:(T private_) ->
                  ((fun ~manifest:(T manifest) ->
                      T
                        (fun ctx ->
                           ((fun loc ->
                               ((fun x ->
                                   ((fun k ->
                                       Common.assert_no_attributes
                                         x.ptype_attributes;
                                       (let k =
                                          name ctx (x.ptype_name).loc
                                            (x.ptype_name).txt k in
                                        let k =
                                          params ctx loc x.ptype_params k in
                                        let k = cstrs ctx loc x.ptype_cstrs k in
                                        let k = kind ctx loc x.ptype_kind k in
                                        let k =
                                          private_ ctx loc x.ptype_private k in
                                        let k =
                                          manifest ctx loc x.ptype_manifest k in
                                        k))
                                   [@ppxlib.migration.stop_taking ]))
                               [@ppxlib.migration.stop_taking ]))
                           [@ppxlib.migration.stop_taking ])))
                  [@ppxlib.migration.stop_taking ]))
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let type_exception_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ptyexn_loc in
             let k = f1 ctx loc x.ptyexn_attributes k in
             let x = { x with ptyexn_attributes = [] } in
             let k = f2 ctx loc x k in k)
let type_exception ~constructor:(T constructor)  =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   Common.assert_no_attributes x.ptyexn_attributes;
                   (let k = constructor ctx loc x.ptyexn_constructor k in k))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let type_extension_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.ptyext_loc in
             let k = f1 ctx loc x.ptyext_attributes k in
             let x = { x with ptyext_attributes = [] } in
             let k = f2 ctx loc x k in k)
let type_extension ~path:(T path)  =
  ((fun ~params:(T params) ->
      ((fun ~constructors:(T constructors) ->
          ((fun ~private_:(T private_) ->
              T
                (fun ctx ->
                   ((fun loc ->
                       ((fun x ->
                           ((fun k ->
                               Common.assert_no_attributes
                                 x.ptyext_attributes;
                               (let k =
                                  path ctx (x.ptyext_path).loc
                                    (x.ptyext_path).txt k in
                                let k = params ctx loc x.ptyext_params k in
                                let k =
                                  constructors ctx loc x.ptyext_constructors
                                    k in
                                let k = private_ ctx loc x.ptyext_private k in
                                k))
                           [@ppxlib.migration.stop_taking ]))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ])))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let ptype_abstract =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptype_abstract -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "abstract")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let ptype_variant (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptype_variant x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "variant")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptype_record (T f0) =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptype_record x0 ->
                       (ctx.matched <- (ctx.matched + 1);
                        (let k = f0 ctx loc x0 k in k))
                   | _ -> fail loc "record")
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
let ptype_open =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Ptype_open -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "open")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let value_binding_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pvb_loc in
             let k = f1 ctx loc x.pvb_attributes k in
             let x = { x with pvb_attributes = [] } in
             let k = f2 ctx loc x k in k)
let value_binding ~pat:(T pat)  =
  ((fun ~expr:(T expr) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       Common.assert_no_attributes x.pvb_attributes;
                       (let k = pat ctx loc x.pvb_pat k in
                        let k = expr ctx loc x.pvb_expr k in k))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ])
let value_description_attributes (T f1) (T f2) =
  T
    (fun ctx ->
       fun _loc ->
         fun x ->
           fun k ->
             let loc = x.pval_loc in
             let k = f1 ctx loc x.pval_attributes k in
             let x = { x with pval_attributes = [] } in
             let k = f2 ctx loc x k in k)
let value_description ~name:(T name)  =
  ((fun ~type_:(T type_) ->
      ((fun ~prim:(T prim) ->
          T
            (fun ctx ->
               ((fun loc ->
                   ((fun x ->
                       ((fun k ->
                           Common.assert_no_attributes x.pval_attributes;
                           (let k =
                              name ctx (x.pval_name).loc (x.pval_name).txt k in
                            let k = type_ ctx loc x.pval_type k in
                            let k = prim ctx loc x.pval_prim k in k))
                       [@ppxlib.migration.stop_taking ]))
                   [@ppxlib.migration.stop_taking ]))
               [@ppxlib.migration.stop_taking ])))
      [@ppxlib.migration.stop_taking ]))
  [@ppxlib.migration.stop_taking ])
let covariant =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Covariant -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Covariant")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let contravariant =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Contravariant -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Contravariant")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let novariance =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | NoVariance -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "NoVariance")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let virtual_ =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Virtual -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Virtual")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let concrete =
  T
    (fun ctx ->
       ((fun loc ->
           ((fun x ->
               ((fun k ->
                   match x with
                   | Concrete -> (ctx.matched <- (ctx.matched + 1); k)
                   | _ -> fail loc "Concrete")
               [@ppxlib.migration.stop_taking ]))
           [@ppxlib.migration.stop_taking ]))
       [@ppxlib.migration.stop_taking ]))
let pwith_type (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_type (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "type")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pwith_module (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_module (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx x1.loc x1.txt k in k))
                       | _ -> fail loc "module")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pwith_modtype (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_modtype (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "modtype")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pwith_modtypesubst (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_modtypesubst (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "modtypesubst")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pwith_typesubst (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_typesubst (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx loc x1 k in k))
                       | _ -> fail loc "typesubst")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
let pwith_modsubst (T f0) =
  ((fun (T f1) ->
      T
        (fun ctx ->
           ((fun loc ->
               ((fun x ->
                   ((fun k ->
                       match x with
                       | Pwith_modsubst (x0, x1) ->
                           (ctx.matched <- (ctx.matched + 1);
                            (let k = f0 ctx x0.loc x0.txt k in
                             let k = f1 ctx x1.loc x1.txt k in k))
                       | _ -> fail loc "modsubst")
                   [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                     ][@ppxlib.migration.stop_taking
                                                        ]))
               [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                 ][@ppxlib.migration.stop_taking
                                                    ]))
           [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
           [@ppxlib.migration.stop_taking ])))
  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
