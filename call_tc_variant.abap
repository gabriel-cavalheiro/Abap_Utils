function rs_hdsys_call_tc_variant.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(TCODE) LIKE  SHDFV-TCODE
*"     VALUE(VTCODE) LIKE  SHDFV-TCODE OPTIONAL
*"     VALUE(VARIANT) LIKE  SHDFV-TCVARIANT OPTIONAL
*"     VALUE(I_FLAG_CLIENT_INDEPENDENT) TYPE  AS4FLAG OPTIONAL
*"     VALUE(CALL_MODE) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(AUTHORITY_CHECK) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(VARIANT_CHECK) TYPE  AS4FLAG DEFAULT 'X'
*"     VALUE(NO_EXPORT) TYPE  AS4FLAG OPTIONAL
*"     VALUE(I_FLAG_SKIP_FIRST_SCREEN) TYPE  AS4FLAG OPTIONAL
*"     VALUE(RESET_AFTER_CALL_TC) TYPE  AS4FLAG OPTIONAL
*"  EXCEPTIONS
*"      NO_VARIANT
*"      NO_AUTHORITY_TCODE
*"----------------------------------------------------------------------

  data: l_subrc like sy-subrc.
  data: l_flag_variant_fields_set,
        l_variant_before like shdtvciu-tcvariant.
  data: l_shdfv    like field_vals      occurs 0 with header line,
        l_shdfvgui like fieldv_gui      occurs 0 with header line,
        l_screen_variants
                   like screen_variants occurs 0 with header line,
        l_shdguixt like shdguixt        occurs 0 with header line.
  data: l_curr_tcode       type sytcode,
        l_curr_param_tcode type sytcode,
        l_curr_tcvariant   type tcvariant,
        l_curr_flag_client_independent,
        l_curr_flag_standard_variant.
  data: l_rc type sysubrc.

* init ----------------------------------------------------------------
  free memory id: c_mem_id_tcvariant,
                  c_mem_id_variant_name.

* decide for what purpose the function is called ----------------------
* (a) to return to a already running variant transaction
*     note: in this case no checks are necessary since everything was
*     already checked before the first start
* (b) to start a (new) variant transaction
* get current variant (if any)
  call function 'RS_HDSYS_GET_TCODE'
       importing
         tcode                   = l_curr_tcode
         param_tcode             = l_curr_param_tcode
         variant                 = l_curr_tcvariant
         flag_client_independent = l_curr_flag_client_independent
         flag_standard_variant   = l_curr_flag_standard_variant
         rc                      = l_rc.
  if l_rc = 0.           "variant transaction active
    if  l_curr_tcode                   = tcode
    and l_curr_tcvariant               = variant
    and l_curr_flag_client_independent = i_flag_client_independent.
*     restart current variant transaction
      perform execute_transaction
              using l_curr_param_tcode
                    call_mode
                    i_flag_skip_first_screen.
      exit.
    elseif l_rc = 2.     "variant active but no variant transaction
      if  l_curr_tcode                   = tcode
      and l_curr_tcvariant               = variant
      and l_curr_flag_client_independent = i_flag_client_independent
      and l_curr_flag_standard_variant   = c_true.
*       restart transaction (standard variant set in SAPSHDTV)
        perform execute_transaction
                using l_curr_tcode
                      call_mode
                      i_flag_skip_first_screen.
        exit.
      endif.
    endif.
  endif.

* Authority for transaction -------------------------------------------
  if authority_check = c_true.
    perform check_authority_tcode
            using    tcode
            changing l_subrc.
    if l_subrc >< 0.
      message e408 with tcode raising no_authority_tcode.
    endif.
  endif.

* Variant exists? -----------------------------------------------------
  if variant ne space and variant_check = c_true.
    if i_flag_client_independent = c_false.
      select single * from shdtvu into corresponding fields of shdfv
             where  tcvariant = variant.
    else.
      select single * from shdtvciu
                      into corresponding fields of shdtvci
             where   tcvariant = variant.
    endif.
    if sy-subrc >< 0.
*     Variant must exist
      message e402 with variant raising no_variant.
    endif.
  endif.

* within variant maintenance only (<--> no_export = true): ------------
* export name of variant and transaction for transaction SHDS
  if no_export = c_true.
    global-tcode     = tcode.
    global-tcvariant = variant.
    global-flag_client_independent = i_flag_client_independent.
    export global to memory id c_mem_id_variant_name.
  endif.

* call transaction with variant according to call_modus ---------------
* indicate transaction variant (used in %_CTRL_OUTPUT)
  if not variant is initial.
    extd_variant-vtcode                  = vtcode.
    extd_variant-tcode                   = tcode.
    extd_variant-variant                 = variant.
    extd_variant-flag_client_independent = i_flag_client_independent.
    set parameter id c_par_id_tv_name field extd_variant.
  endif.
* process transaction according to call mode
* (a) call transaction (--> reset memories if required)
  if call_mode = c_true.
*   prepare for reset
    if reset_after_call_tc = c_true.
      perform prepare_for_reset
              tables   l_shdfv
                       l_shdfvgui
                       l_screen_variants
                       l_shdguixt
              changing l_flag_variant_fields_set
                       l_variant_before.
    endif.
*   export tcode to indicate new tcode in %_CTRL_OUTPUT
    export g_tcode from tcode
           to memory id c_mem_id_tcode.
*   call transaction
    if i_flag_skip_first_screen = c_true.
      call transaction tcode and skip first screen.
    else.
      call transaction tcode.
    endif.
*   reset variant
    if reset_after_call_tc = c_true.
      perform reset_variant
              tables l_shdfv
                     l_shdfvgui
                     l_screen_variants
                     l_shdguixt
              using  l_flag_variant_fields_set
                     l_variant_before
                     variant
                     i_flag_client_independent.
    endif.
* (b) leave to transaction (all ABAP-memories initialized!)
  else.
    if i_flag_skip_first_screen = c_true.
      leave to transaction tcode and skip first screen.
    else.
      leave to transaction tcode.
    endif.
  endif.

endfunction.


**** execute_transaction **********************************************
form execute_transaction
     using p_curr_param_tcode       type sytcode
           p_call_mode              type as4flag
           p_flag_skip_first_screen type as4flag.
  if p_call_mode = c_true.
*   call transaction
    if p_flag_skip_first_screen = c_true.
      call transaction p_curr_param_tcode and skip first screen.
    else.
      call transaction p_curr_param_tcode.
    endif.
  else.
*   leave to transaction
    if p_flag_skip_first_screen = c_true.
      leave to transaction p_curr_param_tcode and skip first screen.
    else.
      leave to transaction p_curr_param_tcode.
    endif.
  endif.
endform.

**** prepare_for_reset ************************************************
form prepare_for_reset
     tables   p_shdfv           structure field_vals
              p_shdfvgui        structure fieldv_gui
              p_screen_variants structure screen_variants
              p_shdguixt        structure shdguixt
     changing p_flag_variant_fields_set type as4flag
              p_variant_before          like shdfv-tcvariant.
* NOTE: same form with same coding in program SAPMSHD0!
* init
  clear p_variant_before.
* check if already variant active
  import flag_set_variant_fields to p_flag_variant_fields_set
         from memory id c_mem_id_set_tv.
  if p_flag_variant_fields_set = c_true.
*   save current variant values
    p_shdfv[]           = i_shdfv[].
    p_shdfvgui[]        = i_shdfvgui[].
    p_screen_variants[] = screen_variants[].
    p_shdguixt[]        = i_shdguixt[].
*   transaction variant or screen variant
    call 'DY_GET_TX_VARIANT'
         id 'VARIANT' field p_variant_before.
    if sy-subrc >< 0.
      clear p_variant_before.
    endif.
  endif.
endform.

**** reset_variant ****************************************************
form reset_variant
     tables p_shdfv           structure field_vals
            p_shdfvgui        structure fieldv_gui
            p_screen_variants structure screen_variants
            p_shdguixt        structure shdguixt
     using  p_flag_variant_fields_set type as4flag
            p_variant_before          like shdtvciu-tcvariant
            p_variant                 like shdtvciu-tcvariant
            p_flag_client_independent type as4flag.
* NOTE: same form with same coding in program SAPMSHD0!
* (a) variant fields set before
  if p_flag_variant_fields_set = c_true.
*   reset fields of variant before
    i_shdfv[]         = p_shdfv[].
    i_shdfvgui[]      = p_shdfvgui[].
    screen_variants[] = p_screen_variants[].
    i_shdguixt[]      = p_shdguixt[].
*   tell c-kernel no variant, if before only screen variant
    if p_variant_before is initial.
      call 'DY_SET_TX_VARIANT'
           id 'VARIANT'            field space
           id 'CLIENT_INDEPENDENT' field space.
    endif.
* (b) no variant fields set before
  else.
*   reset ABAP memories
    export flag_set_variant_fields from space
           to memory id c_mem_id_set_tv.
    free memory id c_mem_id_tcvariant.
*   tell c-kernel no variant
    call 'DY_SET_TX_VARIANT'
         id 'VARIANT'            field space
         id 'CLIENT_INDEPENDENT' field space.
  endif.
endform.
