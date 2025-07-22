*-----------------------------------------------------------------------*
* Empresa..: Global (válido para todas as empresas)
* Programa.: ZGLRFI0001_EXTRAT_BANC_AUDIT
* Tipo.....: Report
* Módulo...: FI
* Transação:
* Descrição:
* Autor....: Gabriel Cavalheiro - TGABRIELHCMR
* Data.....:
*
*                           [HISTÓRICO]
* ========== ================ ============== ===========================
* Data       Autor            Request        Descrição
* ========== ================ ============== ===========================
*            TGABRIELHCMR          Desenvolvimento Inicial
* ========== ================ ============== ===========================
*----------------------------------------------------------------------*

REPORT zglrfi0001_extrat_banc_audit.

*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: febko.

*----------------------------------------------------------------------*
* Types Globais                                                        *
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF y_alv,
    bukrs   TYPE  febko-bukrs,
    hkont   TYPE  febko-hkont,
    hbkid   TYPE  febko-hbkid,
    hktid   TYPE  febko-hktid,
    azdat   TYPE  febko-azdat,
    esnum   TYPE  febep-esnum,
    valut   TYPE  febep-valut,
    bvdat   TYPE  febep-bvdat,
    sgtxt   TYPE  febep-sgtxt,
    butxt   TYPE  febep-butxt,
    kwbtr   TYPE  febep-kwbtr,
    aznum   TYPE  febko-aznum,
    waers   TYPE  febko-waers,
    kukey   TYPE  febko-kukey,
    ssbtr   TYPE  febko-ssbtr,
    vgext   TYPE  febep-vgext,
    esbtr   TYPE  febko-esbtr,
    esvoz   TYPE  febko-esvoz,
    ssvoz   TYPE  febko-ssvoz,
    epvoz   TYPE  febep-epvoz,
    color   TYPE  lvc_t_scol,
    z_ordem TYPE  n LENGTH 3,
  END OF y_alv.

*----------------------------------------------------------------------*
* Classes e Objetos Globais                                            *
*----------------------------------------------------------------------*
DATA: v_o_alv       TYPE REF TO cl_salv_table,
      v_o_functions TYPE REF TO cl_salv_functions,
      v_o_column    TYPE REF TO cl_salv_column_table.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*

DATA: t_alv      TYPE TABLE OF y_alv,
      t_grup_aux TYPE TABLE OF y_alv,
      t_dados    TYPE TABLE OF zcdsfi_tf_extrat_bancario.

*----------------------------------------------------------------------*
* Váriaveis Globais                                                    *
*----------------------------------------------------------------------*
DATA: v_descr_conta TYPE char50.

*----------------------------------------------------------------------*
* Workareas Globais                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Constantes Globais                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Ranges Globais                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Field-Symbols Globais                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK mainblk WITH FRAME TITLE TEXT-001. " Bloco principal

  PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF BLOCK blk_razao WITH FRAME.
    SELECT-OPTIONS: s_hkont FOR febko-hkont.
  SELECTION-SCREEN END OF BLOCK blk_razao.

  SELECTION-SCREEN BEGIN OF BLOCK blk_banco WITH FRAME.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 16(15) TEXT-002.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: p_hbkid   TYPE febko-hbkid.
      SELECTION-SCREEN COMMENT 42(30) vlabel.
    SELECTION-SCREEN END OF LINE.


    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 18(12) TEXT-003.
      SELECTION-SCREEN POSITION 31.
      PARAMETERS: p_hktid   TYPE febko-hktid.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK blk_banco.

  SELECTION-SCREEN SKIP 1.

  SELECT-OPTIONS: s_azdat FOR febko-azdat.

SELECTION-SCREEN END OF BLOCK mainblk.

*----------------------------------------------------------------------*
* At Selection Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM zf_screen.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_auth.
  PERFORM zf_busca_dados.
  PERFORM zf_monta_alv.

*&---------------------------------------------------------------------*
*&     Form zf_screen
*&---------------------------------------------------------------------*
FORM zf_screen .
  IF p_bukrs IS NOT INITIAL AND p_hbkid IS NOT INITIAL.

    SELECT SINGLE bankn, bkont
    INTO @DATA(w_bank)
          FROM t012k
          WHERE bukrs = @p_bukrs
          AND   hbkid = @p_hbkid.

    SELECT SINGLE text1
    INTO @DATA(v_text1)
          FROM t012t
          WHERE bukrs = @p_bukrs
          AND   hbkid = @p_hbkid
          AND   hktid = @p_hktid
          AND   spras = @sy-langu.

    IF sy-subrc = 0.
      vlabel = |{ w_bank-bankn } { w_bank-bkont } { v_text1 }|.
      v_descr_conta = |{ w_bank-bankn } { w_bank-bkont } { v_text1 }|.
    ELSE.
      vlabel = 'Dados não encontrados'.
    ENDIF.
  ELSE.
    vlabel = ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&     Form zf_busca_dados
*&---------------------------------------------------------------------*
FORM zf_busca_dados.

* Converter Range para WHERE SQL
  IF p_hbkid IS NOT INITIAL.
    DATA(v_where_hbkid) = |HBKID = '{ p_hbkid }'|.
  ENDIF.

  IF p_hktid IS NOT INITIAL.
    DATA(v_where_hktid) = |HKTID = '{ p_hktid }'|.
  ENDIF.

  DATA(v_where_hkont) = cl_shdb_seltab=>combine_seltabs(
        it_named_seltabs = VALUE #(
        ( name = 'HKONT' dref = REF #( s_hkont[] ) ) )
      ).

  DATA(v_where_azdat) = cl_shdb_seltab=>combine_seltabs(
        it_named_seltabs = VALUE #(
        ( name = 'AZDAT' dref = REF #( s_azdat[] ) ) )
      ).

  SELECT *
  FROM zcdsfi_tf_extrat_bancario(
  iv_bukrs       = @p_bukrs,
  iv_hbkid       = @v_where_hbkid,
  iv_hktid       = @v_where_hktid,
  iv_where_hkont = @v_where_hkont,
  iv_where_azdat = @v_where_azdat
  )
  INTO TABLE @t_dados.

  IF t_dados IS NOT INITIAL.

    PERFORM zf_trata_dados_alv USING t_dados
                            CHANGING t_alv.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_auth
*&---------------------------------------------------------------------*
FORM zf_auth.

  CONSTANTS:
    c_actvt TYPE xufield     VALUE 'ACTVT',      " ACTVT
    c_bukrs TYPE xufield     VALUE 'BUKRS',      " BUKRS
    c_03    TYPE xuval       VALUE '03'.         " Act. leitura

  SELECT SINGLE bukrs
  FROM t001
  INTO @DATA(l_bukrs)
  WHERE bukrs EQ @p_bukrs.

  IF sy-subrc IS INITIAL.

    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS'  FIELD l_bukrs
    ID 'ACTVT'  FIELD '03'.

    IF sy-subrc IS NOT INITIAL.
      " Falta autorização para empresa
      MESSAGE s000(zfi47) DISPLAY LIKE sy-abcde+4(1) WITH l_bukrs.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&     Form zf_monta_alv
*&---------------------------------------------------------------------*
FORM zf_monta_alv .

  IF t_alv IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
        IMPORTING r_salv_table = v_o_alv
        CHANGING  t_table      = t_alv ).
      CATCH cx_salv_msg INTO DATA(lx_salv_error).
        MESSAGE lx_salv_error TYPE 'E'.
    ENDTRY.
    IF v_o_alv IS NOT BOUND.
      MESSAGE s001(zfi47) DISPLAY LIKE 'E'.
      RETURN.
    ELSE.

      DATA(lr_aggregations) = v_o_alv->get_aggregations( ).
      v_o_functions = v_o_alv->get_functions( ).
      v_o_functions->set_all( abap_true ).
      v_o_alv->get_columns( )->set_color_column('COLOR').

      v_o_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      v_o_alv->get_display_settings( )->set_list_header( 'Relatório Extrato Bancário' ).
      v_o_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).


      PERFORM zf_monta_header_alv.
      PERFORM zf_set_colunas.

      lr_aggregations->clear( ).

      DATA(lr_groups) = v_o_alv->get_sorts( ) .
      lr_groups->clear( ).

      TRY.
          lr_groups->add_sort(
          columnname = 'HKONT'
          position   = 1
          subtotal   = abap_true
          sequence   = if_salv_c_sort=>group_with_newpage ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.

      TRY.
          lr_aggregations->add_aggregation( columnname = 'AZDAT' ).
        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.

      v_o_alv->display( ).

    ENDIF.
  ELSE.
    MESSAGE s002(zfi47) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&     Form zf_monta_header_alv
*&---------------------------------------------------------------------*
FORM zf_monta_header_alv .

  DATA(lo_header)  = NEW cl_salv_form_layout_grid( ).
  DATA(lo_h_label) = NEW cl_salv_form_label( ).
  DATA(lo_h_flow)  = NEW cl_salv_form_layout_flow( ).

  lo_h_label = lo_header->create_label( row = 1 column = 1 ).
  lo_h_label->set_text( 'Detalhes do Extrato' ).

*   Mapeamento Header ALV
  lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
  lo_h_flow->create_text( text = '       ' ).
  lo_h_flow = lo_header->create_flow( row = 2  column = 2 ).
  lo_h_flow->create_text( text = '        ' ).

  lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
  lo_h_flow->create_text( text = 'Empresa:' ).
  lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
  lo_h_flow->create_text( text = |{ p_bukrs }| ).

  lo_h_flow = lo_header->create_flow( row = 4  column = 1 ).
  lo_h_flow->create_text( text = 'Banco Empresa:' ).
  lo_h_flow = lo_header->create_flow( row = 4  column = 2 ).
  lo_h_flow->create_text( text = |{ p_hbkid }| ).

  lo_h_flow = lo_header->create_flow( row = 5  column = 1 ).
  lo_h_flow->create_text( text = 'Conta:' ).
  lo_h_flow = lo_header->create_flow( row = 5  column = 2 ).
  lo_h_flow->create_text( text = |{ v_descr_conta }| ).

  lo_h_flow = lo_header->create_flow( row = 6  column = 1 ).
  lo_h_flow->create_text( text = 'ID:' ).
  lo_h_flow = lo_header->create_flow( row = 6  column = 2 ).
  lo_h_flow->create_text( text = |{ p_hktid }|  ).

  lo_h_flow = lo_header->create_flow( row = 3  column = 3 ).
  lo_h_flow->create_text( text = 'Nº de Movimentações do Período:' ).
  lo_h_flow = lo_header->create_flow( row = 3  column = 4 ).
  lo_h_flow->create_text( text = |{ lines( t_alv ) }|  ).

  lo_h_flow = lo_header->create_flow( row = 4  column = 3 ).
  lo_h_flow->create_text( text = 'Período do Extrato:' ).
  lo_h_flow = lo_header->create_flow( row = 4  column = 4 ).
  lo_h_flow->create_text( text = |{ s_azdat-low DATE = USER }  á  { s_azdat-high DATE = USER }| ).

  v_o_alv->set_top_of_list( lo_header ).

ENDFORM.
*&---------------------------------------------------------------------*
*&     Form zf_trata_dados_alv
*&---------------------------------------------------------------------*
FORM zf_trata_dados_alv  USING    p_t_dados LIKE t_dados
                         CHANGING p_t_alv   LIKE t_alv.


  SORT p_t_dados BY hkont aznum azdat esnum.

  DATA: l_index TYPE n LENGTH 3 VALUE '001'.

  LOOP AT p_t_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
  GROUP BY ( hkont = <fs_dados>-hkont )
*  aznum = <fs_dados>-aznum
*  azdat = <fs_dados>-azdat )
  ASCENDING INTO DATA(w_grp).

    CLEAR t_grup_aux.

    " Pega todas as linhas do grupo atual
    LOOP AT GROUP w_grp ASSIGNING FIELD-SYMBOL(<fs_group_member>).
      APPEND CORRESPONDING y_alv( <fs_group_member> ) TO t_grup_aux.
    ENDLOOP.

    SORT t_grup_aux BY azdat.

    " CRIA LINHA SALDO INICIAL
    DATA(w_saldo_inicial) = t_grup_aux[ 1 ].

    IF sy-subrc = 0.
      DATA(w_saldo_ini_aux) = w_saldo_inicial.

      CLEAR: w_saldo_ini_aux-ssbtr,
      w_saldo_ini_aux-ssvoz,
      w_saldo_ini_aux-esbtr,
      w_saldo_ini_aux-esvoz,
      w_saldo_ini_aux-esnum,
      w_saldo_ini_aux-valut,
      w_saldo_ini_aux-bvdat,
      w_saldo_ini_aux-sgtxt,
      w_saldo_ini_aux-butxt,
      w_saldo_ini_aux-kwbtr,
      w_saldo_ini_aux-epvoz,
      w_saldo_ini_aux-vgext.

      w_saldo_ini_aux-sgtxt = 'Saldo Inicial'.
      w_saldo_ini_aux-kwbtr = CONV #( w_saldo_inicial-ssbtr ).
      w_saldo_ini_aux-z_ordem = l_index.
      w_saldo_ini_aux-color = VALUE #( ( color-col = 3
                                         color-int = 0
                                         color-inv = 0 ) ).

      APPEND w_saldo_ini_aux TO p_t_alv.

    ENDIF.

    " MOVIMENTAÇÕES
    LOOP AT t_grup_aux INTO DATA(w_mov).
      CLEAR: w_mov-ssbtr, w_mov-esbtr.
      ADD 1 TO l_index.
      w_mov-z_ordem = l_index.

      APPEND w_mov TO p_t_alv.
    ENDLOOP.

    " CRIA LINHA SALDO FINAL
    DATA(w_saldo_final) = t_grup_aux[ lines( t_grup_aux ) ].

    IF sy-subrc = 0.

      ADD 1 TO l_index.

      DATA(w_saldo_fin_aux) = w_saldo_final.

      CLEAR: w_saldo_fin_aux-ssbtr,
      w_saldo_fin_aux-ssvoz,
      w_saldo_fin_aux-esbtr,
      w_saldo_fin_aux-esvoz,
      w_saldo_fin_aux-esnum,
      w_saldo_fin_aux-valut,
      w_saldo_fin_aux-bvdat,
      w_saldo_fin_aux-sgtxt,
      w_saldo_fin_aux-butxt,
      w_saldo_fin_aux-kwbtr,
      w_saldo_fin_aux-epvoz,
      w_saldo_fin_aux-vgext.

      w_saldo_fin_aux-sgtxt = 'Saldo Final'.
      w_saldo_fin_aux-kwbtr = CONV #( w_saldo_final-esbtr ).
      w_saldo_fin_aux-z_ordem = '999'.
      w_saldo_fin_aux-color = VALUE #( ( color-col = 7
                                         color-int = 0
                                         color-inv = 0 ) ).

      APPEND w_saldo_fin_aux TO p_t_alv.

    ENDIF.

    CLEAR l_index.
    l_index = '001'.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&     Form zf_set_colunas
*&---------------------------------------------------------------------*

FORM zf_set_colunas .

  DATA(lr_column) = v_o_alv->get_columns( ).

  lr_column->get_column( 'KWBTR' )->set_output_length('10').

  TRY.
      lr_column->get_column( 'SSBTR' )->set_visible( abap_false ).
      lr_column->get_column( 'ESBTR' )->set_visible( abap_false ).
      lr_column->get_column( 'SSVOZ' )->set_visible( abap_false ).
      lr_column->get_column( 'ESVOZ' )->set_visible( abap_false ).
      lr_column->get_column( 'EPVOZ' )->set_visible( abap_false ).
      lr_column->get_column( 'Z_ORDEM' )->set_visible( abap_false ).
    CATCH cx_salv_not_found INTO DATA(lx_not_found).
  ENDTRY.

ENDFORM.

/////////////////// Table Function


