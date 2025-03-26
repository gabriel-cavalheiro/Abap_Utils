*----------------------------------------------------------------------*
***INCLUDE LZGFI_CTRL_TARIFAI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZLOAD_FROM_EXCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE zf_load_from_excel INPUT.

  DATA: l_t_filetable        TYPE filetable,
        l_t_ztbfi_ctrl_tarif TYPE TABLE OF ztbfi_ctrl_tarif,
        l_t_file             TYPE issr_alsmex_tabline.

  DATA: l_w_filetable        LIKE LINE OF l_t_filetable,
        l_w_ztbfi_ctrl_tarif LIKE ztbfi_ctrl_tarif.

  DATA: l_cotacao TYPE char25,
        l_data    TYPE datum,
        l_subrc   TYPE sy-subrc,
        l_tabix   TYPE sy-tabix,
        l_file    TYPE char255,
        l_rc      TYPE i.

  CASE function.

    WHEN 'UPLXLS'.

      REFRESH l_t_filetable.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        CHANGING
          file_table              = l_t_filetable
          rc                      = l_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc = 0.
        READ TABLE l_t_filetable INTO l_w_filetable INDEX 1.
        IF sy-subrc EQ 0.
          l_file  = l_w_filetable-filename.
        ENDIF.
      ENDIF.

      PERFORM zf_gui_upload TABLES l_t_file USING l_file.

      " Apaga o cabeçalho.
      DELETE l_t_file WHERE row = 1.

      LOOP AT l_t_file INTO DATA(l_w_file).

        CASE l_w_file-col.

            "Cod.Banco
          WHEN '0001'.
            l_w_ztbfi_ctrl_tarif-cod_banco = l_w_file-value.

            "Empresa
          WHEN '0002'.
            l_w_ztbfi_ctrl_tarif-bukrs = l_w_file-value.

            "Cod.Tarif
          WHEN '0003'.
            l_w_ztbfi_ctrl_tarif-cod_tarif = l_w_file-value.

            "Descrição Tarifa
          WHEN '0004'.
            TRANSLATE l_w_file-value TO UPPER CASE.
            l_w_ztbfi_ctrl_tarif-desc_tarif = l_w_file-value.

            "Data Inicio
          WHEN '0005'.
            REPLACE ALL OCCURRENCES OF '.' IN l_w_file-value WITH ''.
            REPLACE ALL OCCURRENCES OF '/' IN l_w_file-value WITH ''.
            CONCATENATE
            l_w_file-value+4(4)
            l_w_file-value+2(2)
            l_w_file-value(2)
            INTO l_w_ztbfi_ctrl_tarif-dt_val_ini.

            "Data Fim
          WHEN '0006'.
            REPLACE ALL OCCURRENCES OF '.' IN l_w_file-value WITH ''.
            REPLACE ALL OCCURRENCES OF '/' IN l_w_file-value WITH ''.
            CONCATENATE
            l_w_file-value+4(4)
            l_w_file-value+2(2)
            l_w_file-value(2)
            INTO l_w_ztbfi_ctrl_tarif-dt_val_fim.

            "Descrição Tarifa Votorantim
          WHEN '0007'.
            TRANSLATE l_w_file-value TO UPPER CASE.
            l_w_ztbfi_ctrl_tarif-desc_tarif_vot = l_w_file-value.

            "Montante
          WHEN '0008'.
            REPLACE ',' WITH '.' INTO l_w_file-value.
            l_w_ztbfi_ctrl_tarif-montante     = l_w_file-value.

          WHEN '0009'.
            TRANSLATE l_w_file-value TO UPPER CASE.
            l_w_ztbfi_ctrl_tarif-tip_tarif = l_w_file-value.

            <vim_extract_struc> = l_w_ztbfi_ctrl_tarif.

            PERFORM move_extract_to_view_wa.

            l_subrc = 8.
            l_tabix = l_tabix + 1.
            nextline = nextline + 1.
            PERFORM check_key.
            PERFORM nicht_vorhanden USING l_subrc l_tabix.

        ENDCASE.
      ENDLOOP.
    WHEN OTHERS.
      "
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_GUI_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_FILE  text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM zf_gui_upload TABLES ct_file      TYPE issr_alsmex_tabline
* ---> S4 Migration - 29/11/2023 - JS
*                     USING  cp_localfile TYPE file.
USING  cp_localfile TYPE char255.
*   <--- S4 Migration - 29/11/2023 - JS

  DATA: t_file_excel TYPE STANDARD TABLE OF alsmex_tabline, "#EC NEEDED
        t_files_itab TYPE filetable.                        "#EC NEEDED
  DATA: t_file_aux  TYPE kcde_intern,
        w_file1     LIKE LINE OF t_file_aux,
        w_filename  TYPE string,
        l_localfile TYPE rlgrap-filename,
        w_file2     LIKE LINE OF t_file_excel,
        l_len       TYPE i,
        l_c_aux(3)  TYPE c.

  REFRESH: t_file_aux,
  ct_file,
  t_file_excel.

  IF cp_localfile IS NOT INITIAL.

    l_len = strlen( cp_localfile ).
    l_len = l_len - 3.
    l_c_aux    = cp_localfile+l_len(3).

    TRANSLATE l_c_aux TO UPPER CASE.
    l_localfile = cp_localfile.

    IF l_c_aux = 'CSV'.
      CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT'
        EXPORTING
          i_filename      = l_localfile
          i_separator     = ';'
        TABLES
          e_intern        = t_file_aux
        EXCEPTIONS
          upload_csv      = 1
          upload_filetype = 2
          OTHERS          = 3.

      IF sy-subrc IS INITIAL.
        LOOP AT t_file_aux INTO w_file1.
          w_file2-row    = w_file1-row.
          w_file2-col    = w_file1-col.
          w_file2-value  = w_file1-value.

          APPEND w_file2 TO ct_file.
          CLEAR w_file2.
        ENDLOOP.
      ENDIF.

    ELSE.
      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
        EXPORTING
          filename                = l_localfile
          i_begin_col             = 1
          i_begin_row             = 1
          i_end_col               = 30
          i_end_row               = 65000
        TABLES
          intern                  = t_file_excel
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.

      IF sy-subrc IS INITIAL.
        APPEND LINES OF t_file_excel TO ct_file.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
