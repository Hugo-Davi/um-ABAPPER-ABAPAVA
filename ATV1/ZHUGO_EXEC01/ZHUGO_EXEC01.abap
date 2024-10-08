REPORT zhugo_exec01.

DATA: ebeln TYPE ekko-ebeln,
      ebelp TYPE ekpo-ebelp,
      bukrs TYPE ekko-bukrs.

DATA: datenow  LIKE zhugot_exec01-datenow,
      time     LIKE zhugot_exec01-time,
      usernome LIKE zhugot_exec01-usernome.

datenow  = sy-datum.
time     = sy-uzeit.
usernome = sy-uname.

DATA: it_final TYPE TABLE OF zhugot_exec01.

* ALV
DATA: wa_layout   TYPE slis_layout_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b1t.
SELECT-OPTIONS: s_ebeln FOR ebeln,
                s_ebelp FOR ebelp,
                s_bukrs FOR bukrs.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
* SELECT
  SELECT  ekko~ebeln, ekko~bukrs, ekpo~ebelp, ekko~bsart, ekko~lifnr, ekko~aedat, ekpo~matnr, ekko~bstyp,
          CASE WHEN hgo~maktx LIKE '%' THEN hgo~maktx ELSE makt~maktx END AS maktx," CHECANDO SE EXISTE OU NÃO DESCRIÇÃO PERSONALIZADA
          @datenow AS datenow, @time AS time, @usernome AS usernome
          FROM ekko
    INNER JOIN ekpo ON ekpo~ebeln EQ ekko~ebeln
    INNER JOIN makt ON makt~matnr EQ ekpo~matnr
     LEFT JOIN zhugot_exec01 AS hgo ON hgo~ebeln EQ ekko~ebeln AND
                                       hgo~bukrs EQ ekko~bukrs AND
                                       hgo~ebelp EQ ekpo~ebelp AND
                                       hgo~bsart EQ ekko~bsart AND
                                       hgo~lifnr EQ ekko~lifnr AND
                                       hgo~aedat EQ ekko~aedat AND
                                       hgo~matnr EQ ekpo~matnr AND
                                       hgo~bstyp EQ ekko~bstyp
         WHERE ekko~ebeln IN @s_ebeln AND
               ekpo~ebelp IN @s_ebelp AND
               ekpo~bukrs IN @s_bukrs
          INTO CORRESPONDING FIELDS OF TABLE @it_final.


  IF sy-subrc IS INITIAL.

*   EXIBINDO ALV
    PERFORM f_fields.
    PERFORM f_alv.

  ELSE.
    MESSAGE: TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

FORM f_alv.
  wa_layout-zebra = 'X'.
  wa_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      is_layout                = wa_layout
      it_fieldcat              = it_fieldcat
    TABLES
      t_outtab                 = it_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE: TEXT-e03 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
FORM f_set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'HUGOEXEC01'.
ENDFORM.
FORM f_user_command USING l_ucomm    LIKE sy-ucomm
                          l_selfield TYPE slis_selfield.

*** BEGIN OF ATUALIZANDO O ALV GRID ( https://answers.sap.com/questions/4385013/problems-in-alv-refresh-after-data-changes.html )
  DATA: gd_repid LIKE sy-repid, "Exists
        ref_grid TYPE REF TO cl_gui_alv_grid.
  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.
  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data .
  ENDIF.
  l_selfield-refresh = abap_true.  " refresh ALV list !!!
*** END OF ATUALIZANDO O ALV GRID

  CASE l_ucomm. " Checando Input do Usuário
    WHEN 'SAVE'.
      "READ TABLE it_final INTO DATA(lwa_final2) INDEX l_selfield-tabindex.
      PERFORM f_save_in_table.
    WHEN '&IC1'.
      IF l_selfield-fieldname EQ 'EBELN'. " Validando o duplo clique
        READ TABLE it_final INTO DATA(lwa_final) INDEX l_selfield-tabindex.
        IF sy-subrc IS INITIAL.
*         Checando BSTYP para verificar se é possíbel achá-lo na ME23N
          CASE lwa_final-bstyp.
            WHEN 'K'.
              MESSAGE: TEXT-e02 TYPE 'S' DISPLAY LIKE 'W'.
            WHEN OTHERS.
              SET PARAMETER ID 'BES' FIELD lwa_final-ebeln.
              CALL TRANSACTION 'ME23N'.
          ENDCASE.

        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
*-------------------------------------------*
*&-----------------------------------------&*
*&------- PROCESSO DE SALVAR OS -----------&*
*&------------- DADOS NO BANCO ------------&*
*&-----------------------------------------&*
*-------------------------------------------*
FORM f_save_in_table.
  SELECT *
    FROM zhugot_exec01 AS h
    INTO @DATA(it_duplicate)
    WHERE h~ebeln IN @s_ebeln AND
          h~ebelp IN @s_ebelp AND
          h~bukrs IN @s_bukrs.
  ENDSELECT.
  IF sy-subrc IS INITIAL.
    DATA l_ans.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-ant
        text_question         = TEXT-m01
        text_button_1         = TEXT-an1
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = TEXT-an2
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
      IMPORTING
        answer                = l_ans
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE: TEXT-e04 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    IF l_ans EQ 001.
      PERFORM f_att_dates.
      MODIFY zhugot_exec01 FROM TABLE it_final.
      MESSAGE: TEXT-m02 TYPE 'S'.
    ELSE.
      MESSAGE: TEXT-m03 TYPE 'E'.
    ENDIF.
  ELSE.
    PERFORM f_att_dates.
    MODIFY zhugot_exec01 FROM TABLE it_final.
    IF sy-subrc IS INITIAL.
      MESSAGE: TEXT-m04 TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_att_dates.
*&-----------------------------------------&*
*&&---------------------------------------&&*
*&&&----- ATUALIZANDO A DATA E HORA -----&&&*
*&&&-----PARA A DO REGISTRO NO BANCO-----&&&*
*&&---------------------------------------&&*
*&-----------------------------------------&*
  FIELD-SYMBOLS <fs> LIKE LINE OF it_final.
  LOOP AT it_final ASSIGNING <fs>.
    <fs>-datenow = sy-datum.
    <fs>-time = sy-uzeit.
  ENDLOOP.
ENDFORM.
FORM f_fields.
  wa_fieldcat-fieldname = 'EBELN'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f01.
  wa_fieldcat-hotspot   = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'BUKRS'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f02.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'EBELP'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f03.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'BSART'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f04.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f05.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'AEDAT'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f06.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f07.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'MAKTX'.
  wa_fieldcat-tabname   = 'IT_FINAL'.
  wa_fieldcat-seltext_m = TEXT-f08.
  wa_fieldcat-edit = 'X'.
  wa_fieldcat-outputlen = 50.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.
