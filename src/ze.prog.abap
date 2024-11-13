*&---------------------------------------------------------------------*
*& Report ZE
*&---------------------------------------------------------------------*
*& DEV : XFC
*&---------------------------------------------------------------------*
*& Usage Instructions:
*&
*&   1 . You can activate this code by creating a local program in the
*&       QA or PRD environments. Alternatively, you can transport it via
*&       a development request in the DEV environment.
*&   2 . This program supports SE38 programs, SE37 functions, SE24 class
*&       methods, CMOD enhancements, and other custom developments.
*&   3 . Do not modify SAP system standard programs! Avoid performing
*&       actions without understanding what you are doing. Incorrect
*&       actions may be irreversible and could negatively affect the
*&       system's operation.
*&
*& Disclaimer:
*&
*&   1 . Users bear all risks associated with using this program.

REPORT ze.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
PARAMETERS: p_prog RADIOBUTTON GROUP rg1 USER-COMMAND uc1 DEFAULT 'X'.
PARAMETERS: p_func RADIOBUTTON GROUP rg1.
PARAMETERS: p_clas RADIOBUTTON GROUP rg1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t02.
PARAMETERS: prog TYPE rs38m-programm    MODIF ID m1 MATCHCODE OBJECT sedt_programs.
PARAMETERS: func TYPE rs38l-name        MODIF ID m2 MATCHCODE OBJECT sfunc_modules.
PARAMETERS: clss TYPE seoclass-clsname  MODIF ID m3 MATCHCODE OBJECT seo_classes_interfaces.
PARAMETERS: meth TYPE seocpdkey-cpdname MODIF ID m3.
SELECTION-SCREEN END OF BLOCK b2.


CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      initialization.
    CLASS-METHODS:
      at_selection_screen_output.
    CLASS-METHODS:
      on_value_request_for_method.
    CLASS-METHODS:
      start_of_selection.

    CONSTANTS:BEGIN OF mc_modif_ids,
                program  TYPE c LENGTH 2 VALUE 'M1',
                function TYPE c LENGTH 2 VALUE 'M2',
                class    TYPE c LENGTH 2 VALUE 'M3',
              END OF mc_modif_ids.

    CONSTANTS:BEGIN OF mc_texts,
                report              TYPE c LENGTH 50 VALUE 'Report',
                function            TYPE c LENGTH 50 VALUE 'Function',
                class               TYPE c LENGTH 50 VALUE 'Class',
                method              TYPE c LENGTH 50 VALUE 'Method',
                p_program_app_text  TYPE c LENGTH 50 VALUE 'Report Name',
                p_function_app_text TYPE c LENGTH 50 VALUE 'Function Name',
                p_clsname_app_text  TYPE c LENGTH 50 VALUE 'Class Name',
                p_methname_app_text TYPE c LENGTH 50 VALUE 'Method Name',
                t01_text            TYPE c LENGTH 50 VALUE 'Function Selection',
                t02_text            TYPE c LENGTH 50 VALUE 'Condition Selection',
                message             TYPE c LENGTH 50 VALUE 'Message',
                line                TYPE c LENGTH 50 VALUE 'Line',
                word                TYPE c LENGTH 50 VALUE 'Word',
              END OF mc_texts.

  PRIVATE SECTION.
    TYPES:ty_source TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    CLASS-METHODS:
      get_program
        IMPORTING
          ip_prog    TYPE rs38m-programm
        EXPORTING
          ep_program TYPE rs38m-programm
        EXCEPTIONS
          program_not_exist.
    CLASS-METHODS:
      get_prog_from_function
        IMPORTING
          ip_func    TYPE rs38l-name
        EXPORTING
          ep_program TYPE rs38l-include
        EXCEPTIONS
          function_not_exist.
    CLASS-METHODS:
      get_prog_from_class_and_method
        IMPORTING
          ip_clss    TYPE seoclass-clsname
          ip_meth    TYPE seocpdkey-cpdname
        EXPORTING
          ep_program TYPE rs38m-programm
        EXCEPTIONS
          class_not_exist
          method_not_exist
          class_and_method_not_exist.
    CLASS-METHODS:
      call_editor
        IMPORTING
          ip_prog TYPE program.
    CLASS-METHODS:
      get_dynpro_field_value
        IMPORTING
          ip_dyname      TYPE d020s-prog DEFAULT sy-repid
          ip_dynumb      TYPE d020s-dnum DEFAULT sy-dynnr
          ip_field       TYPE dynpread-fieldname
        RETURNING
          VALUE(r_value) TYPE dynpread-fieldvalue.
    CLASS-METHODS:
      format_source
        CHANGING
          ct_source TYPE ty_source.
ENDCLASS.
CLASS lcl_report IMPLEMENTATION.
  METHOD:initialization.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE38'.
    IF sy-subrc <> 0.
      "You are not authorized to use transaction SE38
      MESSAGE e077(s#) WITH 'SE38'.
    ENDIF.

    %_p_prog_%_app_%-text = mc_texts-report.
    %_p_func_%_app_%-text = mc_texts-function.
    %_p_clas_%_app_%-text = mc_texts-class.
    %_prog_%_app_%-text = mc_texts-p_program_app_text.
    %_func_%_app_%-text = mc_texts-p_function_app_text.
    %_clss_%_app_%-text = mc_texts-p_clsname_app_text.
    %_meth_%_app_%-text = mc_texts-p_methname_app_text.
    t01 = mc_texts-t01_text.
    t02 = mc_texts-t02_text.
  ENDMETHOD.
  METHOD:at_selection_screen_output.
    LOOP AT SCREEN.
      CASE abap_true.
        WHEN p_prog.
          CASE screen-group1.
            WHEN mc_modif_ids-program.
              screen-required = 2.
            WHEN mc_modif_ids-function OR mc_modif_ids-class.
              screen-active = 0.
          ENDCASE.
        WHEN p_func.
          CASE screen-group1.
            WHEN mc_modif_ids-program OR mc_modif_ids-class.
              screen-active = 0.
            WHEN mc_modif_ids-function.
              screen-required = 2.
          ENDCASE.
        WHEN p_clas.
          CASE screen-group1.
            WHEN mc_modif_ids-program OR mc_modif_ids-function.
              screen-active = 0.
            WHEN mc_modif_ids-class.
              screen-required = 2.
          ENDCASE.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD:on_value_request_for_method.
    TYPES: BEGIN OF ty_method,
             method TYPE seocpdkey-cpdname,
           END OF ty_method.
    DATA:
      lt_result  TYPE seop_methods_w_include,
      lt_methods TYPE TABLE OF ty_method.

    clss = to_upper( get_dynpro_field_value( ip_field = 'CLSS' ) ).

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = clss
      RECEIVING
        result             = lt_result
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2
    ).
    IF sy-subrc NE 0.
      "Class & does not exist
      MESSAGE s003(oo) WITH mc_texts-class clss DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lt_methods = CORRESPONDING #( lt_result MAPPING method = cpdkey-cpdname ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'METHOD'
        value_org   = 'S'
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = 'METH'
      TABLES
        value_tab   = lt_methods
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.
  METHOD:start_of_selection.
    DATA:lv_program TYPE program.

    DEFINE show_message.
      MESSAGE ID sy-msgid
            TYPE 'S'
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         DISPLAY LIKE 'E'.
    END-OF-DEFINITION.

    CASE abap_true.
      WHEN p_prog.
        CHECK prog IS NOT INITIAL.
        get_program(
          EXPORTING
            ip_prog           = prog
          IMPORTING
            ep_program        = lv_program
          EXCEPTIONS
            program_not_exist = 1
            OTHERS            = 2
        ).
        IF sy-subrc <> 0.
          show_message.
          RETURN.
        ENDIF.
      WHEN p_func.
        CHECK func IS NOT INITIAL.
        get_prog_from_function(
          EXPORTING
            ip_func            = func
          IMPORTING
            ep_program         = lv_program
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2
        ).
        IF sy-subrc <> 0.
          show_message.
          RETURN.
        ENDIF.
      WHEN p_clas.
        CHECK clss IS NOT INITIAL AND meth IS NOT INITIAL.
        get_prog_from_class_and_method(
          EXPORTING
            ip_clss                    = clss
            ip_meth                    = meth
          IMPORTING
            ep_program                 = lv_program
          EXCEPTIONS
            class_not_exist            = 1
            method_not_exist           = 2
            class_and_method_not_exist = 3
            OTHERS                     = 4
        ).
        IF sy-subrc <> 0.
          show_message.
          RETURN.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    call_editor( ip_prog = lv_program ).
  ENDMETHOD.
  METHOD:get_program.
    SELECT SINGLE COUNT(*) FROM trdir WHERE name = ip_prog.
    IF sy-subrc NE 0.
      "Program & does not exist
      MESSAGE e017(ds) WITH ip_prog RAISING program_not_exist.
    ENDIF.
    ep_program = ip_prog.
  ENDMETHOD.
  METHOD:get_prog_from_function.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname = ip_func
      IMPORTING
        include  = ep_program
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc NE 0.
      "Function module & does not exist
      MESSAGE e110(fl) WITH ip_func RAISING function_not_exist.
    ENDIF.
  ENDMETHOD.
  METHOD:get_prog_from_class_and_method.
    SELECT SINGLE COUNT(*) FROM tadir WHERE obj_name = ip_clss.
    IF sy-subrc NE 0.
      "Class & does not exist
      MESSAGE e003(oo) WITH mc_texts-class clss.
    ENDIF.
    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey                = VALUE #( clsname = ip_clss cpdname = ip_meth )
      RECEIVING
        result                = ep_program
      EXCEPTIONS
        class_not_existing    = 1
        method_not_existing   = 2
        OTHERS                = 3
    ).
    CASE sy-subrc.
      WHEN  1.
        "Class & does not exist
        MESSAGE e003(oo) WITH mc_texts-class ip_clss RAISING class_not_exist.
      WHEN  2.
        "Method & does not exist
        MESSAGE e003(oo) WITH mc_texts-method ip_meth RAISING method_not_exist.
      WHEN  3.
        "Class Method does not exist
        MESSAGE e003(oo) WITH mc_texts-class mc_texts-method RAISING class_and_method_not_exist.
    ENDCASE.
  ENDMETHOD.
  METHOD:call_editor.
    DATA:
      lt_src TYPE ty_source,
      lv_msg TYPE string,
      lv_lin TYPE i,
      lv_wrd TYPE string,
      lv_dir TYPE trdir.

    READ REPORT ip_prog INTO lt_src.

    format_source( CHANGING ct_source = lt_src ).

    EDITOR-CALL FOR lt_src.
    CASE sy-subrc.
      WHEN 2."no change
        "Object is already active
        MESSAGE s827(eu) DISPLAY LIKE 'E'.
        RETURN.
      WHEN 4."cancel
        "Editing canceled
        MESSAGE s202(eu) DISPLAY LIKE 'E'.
        RETURN.
    ENDCASE.

    format_source( CHANGING ct_source = lt_src ).

    SYNTAX-CHECK FOR lt_src MESSAGE lv_msg LINE lv_lin WORD lv_wrd PROGRAM ip_prog.
    IF sy-subrc <> 0.
      FORMAT INTENSIFIED COLOR COL_GROUP.
      WRITE: |{ mc_texts-message WIDTH = 55 }|,
             |{ mc_texts-line    WIDTH = 10 }|,
             |{ mc_texts-word    WIDTH = 30 }|.
      FORMAT INTENSIFIED OFF.
      DO.
        IF strlen( lv_msg ) GT 50.
          WRITE: |{ substring( val = lv_msg len = 50 ) WIDTH = 55 }|,
                 |{ lv_lin                             WIDTH = 10 }|,
                 |{ lv_wrd                             WIDTH = 30 }|.
        ELSE.
          WRITE: |{ lv_msg     WIDTH = 55 }|,
                 |{ lv_lin     WIDTH = 10 }|,
                 |{ lv_wrd     WIDTH = 30 }|.
          EXIT.
        ENDIF.
        lv_msg = substring( val = lv_msg off = 50 ).
      ENDDO.
    ENDIF.
    INSERT REPORT ip_prog FROM lt_src.
    "Object saved and activated
    MESSAGE s831(eu).
  ENDMETHOD.
  METHOD:get_dynpro_field_value.
    DATA:lt_dynpfields TYPE TABLE OF dynpread.

    lt_dynpfields = VALUE #( ( fieldname = ip_field ) ).

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfields
      EXCEPTIONS
        OTHERS     = 1.

    CHECK sy-subrc EQ 0.

    r_value = lt_dynpfields[ 1 ]-fieldvalue.
  ENDMETHOD.
  METHOD:format_source.
    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo = abap_false
      TABLES
        ntext  = ct_source
        otext  = ct_source
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  lcl_report=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_report=>at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR meth.
  lcl_report=>on_value_request_for_method( ).

START-OF-SELECTION.
  lcl_report=>start_of_selection( ).
