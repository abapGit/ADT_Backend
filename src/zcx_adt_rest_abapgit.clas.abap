CLASS zcx_adt_rest_abapgit DEFINITION
  PUBLIC
  INHERITING FROM cx_adt_rest
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .

    CLASS-METHODS raise_with_error
      IMPORTING
        !ix_error       TYPE REF TO cx_root
        !iv_http_status TYPE i OPTIONAL
      RAISING
        zcx_adt_rest_abapgit .
    METHODS constructor
      IMPORTING
        textid          LIKE if_t100_message=>t100key OPTIONAL
        previous        LIKE previous OPTIONAL
        subtype         TYPE sadt_exc_type OPTIONAL
        msgv1           TYPE symsgv OPTIONAL
        msgv2           TYPE symsgv OPTIONAL
        msgv3           TYPE symsgv OPTIONAL
        msgv4           TYPE symsgv OPTIONAL
        !properties     TYPE REF TO if_adt_exception_properties OPTIONAL
        !iv_http_status TYPE i OPTIONAL.

    METHODS get_http_status
        REDEFINITION .
    METHODS get_namespace
        REDEFINITION .
    METHODS get_type
        REDEFINITION .
  PRIVATE SECTION.

    DATA mv_http_status TYPE i.

    CLASS-METHODS get_message_var
      IMPORTING
        ix_error          TYPE REF TO cx_root
        iv_attribute      TYPE csequence
      RETURNING
        VALUE(rv_msg_var) TYPE symsgv.

ENDCLASS.



CLASS zcx_adt_rest_abapgit IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous   = previous
        subtype    = subtype
        msgv1      = msgv1
        msgv2      = msgv2
        msgv3      = msgv3
        msgv4      = msgv4
        properties = properties.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    mv_http_status = iv_http_status.
  ENDMETHOD.


  METHOD get_http_status.

    IF mv_http_status IS INITIAL.
      result = cl_rest_status_code=>gc_client_error_bad_request.
    ELSE.
      result = mv_http_status.
    ENDIF.

  ENDMETHOD.


  METHOD get_message_var.

    IF iv_attribute IS NOT INITIAL.
      ASSIGN ix_error->(iv_attribute) TO FIELD-SYMBOL(<lv_msg_var>).
      rv_msg_var = <lv_msg_var>.
    ENDIF.

  ENDMETHOD.


  METHOD get_namespace.

    result = 'org.abapgit.adt'.

  ENDMETHOD.


  METHOD get_type ##needed.
  ENDMETHOD.


  METHOD raise_with_error.

    DATA lx_error        TYPE REF TO cx_root.
    DATA lo_message      TYPE REF TO if_t100_message.
    DATA lo_next_message TYPE REF TO if_t100_message.

    lx_error = ix_error.
    lo_message ?= ix_error.

    WHILE lx_error->previous IS BOUND.

      TRY.
          lo_next_message ?= lx_error->previous.
          lo_message = lo_next_message.
          lx_error = lx_error->previous.
        CATCH cx_sy_move_cast_error.
          EXIT.
      ENDTRY.

    ENDWHILE.

    DATA(ls_msg_key) = lo_message->t100key.

    DATA(lv_msgv1) = get_message_var(
      ix_error     = lx_error
      iv_attribute = ls_msg_key-attr1 ).
    DATA(lv_msgv2) = get_message_var(
      ix_error     = lx_error
      iv_attribute = ls_msg_key-attr2 ).
    DATA(lv_msgv3) = get_message_var(
      ix_error     = lx_error
      iv_attribute = ls_msg_key-attr3 ).
    DATA(lv_msgv4) = get_message_var(
      ix_error     = lx_error
      iv_attribute = ls_msg_key-attr4 ).

    RAISE EXCEPTION TYPE zcx_adt_rest_abapgit
      MESSAGE
      ID ls_msg_key-msgid
      TYPE 'E'
      NUMBER ls_msg_key-msgno
      WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
      EXPORTING
        iv_http_status = iv_http_status.

  ENDMETHOD.
ENDCLASS.
