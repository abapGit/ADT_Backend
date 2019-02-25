class ZCX_ADT_REST_ABAPGIT definition
  public
  inheriting from CX_ADT_REST
  final
  create public .

  public section.

    interfaces IF_T100_DYN_MSG .

    class-methods RAISE_WITH_ERROR
      importing
        !IX_ERROR       type ref to CX_ROOT
        !IV_HTTP_STATUS type I optional
      raising
        ZCX_ADT_REST_ABAPGIT .
    methods CONSTRUCTOR
      importing
        TEXTID          like IF_T100_MESSAGE=>T100KEY optional
        PREVIOUS        like PREVIOUS optional
        SUBTYPE         type SADT_EXC_TYPE optional
        MSGV1           type SYMSGV optional
        MSGV2           type SYMSGV optional
        MSGV3           type SYMSGV optional
        MSGV4           type SYMSGV optional
        !PROPERTIES     type ref to IF_ADT_EXCEPTION_PROPERTIES optional
        !IV_HTTP_STATUS type I optional.

    methods GET_HTTP_STATUS
        redefinition .
    methods GET_NAMESPACE
        redefinition .
    methods GET_TYPE
        redefinition .
  private section.

    data MV_HTTP_STATUS type I.

    class-methods GET_MESSAGE_VAR
      importing
        IX_ERROR          type ref to CX_ROOT
        IV_ATTRIBUTE      type CSEQUENCE
      returning
        value(RV_MSG_VAR) type SYMSGV.

ENDCLASS.



CLASS zcx_adt_rest_abapgit IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
    call method SUPER->CONSTRUCTOR
      exporting
        PREVIOUS   = PREVIOUS
        SUBTYPE    = SUBTYPE
        MSGV1      = MSGV1
        MSGV2      = MSGV2
        MSGV3      = MSGV3
        MSGV4      = MSGV4
        PROPERTIES = PROPERTIES.
    clear ME->TEXTID.
    if TEXTID is initial.
      IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
    else.
      IF_T100_MESSAGE~T100KEY = TEXTID.
    endif.
    MV_HTTP_STATUS = IV_HTTP_STATUS.
  endmethod.


  method GET_HTTP_STATUS.

    if MV_HTTP_STATUS is initial.
      RESULT = CL_REST_STATUS_CODE=>GC_CLIENT_ERROR_BAD_REQUEST.
    else.
      RESULT = MV_HTTP_STATUS.
    endif.

  endmethod.


  method GET_MESSAGE_VAR.

    if IV_ATTRIBUTE is not initial.
      assign IX_ERROR->(IV_ATTRIBUTE) to field-symbol(<LV_MSG_VAR>).
      RV_MSG_VAR = <LV_MSG_VAR>.
    endif.

  endmethod.


  method GET_NAMESPACE.

    RESULT = 'org.abapgit.adt'.

  endmethod.


  method GET_TYPE ##needed.
  endmethod.


  method RAISE_WITH_ERROR.

    data LX_ERROR        type ref to CX_ROOT.
    data LO_MESSAGE      type ref to IF_T100_MESSAGE.
    data LO_NEXT_MESSAGE type ref to IF_T100_MESSAGE.

    LX_ERROR = IX_ERROR.
    LO_MESSAGE ?= IX_ERROR.

    while LX_ERROR->PREVIOUS is bound.

      try.
          LO_NEXT_MESSAGE ?= LX_ERROR->PREVIOUS.
          LO_MESSAGE = LO_NEXT_MESSAGE.
          LX_ERROR = LX_ERROR->PREVIOUS.
        catch CX_SY_MOVE_CAST_ERROR.
          exit.
      endtry.

    endwhile.

    data(LS_MSG_KEY) = LO_MESSAGE->T100KEY.

    data(LV_MSGV1) = GET_MESSAGE_VAR(
      IX_ERROR     = LX_ERROR
      IV_ATTRIBUTE = LS_MSG_KEY-ATTR1 ).
    data(LV_MSGV2) = GET_MESSAGE_VAR(
      IX_ERROR     = LX_ERROR
      IV_ATTRIBUTE = LS_MSG_KEY-ATTR2 ).
    data(LV_MSGV3) = GET_MESSAGE_VAR(
      IX_ERROR     = LX_ERROR
      IV_ATTRIBUTE = LS_MSG_KEY-ATTR3 ).
    data(LV_MSGV4) = GET_MESSAGE_VAR(
      IX_ERROR     = LX_ERROR
      IV_ATTRIBUTE = LS_MSG_KEY-ATTR4 ).

    raise exception type ZCX_ADT_REST_ABAPGIT
      message
      id LS_MSG_KEY-MSGID
      type 'E'
      number LS_MSG_KEY-MSGNO
      with LV_MSGV1 LV_MSGV2 LV_MSGV3 LV_MSGV4
      exporting
        IV_HTTP_STATUS = IV_HTTP_STATUS.

  endmethod.
ENDCLASS.
