class ZCL_ABAPGIT_ADT_TEST definition
  public
  abstract
  create public
  for testing
  duration short
  risk level harmless .

public section.

  types:
    BEGIN OF ty_response,
           status TYPE string,
           body   TYPE string,
         END OF ty_response .

  methods POST
    importing
      !IV_URI type STRING
      !IV_BODY type STRING optional
    returning
      value(RS_RESPONSE) type TY_RESPONSE .
protected section.
private section.

  class-methods XSTRING_TO_STRING_UTF8
    importing
      !IV_DATA type XSTRING
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_XSTRING_UTF8
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT_TEST IMPLEMENTATION.


  METHOD post.

    DATA: ls_response TYPE sadt_rest_response.


    DATA(ls_request) = VALUE sadt_rest_request(
      request_line = VALUE #(
        method  = 'POST'
        uri     = iv_uri
        version = 'HTTP1/1' )
      header_fields = VALUE #( )
      message_body = string_to_xstring_utf8( iv_body ) ).

    CALL FUNCTION 'SADT_REST_RFC_ENDPOINT'
      DESTINATION 'NONE'
      EXPORTING
        request  = ls_request
      IMPORTING
        response = ls_response
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      ASSERT 0 = 1.
    ENDIF.

    rs_response = VALUE #(
      status = ls_response-status_line-status_code
      body   = xstring_to_string_utf8( ls_response-message_body ) ).

  ENDMETHOD.


  METHOD STRING_TO_XSTRING_UTF8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD XSTRING_TO_STRING_UTF8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
*            ignore_cerr = abap_true
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
