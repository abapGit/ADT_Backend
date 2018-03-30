CLASS zcl_abapgit_adt_test DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS .

  PUBLIC SECTION.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_response,
        status TYPE string,
        body   TYPE string,
      END OF ty_response .

    METHODS get
      IMPORTING
        !iv_uri            TYPE string
        !iv_body           TYPE string OPTIONAL
      RETURNING
        VALUE(rs_response) TYPE ty_response .
    METHODS put
      IMPORTING
        !iv_uri            TYPE string
        !iv_body           TYPE string OPTIONAL
      RETURNING
        VALUE(rs_response) TYPE ty_response .
    METHODS post
      IMPORTING
        !iv_uri            TYPE string
        !iv_body           TYPE string OPTIONAL
      RETURNING
        VALUE(rs_response) TYPE ty_response .
    METHODS delete
      IMPORTING
        !iv_uri            TYPE string
        !iv_body           TYPE string OPTIONAL
      RETURNING
        VALUE(rs_response) TYPE ty_response .
  PRIVATE SECTION.

    METHODS request
      IMPORTING
        !iv_uri            TYPE string
        !iv_body           TYPE string
        !iv_method         TYPE string
      RETURNING
        VALUE(rs_response) TYPE ty_response .
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT_TEST IMPLEMENTATION.


  METHOD delete.

    rs_response = request(
      iv_uri    = iv_uri
      iv_body   = iv_body
      iv_method = 'DELETE' ).

  ENDMETHOD.


  METHOD get.

    rs_response = request(
      iv_uri    = iv_uri
      iv_body   = iv_body
      iv_method = 'GET' ).

  ENDMETHOD.


  METHOD post.

    rs_response = request(
      iv_uri    = iv_uri
      iv_body   = iv_body
      iv_method = 'POST' ).

  ENDMETHOD.


  METHOD put.

    rs_response = request(
      iv_uri    = iv_uri
      iv_body   = iv_body
      iv_method = 'PUT' ).

  ENDMETHOD.


  METHOD request.

    DATA: ls_response TYPE sadt_rest_response.


    DATA(ls_request) = VALUE sadt_rest_request(
      request_line = VALUE #(
        method  = iv_method
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
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    rs_response = VALUE #(
      status = ls_response-status_line-status_code
      body   = xstring_to_string_utf8( ls_response-message_body ) ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
