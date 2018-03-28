CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      resource TYPE REF TO cl_adt_rest_resource,
      request  TYPE REF TO cl_adt_rest_request_stub,
      response TYPE REF TO cl_adt_rest_response_spy.

    METHODS:
      setup,
      get FOR TESTING RAISING cx_static_check.

ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT request.
    CREATE OBJECT response.
    CREATE OBJECT resource TYPE zcl_abapgit_adt.

  ENDMETHOD.

  METHOD get.

    CONSTANTS: c_key TYPE c LENGTH 1 VALUE '1'.

    DATA:
      ls_repo             TYPE zif_abapgit_persistence=>ty_repo,
      act_content_handler TYPE REF TO if_adt_rest_content_handler.


    request->set_uri_attribute(
      name  = zcl_abapgit_adt=>c_uri_key
      value = c_key ).

* call the resource controller method
    resource->get(
      request  = request
      response = response ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = response->is_body_empty( ) ).

* verify the results
    response->get_body_data( IMPORTING data = ls_repo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_repo-key
      exp = c_key ).

    act_content_handler = response->get_content_handler( ).
    cl_abap_unit_assert=>assert_bound( act_content_handler ).

  ENDMETHOD.

ENDCLASS.
