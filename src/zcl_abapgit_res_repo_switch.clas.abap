CLASS zcl_abapgit_res_repo_switch DEFINITION PUBLIC INHERITING FROM cl_adt_rest_resource CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PRIVATE SECTION.
    METHODS branch_exists IMPORTING iv_repo          TYPE REF TO zcl_abapgit_repo_online
                                    iv_branch_name   TYPE string
                          RETURNING VALUE(rs_exists) TYPE abap_bool.
ENDCLASS.



CLASS zcl_abapgit_res_repo_switch IMPLEMENTATION.

  METHOD post.
    DATA: lv_repo_key        TYPE zif_abapgit_persistence=>ty_value,
          lo_http_utility    TYPE REF TO cl_http_utility,
          lo_repo            TYPE REF TO zcl_abapgit_repo_online,
          lv_username        TYPE string,
          lv_password        TYPE string,
          lv_inner_req       TYPE REF TO if_rest_request,
          lv_branch          TYPE string,
          lv_createifmissing TYPE string.

    TRY.
        CREATE OBJECT lo_http_utility.
        lv_inner_req = request->get_inner_rest_request( ).

        request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true IMPORTING value = lv_repo_key ).
        request->get_uri_attribute( EXPORTING name = 'branch' mandatory = abap_true IMPORTING value = lv_branch ).
        request->get_uri_query_parameter(  EXPORTING name = 'create' IMPORTING value = lv_createifmissing ).
        lv_username = lv_inner_req->get_header_field( iv_name = 'Username' ).
*------ Client encodes password with base64 algorithm
        lv_password = lo_http_utility->decode_base64( lv_inner_req->get_header_field( iv_name = 'Password' ) ).
        TRANSLATE lv_createifmissing TO UPPER CASE.

        zcl_abapgit_default_auth_info=>refresh( ).
        zcl_abapgit_default_auth_info=>set_auth_info( iv_user = lv_username
                                                     iv_password = lv_password ).
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).

        IF branch_exists( iv_repo = lo_repo iv_branch_name = lv_branch ) = abap_true.
          lo_repo->set_branch_name( lv_branch ).
        ELSEIF lv_createifmissing = 'TRUE'.
          lo_repo->create_branch( lv_branch ).
        ENDIF.

      CATCH zcx_abapgit_exception cx_root INTO DATA(lx_exception).
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.

  METHOD branch_exists.
    TRY.
        DATA(lo_branches) = zcl_abapgit_git_transport=>branches( iv_repo->get_url(  ) ).
        lo_branches->find_by_name( iv_branch_name ).
        rs_exists = abap_true.
      CATCH cx_root INTO DATA(cx).
        rs_exists = abap_false.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.