CLASS zcl_abapgit_res_repo_files DEFINITION PUBLIC INHERITING FROM cl_adt_rest_resource FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      co_uri_attribute_key   TYPE string VALUE 'key',
      co_query_parm_filename TYPE string VALUE 'filename',
      co_query_parm_version  TYPE string VALUE 'version',
      co_file_remote         TYPE string VALUE 'remote',
      co_file_local          TYPE string VALUE 'local',
      co_utf_8               TYPE string VALUE 'UTF-8'.
ENDCLASS.



CLASS zcl_abapgit_res_repo_files IMPLEMENTATION.

  METHOD get.
    DATA: lv_repo_key        TYPE zif_abapgit_persistence=>ty_value,
          lv_version         TYPE string,
          lv_filename        TYPE string,
          lv_source          TYPE string,
          lv_username        TYPE string,
          lv_password        TYPE string,
          lo_http_utility    TYPE REF TO cl_http_utility,
          lo_content_handler TYPE REF TO if_adt_rest_content_handler.

    "Get repository key
    request->get_uri_attribute( EXPORTING name  = co_uri_attribute_key mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

    "Get credentials from request header
    CREATE OBJECT lo_http_utility.
    lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).

    "Client encodes password with base64 algorithm
    lv_password = lo_http_utility->decode_base64(
        encoded = request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' ) ).

    "Get filename
    request->get_uri_query_parameter( EXPORTING name  = co_query_parm_filename mandatory = abap_true
                                      IMPORTING value = lv_filename ).

    "Get requested file version
    request->get_uri_query_parameter( EXPORTING name  = co_query_parm_version mandatory = abap_false
                                      IMPORTING value = lv_version ).

    "Content handler for plain text
    lo_content_handler = cl_adt_rest_comp_cnt_handler=>create(
       content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
       request = request ).

    TRY.
        "read the repository from repository key
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).

        "SET credentials in case they are supplied
        IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user     = lv_username
                                                        iv_password = lv_password ).
        ENDIF.

        IF lv_version = co_file_remote. "if requested is remote version

          "read the contents of the requested remote file
          DATA(rfiles) = lo_repo->get_files_remote( ).
          READ TABLE rfiles WITH KEY filename = lv_filename TRANSPORTING all fields INTO DATA(rfile).
          lv_source = cl_abap_codepage=>convert_from( source = rfile-data ignore_cerr = abap_true codepage = co_utf_8 ).

        ELSE. "if requested is local version

          "read the contents of the requested remote file
          DATA(lfiles) = lo_repo->get_files_local( ).
          READ TABLE lfiles WITH KEY file-filename = lv_filename TRANSPORTING all fields INTO DATA(lfile).
          lv_source = cl_abap_codepage=>convert_from(
            source = lfile-file-data ignore_cerr = abap_true codepage = co_utf_8 ).

        ENDIF.
        response->set_body_data( content_handler = lo_content_handler data = lv_source ).
      CATCH zcx_abapgit_exception INTO DATA(lx_exception).
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
