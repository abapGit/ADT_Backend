CLASS zcl_abapgit_res_repo_checks DEFINITION
PUBLIC
INHERITING FROM cl_adt_rest_resource
FINAL
CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_repo_service       TYPE REF TO zif_abapgit_repo_srv.
    " DATA: lo_repo_check_service TYPE REF TO zif_abapgit_repository_checks.

    "! <p>Checks whether the abapgit exception which is raised is
    "! because of any authentication related issues on the git server</p>
    METHODS is_authorization_issue
      IMPORTING iv_exc_text      TYPE string
      EXPORTING ev_http_status   TYPE i
                ev_is_auth_issue TYPE abap_bool.

    METHODS check_connection
        IMPORTING io_repository TYPE REF TO zcl_abapgit_repo_online
                  iv_service    TYPE string
        RAISING   zcx_abapgit_exception.

    METHODS get_repository_service
      RETURNING VALUE(ro_repo_service) TYPE REF TO zif_abapgit_repo_srv.

    CLASS-METHODS get_request_uri
      IMPORTING iv_url        TYPE string
                iv_service    TYPE string
      RETURNING VALUE(rv_uri) TYPE string
      RAISING   zcx_abapgit_exception.

ENDCLASS.


CLASS zcl_abapgit_res_repo_checks IMPLEMENTATION.

  METHOD get_request_uri.
    rv_uri = |{ zcl_abapgit_url=>path_name( iv_url ) }/info/refs?service=git-{ iv_service }-pack|.
  ENDMETHOD.

  METHOD check_connection.
    DATA lt_headers TYPE zcl_abapgit_http=>ty_headers.
    DATA ls_header  LIKE LINE OF lt_headers.

    ls_header-key   = '~request_uri'.
    ls_header-value = get_request_uri( iv_url     = io_repository->get_url( )
                                       iv_service = iv_service ).
    APPEND ls_header TO lt_headers.
    zcl_abapgit_http=>create_by_url( iv_url     = io_repository->get_url( )
                                     it_headers = lt_headers ).
  ENDMETHOD.

  METHOD is_authorization_issue.
    IF iv_exc_text     = 'HTTP 401, unauthorized'.
      ev_http_status = cl_rest_status_code=>gc_client_error_unauthorized.
    ELSEIF iv_exc_text = 'HTTP 403, forbidden'.
      ev_http_status = cl_rest_status_code=>gc_client_error_forbidden.
    ELSEIF iv_exc_text = 'HTTP 404, not found'.
      ev_http_status = cl_rest_status_code=>gc_client_error_not_found.
    ENDIF.

    IF ev_http_status IS NOT INITIAL.
      ev_is_auth_issue = abap_true.
    ELSE.
      ev_is_auth_issue = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_repository_service.
    ro_repo_service = zcl_abapgit_repo_srv=>get_instance( ).
  ENDMETHOD.

  METHOD post.
    DATA:
      lv_repo_key      TYPE zif_abapgit_persistence=>ty_value,
      lv_username      TYPE string,
      lv_password      TYPE string,
      lv_service       TYPE string,
      lo_repo_online   TYPE REF TO zcl_abapgit_repo_online,
      lo_http_utility  TYPE REF TO cl_http_utility,
      lv_http_status   TYPE i,
      lv_is_auth_issue TYPE abap_bool.

*-- Get repository key
    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

*-- Get credentials from request header
    CREATE OBJECT lo_http_utility.
    lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).

*-- Client encodes password with base64 algorithm
    lv_password = lo_http_utility->decode_base64(
      encoded = request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' ) ).

    TRY.

        IF mv_repo_service IS INITIAL.
          mv_repo_service = get_repository_service( ).
        ENDIF.

* ----- Determine repository from repository key
        DATA(lo_repo) = mv_repo_service->get( lv_repo_key ).
        lo_repo_online ?= lo_repo.

        lv_service = 'upload'.

*------ SET credentials in CASE they are supplied
        IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user     = lv_username
                                                       iv_password = lv_password ).
          lv_service = 'receive'.
        ENDIF.

*------ Check connection with git server
        check_connection( io_repository = lo_repo_online iv_service = lv_service ).

      CATCH zcx_abapgit_exception INTO DATA(lx_abapgit_exception).

*------ Check whether the exception occurred because of any authentication issues
        is_authorization_issue( EXPORTING iv_exc_text = lx_abapgit_exception->get_text( )
                                IMPORTING ev_http_status = lv_http_status ev_is_auth_issue = lv_is_auth_issue ).
        IF lv_is_auth_issue = abap_true.

*---------- Convert lv_http_status to a string and then remove the trailing whitespaces
*---------- This step is added as directly sending the integer http status as part of exception properties will
*---------- will result in a trailing space at the end in the response
          DATA(lv_http_status_string) = CONV string( lv_http_status ).
          CONDENSE lv_http_status_string NO-GAPS.

*---------- Raise internal server error, as the connection to github failed from abap server
*---------- Return the error code from the abapgit exception as part of additional adt exception properties.
          DATA(lv_properties) = zcx_adt_rest_abapgit=>create_properties( ).
          lv_properties->add_property(
            key = 'http_status'
            value = lv_http_status_string ).
          zcx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception
                                                 iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
          "  iv_properties = iv_properties ).
        ELSE.
          zcx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
        ENDIF.

        response->set_status( cl_rest_status_code=>gc_success_ok ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
