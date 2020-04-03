CLASS zcl_abapgit_res_repo_checks DEFINITION
PUBLIC
INHERITING FROM cl_adt_rest_resource
FINAL
CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lo_repo_service       TYPE REF TO zif_abapgit_repo_srv.
    " DATA: lo_repo_check_service TYPE REF TO zif_abapgit_repository_checks.

    "! <p>Checks whether the abapgit exception which is raised is because of any authentication related issues on the git server</p>
    METHODS is_authorization_issue
      IMPORTING iv_exc_text             TYPE string
      EXPORTING ev_http_status          TYPE i
      RETURNING VALUE(rv_is_auth_issue) TYPE abap_bool.

    METHODS check_connection IMPORTING
                               io_repository TYPE REF TO zcl_abapgit_repo_online
                               iv_service    TYPE string
                             RAISING
                               zcx_abapgit_exception.

    METHODS get_repository_service
      RETURNING VALUE(ro_repo_service) TYPE REF TO zif_abapgit_repo_srv.

ENDCLASS.


CLASS zcl_abapgit_res_repo_checks IMPLEMENTATION.

  METHOD check_connection.
    zcl_abapgit_http=>create_by_url(
      EXPORTING
        iv_url     = io_repository->get_url( )
        iv_service = iv_service
    ).
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
      rv_is_auth_issue = abap_true.
    ELSE.
      rv_is_auth_issue = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_repository_service.
    ro_repo_service = zcl_abapgit_repo_srv=>get_instance( ).
  ENDMETHOD.

  METHOD post.
    DATA:
      lv_repo_key     TYPE zif_abapgit_persistence=>ty_value,
      lv_username     TYPE string,
      lv_password     TYPE string,
      lv_service      TYPE string,
      lo_repo_online  TYPE REF TO zcl_abapgit_repo_online,
      lo_http_utility TYPE REF TO cl_http_utility.

*-- Get repository key
    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

*-- Get credentials from request header
    CREATE OBJECT lo_http_utility.
    lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).

*-- Client encodes password with base64 algorithm
    CALL METHOD lo_http_utility->decode_base64
      EXPORTING
        encoded = request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' )
      RECEIVING
        decoded = lv_password.

    TRY.

        IF lo_repo_service IS INITIAL.
          lo_repo_service = get_repository_service( ).
        ENDIF.

* ----- Determine repository from repository key
        DATA(lo_repo) = lo_repo_service->get( lv_repo_key ).
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
        DATA lv_http_status TYPE i.

*------ Check whether the exception occurred because of any authentication issues
        IF is_authorization_issue( EXPORTING iv_exc_text = lx_abapgit_exception->get_text( ) IMPORTING ev_http_status = lv_http_status ).

*---------- Convert lv_http_status to a string and then remove the trailing whitespaces
*---------- This step is added as directly sending the integer http status as part of exception properties will
*---------- will result in a trailing space at the end in the response
          DATA(lv_http_status_string) = CONV string( lv_http_status ).
          CONDENSE lv_http_status_string NO-GAPS.

*---------- Raise internal server error, as the connection to github failed from abap server
*---------- Return the error code from the abapgit exception as part of additional adt exception properties.
          DATA(iv_properties) = zcx_adt_rest_abapgit=>create_properties( )->add_property( key = 'http_status' value  = lv_http_status_string ).
          zcx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception
                                                 iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
          "  iv_properties = iv_properties ).
        ELSE.
          zcx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
        ENDIF.

        response->set_status( cl_rest_status_code=>gc_success_ok ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
