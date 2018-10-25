class ZCL_ABAPGIT_RES_REPO_PULL definition
  public
  inheriting from CL_ADT_REST_RESOURCE
  final
  create public .

public section.

  types:
    BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result .
  types:
    BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data .
  types:
    BEGIN OF ty_repo_w_links.
        INCLUDE  TYPE zif_abapgit_persistence=>ty_repo.
    TYPES: links TYPE if_atom_types=>link_t.
    TYPES : END OF ty_repo_w_links .
  types:
    tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY .

  constants CO_CLASS_NAME type SEOCLSNAME value 'ZCL_ABAPGIT_RES_REPOS' ##NO_TEXT.
  constants CO_RESOURCE_TYPE type STRING value 'REPOS' ##NO_TEXT.             "EC NOTEXT
  constants CO_ST_NAME_PULL type STRING value 'ZABAPGIT_ST_REPO_PULL' ##NO_TEXT.
  constants CO_ROOT_NAME_PULL type STRING value 'REPOSITORY' ##NO_TEXT.
  constants CO_CONTENT_TYPE_REPO_V1 type STRING value 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
  constants CO_CONTENT_TYPE_REPOS_V1 type STRING value 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
  constants CO_ST_NAME_GET type STRING value 'ZABAPGIT_ST_REPOS' ##NO_TEXT.
  constants CO_ROOT_NAME_GET type STRING value 'REPOSITORIES' ##NO_TEXT.
  constants CO_CONTENT_TYPE_REPO_V2 type STRING value 'application/abapgit.adt.repo.v2+xml' ##NO_TEXT.

  methods POST
    redefinition .
protected section.
PRIVATE SECTION.

  METHODS validate_request_data
    IMPORTING
      !is_request_data TYPE ty_request_pull_data
    RAISING
      zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPO_PULL IMPLEMENTATION.


  METHOD post.

    DATA:
      ls_request_data TYPE ty_request_pull_data,
      result_request  TYPE sadt_status_message,
      lv_repo_key     TYPE zif_abapgit_persistence=>ty_value.

*** Get Repository Key
    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

*** Content Handler
    DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
        request         = request
        content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                              st_name      = co_st_name_pull
                              root_name    = co_root_name_pull
                              content_type = co_content_type_repo_v1 ) ).

*** Retrieve request data
    request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    TRY.
**** Prerequisite check for request values
*        validate_request_data( ls_request_data ).

*** Set logon information if supplied
        IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>refresh( ).
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user = ls_request_data-user
                                                       iv_password = ls_request_data-password ).

        ENDIF.

*** Set the default transport request
        zcl_abapgit_default_transport=>get_instance( )->set( CONV #( ls_request_data-transportrequest ) ).

**** Create online repo
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).

*** Pull objects
        lo_repo->refresh( ).

        DATA(ls_checks) = lo_repo->deserialize_checks( ).

*** Overwrite existing ebjects
        LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
          <ls_overwrite>-decision = 'Y'.
        ENDLOOP.

        LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
          <ls_warning_package>-decision = 'Y'.
        ENDLOOP.

*** Import objects
        ls_checks-transport-transport = ls_request_data-transportrequest.
        lo_repo->deserialize( ls_checks ).

        response->set_status( cl_rest_status_code=>gc_success_ok ).

*** Handle Issues
      CATCH zcx_abapgit_exception INTO DATA(lx_abapgit_exception).
        ROLLBACK WORK.

        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_abapgit_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.


  ENDMETHOD.


  METHOD VALIDATE_REQUEST_DATA.

**** Check whether git url is well formed
*    zcl_abapgit_url=>validate( |{ is_request_data-url }| ).
*
**** Check whether package is already used
*    zcl_abapgit_repo_srv=>get_instance( )->validate_package( CONV #( is_request_data-package ) ).
*
**** CHECK whether git url is already used
*
*    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
*
*    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
*      IF cl_http_utility=>if_http_utility~unescape_url( zcl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
*        zcx_abapgit_exception=>raise( |Repository URL { is_request_data-url } is already in use with package { <ls_repo_list>->get_package( ) }| ).
*      ENDIF.
*    ENDLOOP.
*
**** transport request exists
*    SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
*      WHERE
*      trkorr = @is_request_data-transportrequest.
*
*    IF sy-subrc NE 0.
*      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } does not exists or is already released| ).
*    ELSEIF ls_e070-trstatus NE 'D'.
*      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } is not modifiable| ).
*    ELSEIF ls_e070-as4user NE sy-uname.
*      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } does not belong to user { sy-uname }| ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
