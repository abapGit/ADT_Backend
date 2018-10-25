CLASS zcl_abapgit_res_repos DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result .
    TYPES:
      BEGIN OF ty_request_data,
        branch           TYPE string,
        package          TYPE string,
        password         TYPE string,
        transportrequest TYPE string,
        url              TYPE string,
        user             TYPE string,
      END OF ty_request_data .

    TYPES : BEGIN OF ty_repo_w_links.
        INCLUDE  TYPE zif_abapgit_persistence=>ty_repo.
    TYPES: links TYPE if_atom_types=>link_t.
    TYPES : END OF ty_repo_w_links.

    TYPES: tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name TYPE seoclsname VALUE 'ZCL_ABAPGIT_RES_REPOS' ##NO_TEXT.
    CONSTANTS co_resource_type TYPE string VALUE 'REPOS' ##NO_TEXT.         "EC NOTEXT
    CONSTANTS co_st_name_post TYPE string VALUE 'ZABAPGIT_ST_REPO_POST' ##NO_TEXT.
    CONSTANTS co_root_name_post TYPE string VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v1 TYPE string VALUE 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1 TYPE string VALUE 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get TYPE string VALUE 'ZABAPGIT_ST_REPOS' ##NO_TEXT.
    CONSTANTS co_root_name_get TYPE string VALUE 'REPOSITORIES' ##NO_TEXT.

    METHODS get
        REDEFINITION .
    METHODS post
        REDEFINITION .
protected section.
private section.

  methods VALIDATE_REQUEST_DATA
    importing
      !IS_REQUEST_DATA type TY_REQUEST_DATA
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_LINKS
    importing
      !IV_REPO_KEY type ZIF_ABAPGIT_PERSISTENCE=>TY_VALUE
    returning
      value(RT_LINKS) type IF_ATOM_TYPES=>LINK_T .
ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPOS IMPLEMENTATION.


  METHOD get.

    DATA lv_text TYPE string.
    DATA lt_repos_w_links TYPE tt_repo_w_links.
    DATA ls_repos_w_links LIKE LINE OF lt_repos_w_links.

    DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
          st_name      = co_st_name_get
          root_name    = co_root_name_get
          content_type = co_content_type_repos_v1 ).

*** validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.
        DATA(lt_repos) = NEW zcl_abapgit_persistence_repo( )->list( ).

*** Consider only online repos
        DELETE lt_repos WHERE offline = abap_true.

        LOOP AT lt_repos ASSIGNING FIELD-SYMBOL(<ls_repos>).
          CLEAR ls_repos_w_links.
          MOVE-CORRESPONDING <ls_repos> TO ls_repos_w_links.

          ls_repos_w_links-links = get_links( <ls_repos>-key ).
          APPEND ls_repos_w_links TO lt_repos_w_links.
        ENDLOOP.

        response->set_body_data(
                content_handler = lo_resp_content_handler
                data            = lt_repos_w_links
            ).

      CATCH cx_st_error zcx_abapgit_exception INTO DATA(lx_error).
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_error
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_links.

    CONSTANTS:
      co_rel_pull  TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/pull',
      co_root_path TYPE string VALUE '/sap/bc/adt/abapgit'. "todo: extract

    DATA(lo_atom_util) = cl_adt_atom_utility=>create_instance( ).



    lo_atom_util->append_link( EXPORTING
                                 rel = co_rel_pull
                                 href = |{ co_root_path }/repos/{ iv_repo_key }/pull|
                              CHANGING
                                 links = rt_links
                               ).

  ENDMETHOD.


  METHOD post.

    DATA:
      ls_request_data TYPE ty_request_data,
      result_request  TYPE sadt_status_message.

*** Content Handler
*** Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that the client sends a correct 'Content-Type:' header
    DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
        request         = request
        content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                              st_name      = co_st_name_post
                              root_name    = co_root_name_post
                              content_type = co_content_type_repo_v1 ) ).

*** Retrieve request data
    request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    TRY.
*** Prerequisite check for request values
        validate_request_data( ls_request_data ).

*** Set logon information if supplied
        IF ls_request_data-user IS NOT INITIAL AND ls_request_data-password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>refresh( ).
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user = ls_request_data-user
                                                       iv_password = ls_request_data-password ).

        ENDIF.

*** Set the default transport request
        zcl_abapgit_default_transport=>get_instance( )->set( CONV #( ls_request_data-transportrequest ) )..

*** Create online repo
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_online(
             iv_url         = ls_request_data-url
             iv_branch_name = ls_request_data-branch
             iv_package     = CONV devclass( ls_request_data-package )
             ).

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

*        TRY.
**** Unlink repo due to method deserialize calls interally a commit
*            zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).
*            COMMIT WORK.
*          CATCH zcx_abapgit_exception.
*        ENDTRY.

        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_abapgit_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.


  ENDMETHOD.


  METHOD validate_request_data.

*** Check whether git url is well formed
    zcl_abapgit_url=>validate( |{ is_request_data-url }| ).

*** Check whether package is already used
    zcl_abapgit_repo_srv=>get_instance( )->validate_package( CONV #( is_request_data-package ) ).

*** CHECK whether git url is already used

    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
      IF cl_http_utility=>if_http_utility~unescape_url( zcl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
        zcx_abapgit_exception=>raise( |Repository URL { is_request_data-url } is already in use with package { <ls_repo_list>->get_package( ) }| ).
      ENDIF.
    ENDLOOP.

*** transport request exists
    SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
      WHERE
      trkorr = @is_request_data-transportrequest.

    IF sy-subrc NE 0.
      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } does not exists or is already released| ).
    ELSEIF ls_e070-trstatus NE 'D'.
      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } is not modifiable| ).
    ELSEIF ls_e070-as4user NE sy-uname.
      zcx_abapgit_exception=>raise( |Transport Request { is_request_data-transportrequest } does not belong to user { sy-uname }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
