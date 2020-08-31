CLASS zcl_abapgit_res_repos DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF  ty_s_result,
        reason  TYPE string,
        message TYPE string,
      END OF ty_s_result.
    TYPES:
      BEGIN OF ty_request_data,
        branch           TYPE string,
        package          TYPE string,
        password         TYPE string,
        transportrequest TYPE string,
        url              TYPE string,
        user             TYPE string,
      END OF ty_request_data,
      tt_request_data TYPE TABLE OF ty_request_data.

    TYPES: BEGIN OF ty_repo_w_links.
             INCLUDE TYPE zif_abapgit_persistence=>ty_repo.
    TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links.

    TYPES: tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name             TYPE seoclsname VALUE 'ZCL_ABAPGIT_RES_REPOS'.
    CONSTANTS co_resource_type          TYPE string     VALUE 'REPOS'.
    CONSTANTS co_st_name_post           TYPE string     VALUE 'ZABAPGIT_ST_REPO_POST'.
    CONSTANTS co_st_name_post_v2        TYPE string     VALUE 'ZABAPGIT_ST_REPO_POST_V2' .
    CONSTANTS co_st_name_post_res       TYPE string     VALUE 'ZABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_post         TYPE string     VALUE 'REPOSITORY'.
    CONSTANTS co_root_name_post_v2      TYPE string     VALUE 'REPOSITORIES'.
    CONSTANTS co_root_name_post_res     TYPE string     VALUE 'OBJECTS'.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v3+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v2   TYPE string     VALUE 'application/abapgit.adt.repo.v4+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v2+xml' ##NO_TEXT.
    CONSTANTS co_content_type_object_v1 TYPE string     VALUE 'application/abapgit.adt.repo.object.v2+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get            TYPE string     VALUE 'ZABAPGIT_ST_REPOS'.
    CONSTANTS co_root_name_get          TYPE string     VALUE 'REPOSITORIES'.

    METHODS constructor.
    METHODS get REDEFINITION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_abapgit_provider TYPE REF TO lif_abapgit_provider.

    METHODS validate_request_data
      IMPORTING
        !is_request_data TYPE ty_request_data
      RAISING
        zcx_abapgit_exception.
    METHODS get_links
      IMPORTING
        !iv_repo_key    TYPE zif_abapgit_persistence=>ty_value
      RETURNING
        VALUE(rt_links) TYPE if_atom_types=>link_t.
    METHODS:
      post_v1  IMPORTING iv_request  TYPE REF TO if_adt_rest_request
                         iv_response TYPE REF TO if_adt_rest_response
                         iv_context  TYPE REF TO if_rest_context OPTIONAL
               RAISING   cx_adt_rest,
      post_v2  IMPORTING iv_request  TYPE REF TO if_adt_rest_request
                         iv_response TYPE REF TO if_adt_rest_response
                         iv_context  TYPE REF TO if_rest_context OPTIONAL
               RAISING   cx_adt_rest,
      import_repository IMPORTING is_request_data TYPE ty_request_data
                        RAISING   zcx_adt_rest_abapgit,
      set_import_response_data IMPORTING iv_response TYPE REF TO if_adt_rest_response
                               RAISING   cx_adt_rest,
      set_abapgit_provider IMPORTING io_abapgit_provider TYPE REF TO lif_abapgit_provider.

ENDCLASS.



CLASS zcl_abapgit_res_repos IMPLEMENTATION.

  METHOD get.

    DATA lv_text TYPE string.
    DATA lt_repos_w_links TYPE tt_repo_w_links.
    DATA ls_repos_w_links LIKE LINE OF lt_repos_w_links.

    DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
          st_name      = co_st_name_get
          root_name    = co_root_name_get
          content_type = co_content_type_repos_v1 ).

    "validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.
        DATA(li_repo) = zcl_abapgit_persist_factory=>get_repo( ).
        DATA(lt_repos) = li_repo->list( ).

        "Consider only online repos
        DELETE lt_repos WHERE offline = abap_true.

        LOOP AT lt_repos ASSIGNING FIELD-SYMBOL(<ls_repos>).
          CLEAR ls_repos_w_links.
          MOVE-CORRESPONDING <ls_repos> TO ls_repos_w_links.

          ls_repos_w_links-links = get_links( <ls_repos>-key ).
          APPEND ls_repos_w_links TO lt_repos_w_links.
        ENDLOOP.

        response->set_body_data(
                content_handler = lo_resp_content_handler
                data            = lt_repos_w_links ).

      CATCH cx_st_error zcx_abapgit_exception INTO DATA(lx_error).
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_error
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_links.

    CONSTANTS:
      lc_rel_pull   TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/pull',
      " lc_rel_status TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/status',
      " lc_rel_log    TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/log',
      lc_rel_stage  TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/stage',
      lc_rel_commit TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/push',
      lc_rel_check  TYPE string VALUE 'http://www.sap.com/adt/abapgit/relations/check',
      lc_root_path  TYPE string VALUE '/sap/bc/adt/abapgit'. "todo: extract

    DATA(lo_atom_util) = cl_adt_atom_utility=>create_instance( ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_rel_pull
        href = |{ lc_root_path }/repos/{ iv_repo_key }/pull|
      CHANGING
        links = rt_links ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_rel_stage
        href = |{ lc_root_path }/repos/{ iv_repo_key }/stage|
        type = |stage_link|
      CHANGING
        links = rt_links ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_rel_commit
        href = |{ lc_root_path }/repos/{ iv_repo_key }/push|
        type = |push_link|
      CHANGING
        links = rt_links ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_rel_check
        href = |{ lc_root_path }/repos/{ iv_repo_key }/checks|
        type = |check_link|
      CHANGING
        links = rt_links ).
    " IF is_app_log_key-app_log IS NOT INITIAL.
    "   lo_atom_util->append_link(
    "     EXPORTING
    "       rel  = lc_rel_status
    "       href = |{ lc_root_path }/repos/{ iv_repo_key }/status/{ is_app_log_key-app_log }|
    "       type = |status_link|
    "     CHANGING
    "       links = rt_links ).

    "   lo_atom_util->append_link(
    "     EXPORTING
    "       rel  = lc_rel_log
    "       href = |{ lc_root_path }/repos/{ iv_repo_key }/log/{ is_app_log_key-app_log }|
    "       type = |log_link|
    "     CHANGING
    "       links = rt_links ).
    " ENDIF.
  ENDMETHOD.


  METHOD post.

    DATA(ls_requested_content_type) = request->get_inner_rest_request( )->get_header_field(
      iv_name = if_http_header_fields=>content_type ).

    CASE ls_requested_content_type.
      WHEN co_content_type_repo_v1.
        me->post_v1( iv_request  = request
                     iv_response = response
                     iv_context  = context ).
      WHEN co_content_type_repo_v2.
        me->post_v2( iv_request  = request
                     iv_response = response
                     iv_context  = context ).
      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
    ENDCASE.

  ENDMETHOD.


  METHOD validate_request_data.

    DATA: lv_tr_check_required TYPE abap_bool VALUE abap_true.

    "check whether git url is well formed
    zcl_abapgit_url=>validate( |{ is_request_data-url }| ).

    "check whether package is already used
    me->mv_abapgit_provider->validate_package( iv_package = CONV #( is_request_data-package ) ).

    "check whether git url is already used
    DATA(lt_repo_list) = me->mv_abapgit_provider->list_repositories( ).
    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
      IF cl_http_utility=>if_http_utility~unescape_url(
        zcl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
        MESSAGE e002(za4c_agit_adt) WITH is_request_data-url <ls_repo_list>->get_package( ) INTO DATA(lv_msg).
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    " Transport request check is only required if recording is switched on for current package
    IF is_request_data-transportrequest IS INITIAL.
      lv_tr_check_required = me->mv_abapgit_provider->is_tr_check_required(
        iv_package = CONV #( is_request_data-package ) ).
    ENDIF.

    IF lv_tr_check_required = abap_true.
      " transport request exists
      me->mv_abapgit_provider->validate_transport_request(
        iv_transport_request = CONV #( is_request_data-transportrequest ) ).
    ENDIF.

  ENDMETHOD.

  METHOD post_v1.

    DATA:
      ls_request_data TYPE ty_request_data.

    " Content Handler
    " Wrap the request content handler in a cl_adt_rest_comp_cnt_handler
    " in order to ensure that the client sends a correct 'Content-Type:' header
    DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
        request         = iv_request
        content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                              st_name      = co_st_name_post
                              root_name    = co_root_name_post
                              content_type = co_content_type_repo_v1 ) ).

    "Retrieve request data
    iv_request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    me->import_repository( is_request_data = ls_request_data ).

    "[A4C_AGIT_LOG]
    me->set_import_response_data( iv_response ).

    iv_response->set_status( cl_rest_status_code=>gc_success_ok ).

  ENDMETHOD.

  METHOD post_v2.

    DATA: lt_request_data TYPE tt_request_data.

    DATA(lo_adt_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = co_st_name_post_v2
      root_name    = co_root_name_post_v2
      content_type = co_content_type_repo_v2 ).
    DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create( request         = iv_request
                                                                             content_handler = lo_adt_content_handler ).

    iv_request->get_body_data( EXPORTING content_handler = lo_request_content_handler
                               IMPORTING data            = lt_request_data ).

    IF lt_request_data IS INITIAL.
      iv_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    LOOP AT lt_request_data INTO DATA(single_repository).
      me->import_repository( is_request_data = single_repository ).
    ENDLOOP.

    "[A4C_AGIT_LOG]
    me->set_import_response_data( iv_response ).

    iv_response->set_status( cl_rest_status_code=>gc_success_created ).

  ENDMETHOD.

  METHOD set_abapgit_provider.
    me->mv_abapgit_provider = io_abapgit_provider.
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT me->mv_abapgit_provider TYPE lcl_abapgit_provider.

  ENDMETHOD.

  METHOD import_repository.
    TRY.
        "Prerequisite check for request values
        validate_request_data( is_request_data ).

        "Set log-on information if supplied
        IF is_request_data-user IS NOT INITIAL AND is_request_data-password IS NOT INITIAL.
          me->mv_abapgit_provider->set_authentication_info( iv_user     = is_request_data-user
                                                         iv_password = is_request_data-password ).
        ENDIF.

        me->mv_abapgit_provider->perform_import( is_request_data = is_request_data ).

        "Handle Issues
      CATCH zcx_abapgit_exception INTO DATA(lx_abapgit_exception).
        ROLLBACK WORK.

*        TRY.
*            "Unlink repo due to method deserialize calls interally a commit
*            zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).
*            COMMIT WORK.
*          CATCH zcx_abapgit_exception.
*        ENDTRY.

        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_abapgit_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.
  ENDMETHOD.

  METHOD set_import_response_data.
    TYPES:
      BEGIN OF t_obj_result,
        obj_type   TYPE trobjtype,
        obj_name   TYPE sobj_name,
        obj_status TYPE symsgty,
        package    TYPE devclass,
        msg_type   TYPE symsgty,
        msg_text   TYPE string,
      END OF t_obj_result.

    DATA lt_result_table TYPE STANDARD TABLE OF t_obj_result WITH DEFAULT KEY.

    "Response Content Handler
    DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = co_st_name_post_res
      root_name    = co_root_name_post_res
      content_type = co_content_type_object_v1 ).

    " TODO fill lt_result_table
    iv_response->set_body_data(
      content_handler = lo_resp_content_handler
      data            = lt_result_table ).

  ENDMETHOD.

ENDCLASS.
