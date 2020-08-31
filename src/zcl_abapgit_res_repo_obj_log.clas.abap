CLASS zcl_abapgit_res_repo_obj_log DEFINITION
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
      BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data.
    TYPES: BEGIN OF ty_repo_w_links.
             INCLUDE TYPE zif_abapgit_persistence=>ty_repo.
    TYPES:   links TYPE if_atom_types=>link_t.
    TYPES: END OF ty_repo_w_links.
    TYPES:
      tt_repo_w_links TYPE STANDARD TABLE OF ty_repo_w_links WITH DEFAULT KEY.

    CONSTANTS co_class_name             TYPE seoclsname VALUE 'ZCL_ABAPGIT_RES_REPOS' ##NO_TEXT.
    CONSTANTS co_resource_type TYPE string     VALUE 'REPOS' ##NO_TEXT.             "EC NOTEXT
    CONSTANTS co_st_name_pull           TYPE string     VALUE 'ZABAPGIT_ST_REPO_PULL' ##NO_TEXT.
    CONSTANTS co_st_name_post_res       TYPE string     VALUE 'ZABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_pull         TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_root_name_post_res     TYPE string     VALUE 'OBJECTS'.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v3+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v2+xml' ##NO_TEXT.
    CONSTANTS co_content_type_object_v1 TYPE string     VALUE 'application/abapgit.adt.repo.object.v2+xml' ##NO_TEXT.
    CONSTANTS co_st_name_get            TYPE string     VALUE 'ZABAPGIT_ST_REPOS' ##NO_TEXT.
    CONSTANTS co_root_name_get          TYPE string     VALUE 'REPOSITORIES' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v2   TYPE string     VALUE 'application/abapgit.adt.repo.v4+xml' ##NO_TEXT.

    METHODS get REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS validate_request_data
      IMPORTING
        !is_request_data TYPE ty_request_pull_data
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_res_repo_obj_log IMPLEMENTATION.


  METHOD get.
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
    DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
         st_name      = co_st_name_post_res
         root_name    = co_root_name_post_res
         content_type = co_content_type_object_v1 ).

    "validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.

*------ prepare response information
        response->set_body_data(
          content_handler = lo_resp_content_handler
          data            = lt_result_table ).

        response->set_status( cl_rest_status_code=>gc_success_ok ).

*---- Handle issues
      CATCH zcx_abapgit_exception cx_uuid_error INTO DATA(lx_exception).
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD validate_request_data.

*    "check whether git url is well formed
*    zcl_abapgit_url=>validate( |{ is_request_data-url }| ).
*
*    "check whether package is already used
*    zcl_abapgit_repo_srv=>get_instance( )->validate_package( CONV #( is_request_data-package ) ).
*
*    "check whether git url is already used
*    DATA(lt_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
*    LOOP AT lt_repo_list ASSIGNING FIELD-SYMBOL(<ls_repo_list>).
*      IF cl_http_utility=>if_http_utility~unescape_url(
*               zcl_abapgit_url=>name( is_request_data-url ) ) EQ <ls_repo_list>->get_name( ).
*        MESSAGE e002(A4C_AGIT_ADT) WITH is_request_data-url <ls_repo_list>->get_package( ) INTO DATA(lv_msg).
*        zcx_abapgit_exception=>raise_t100( ).
*      ENDIF.
*    ENDLOOP.
*
*    "transport request exists
*    SELECT SINGLE * FROM e070 INTO @DATA(ls_e070)
*      WHERE
*      trkorr = @is_request_data-transportrequest.
*
*    IF sy-subrc NE 0.
*      MESSAGE e003(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-trstatus NE 'D'.
*      MESSAGE e004(A4C_AGIT_ADT) WITH is_request_data-transportrequest INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ELSEIF ls_e070-as4user NE sy-uname.
*      MESSAGE e005(A4C_AGIT_ADT) WITH is_request_data-transportrequest sy-uname INTO lv_msg.
*      zcx_abapgit_exception=>raise_t100( ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
