CLASS zcl_abapgit_res_repo_info_ext DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_request_data,
        url      TYPE string,
        user     TYPE string,
        password TYPE string,
      END OF ty_request_data.
    TYPES:
      BEGIN OF ty_response_data,
        access_mode TYPE string,
        branches    TYPE zif_abapgit_definitions=>ty_git_branch_list_tt,
      END OF ty_response_data.

    CONSTANTS co_content_type_request_v1  TYPE string
      VALUE 'application/abapgit.adt.repo.info.ext.request.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_response_v1 TYPE string
      VALUE 'application/abapgit.adt.repo.info.ext.response.v1+xml' ##NO_TEXT.
    CONSTANTS co_root_name_request        TYPE string VALUE 'REPOSITORY_EXTERNAL_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_request          TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_response         TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RES' ##NO_TEXT.
    CONSTANTS co_root_name_response       TYPE string VALUE 'REPOSITORY_EXTERNAL' ##NO_TEXT.

    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_res_repo_info_ext IMPLEMENTATION.

  METHOD post.

    DATA:
      ls_request_data  TYPE ty_request_data,
      ls_response_data TYPE ty_response_data.

    "Request Content Handler
    "Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that
    "the client sends a correct 'Content-Type:' header
    DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
      request         = request
      content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                            st_name      = co_st_name_request
                            root_name    = co_root_name_request
                            content_type = co_content_type_request_v1 ) ).

    "Response Content Handler
    DATA(lo_response_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = co_st_name_response
      root_name    = co_root_name_response
      content_type = co_content_type_response_v1 ).

    "Validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_response_content_handler
                                        )->check_cnt_type_is_supported( ).

    "Retrieve request data
    request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    TRY.
        "Set logon information if supplied
        IF ls_request_data-user     IS NOT INITIAL AND
           ls_request_data-password IS NOT INITIAL.
          zcl_abapgit_default_auth_info=>refresh( ).
          zcl_abapgit_default_auth_info=>set_auth_info( iv_user     = ls_request_data-user
                                                       iv_password = ls_request_data-password ).
        ENDIF.

        "Check whether passed repo URL has public or privated access
        ls_response_data-access_mode = zcl_abapgit_http=>determine_access_level( ls_request_data-url ).

        "Retrieve list of branches for repo
        IF  ls_response_data-access_mode = 'PUBLIC'  OR
          ( ls_response_data-access_mode = 'PRIVATE' AND
            ls_request_data-user         IS NOT INITIAL AND
            ls_request_data-password     IS NOT INITIAL ).
          DATA(lo_branch_list) = zcl_abapgit_git_transport=>branches( ls_request_data-url ).
          DATA(lt_branches_input) = lo_branch_list->get_branches_only( ).
          APPEND LINES OF lt_branches_input TO ls_response_data-branches.
        ENDIF.

        "Prepare Response
        response->set_body_data( content_handler = lo_response_content_handler data = ls_response_data ).
        response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_abapgit_exception INTO DATA(lx_abapgit_exception).
        cx_adt_rest_abapgit=>raise_with_error( lx_abapgit_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
