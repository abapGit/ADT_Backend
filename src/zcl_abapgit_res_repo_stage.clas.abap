CLASS zcl_abapgit_res_repo_stage DEFINITION PUBLIC INHERITING FROM cl_adt_rest_resource FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_abapgit_file,
        filename    TYPE string,
        path        TYPE string,
        localstate  TYPE char1,
        remotestate TYPE char1,
        atom_links  TYPE if_atom_types=>link_t,
      END OF ty_abapgit_file .
    TYPES:
      tt_abapgit_file TYPE STANDARD TABLE OF ty_abapgit_file WITH KEY filename path .
    TYPES:
      BEGIN OF ty_abapgit_object,
        object_ref TYPE sadt_object_reference,
        wbkey      TYPE string,
        version    TYPE sadt_obj_version,
        files      TYPE tt_abapgit_file,
        atom_links TYPE if_atom_types=>link_t,
      END OF ty_abapgit_object .
    TYPES:
      tt_abapgit_object TYPE STANDARD TABLE OF ty_abapgit_object WITH KEY object_ref-name object_ref-type .
    TYPES:
      BEGIN OF ty_abapgit_staging,
        unstaged_objects TYPE tt_abapgit_object,
        staged_objects   TYPE tt_abapgit_object,
        ignored_objects  TYPE tt_abapgit_object,
        abapgit_comment  TYPE zif_abapgit_definitions=>ty_comment,
        atom_links       TYPE if_atom_types=>link_t,
      END OF ty_abapgit_staging .

    CONSTANTS co_content_type_v1 TYPE string VALUE 'application/abapgit.adt.repo.stage.v1+xml' ##NO_TEXT.
    CONSTANTS co_root_name       TYPE string VALUE 'ABAPGITSTAGING' ##NO_TEXT.
    CONSTANTS co_st_name         TYPE string VALUE 'ZABAPGIT_ST_REPO_STAGE' ##NO_TEXT.

    METHODS get REDEFINITION .

    METHODS get_object_wb_type
      IMPORTING
                iv_obj_name      TYPE sobj_name
                iv_obj_type      TYPE trobjtype
      RETURNING VALUE(rs_wbtype) TYPE wbobjtype.

    METHODS get_object_adt_uri
      IMPORTING
                iv_obj_name       TYPE sobj_name
                is_wbtype         TYPE wbobjtype
      RETURNING VALUE(rv_adt_uri) TYPE string
      RAISING   cx_adt_uri_mapping .

    METHODS get_file_links
      IMPORTING
        !iv_repo_key    TYPE zif_abapgit_persistence=>ty_value
        !iv_filename    TYPE string
      RETURNING
        VALUE(rt_links) TYPE if_atom_types=>link_t.

ENDCLASS.



CLASS zcl_abapgit_res_repo_stage IMPLEMENTATION.


  METHOD get.
    DATA:
      lv_repo_key      TYPE zif_abapgit_persistence=>ty_value,
      lv_username      TYPE string,
      lv_password      TYPE string,
      ls_obj_wbtype    TYPE wbobjtype,
      lo_repo_online   TYPE REF TO zcl_abapgit_repo_online,
      ls_response_data TYPE ty_abapgit_staging,
      ls_object        TYPE ty_abapgit_object,
      ls_object_ref    TYPE sadt_object_reference,
      ls_file          TYPE ty_abapgit_file,
      lt_file          TYPE tt_abapgit_file,
      lo_repo_content  TYPE REF TO zcl_abapgit_repo_content_list,
      lo_http_utility  TYPE REF TO cl_http_utility,
      lo_user          TYPE REF TO zif_abapgit_persist_user.

    FIELD-SYMBOLS:
      <ls_repo_items> TYPE zif_abapgit_definitions=>ty_repo_item,
      <ls_repo_file>  TYPE zif_abapgit_definitions=>ty_repo_file.
    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).
    CREATE OBJECT lo_http_utility.

    lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).
*-------Client encodes password with base64 algorithm
    lv_password = lo_http_utility->decode_base64(
       request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' ) ).

    IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
      zcl_abapgit_default_auth_info=>set_auth_info( iv_user     = lv_username
                                                    iv_password = lv_password ).
    ENDIF.

    " zcl_abapgit_factory=>get_environment( )->set_repo_action( if_abapgit_app_log=>c_action_push ).
    TRY.
        DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).

*-------- Check if a different action is still running
        DATA(ls_repo) = zcl_abapgit_persist_factory=>get_repo( )->read( iv_key = lv_repo_key ).
        lo_repo_online ?= lo_repo.
        lo_repo_online->refresh( ).
        DATA(lv_repo_branch) = lo_repo_online->get_branch_name( ).

*------ Retrieve repository content
        CREATE OBJECT lo_repo_content
          EXPORTING
            io_repo = lo_repo.

        DATA(lt_repo_items) = lo_repo_content->list( iv_path         = '/'
                                                     iv_by_folders   = abap_false
                                                     iv_changes_only = abap_true ).
*------ Process data to output structure
        LOOP AT lt_repo_items ASSIGNING <ls_repo_items>.
*-------- consider only those files which already exist locally
          CLEAR: ls_object, ls_object_ref.

*------ Header DATA
          IF <ls_repo_items>-obj_name IS INITIAL. "non-code and meta files

*---------- handle non-code and meta files
            IF <ls_repo_items>-files IS NOT INITIAL.
              ls_object_ref-name = 'non-code and meta files'. "if the logic is proper move the text to a message class
            ENDIF.
          ELSE.
            ls_object_ref-name = <ls_repo_items>-obj_name.
            ls_object_ref-type = <ls_repo_items>-obj_type.

            IF <ls_repo_items>-obj_type IS NOT INITIAL.
*------------ GET object workbench key
              ls_obj_wbtype   = get_object_wb_type(
                iv_obj_name = <ls_repo_items>-obj_name  iv_obj_type = <ls_repo_items>-obj_type ).
              ls_object-wbkey = cl_wb_object_type=>get_global_id_from_global_type( p_global_type = ls_obj_wbtype ).

*------------ GET adt uri
              ls_object_ref-uri = get_object_adt_uri(
                iv_obj_name = <ls_repo_items>-obj_name  is_wbtype = ls_obj_wbtype ).
            ENDIF.
          ENDIF.
          ls_object-object_ref = ls_object_ref.

*------ File specific DATA
          CLEAR: ls_file, lt_file.
          LOOP AT <ls_repo_items>-files ASSIGNING <ls_repo_file>.
            ls_file-path = <ls_repo_file>-path.
            ls_file-filename = <ls_repo_file>-filename.
            ls_file-remotestate = <ls_repo_file>-rstate.
            ls_file-localstate = <ls_repo_file>-lstate.
            ls_file-atom_links = get_file_links( iv_repo_key = lo_repo_online->get_key( )
              iv_filename = ls_file-filename ).
            INSERT ls_file INTO TABLE lt_file.
          ENDLOOP.
          ls_object-files = lt_file.
          IF <ls_repo_items>-lstate IS NOT INITIAL.
            INSERT ls_object INTO TABLE ls_response_data-unstaged_objects.
          ELSE.
            INSERT ls_object INTO TABLE ls_response_data-ignored_objects.
          ENDIF.
        ENDLOOP.
*------ Author and Committer details
        lo_user = zcl_abapgit_persistence_user=>get_instance( ).

        DATA(lv_user) = lo_user->get_repo_git_user_name( lo_repo_online->get_url( ) ).
        IF lv_user IS INITIAL.
          lv_user  = lo_user->get_default_git_user_name( ).
        ENDIF.
        IF lv_user IS INITIAL.
          " get default from user master record
          lv_user = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_name( ).
        ENDIF.

        DATA(lv_email) = lo_user->get_repo_git_user_email( lo_repo_online->get_url( ) ).
        IF lv_email IS INITIAL.
          lv_email = lo_user->get_default_git_user_email( ).
        ENDIF.
        IF lv_email IS INITIAL.
          " get default from user master record
          lv_email = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_email( ).
        ENDIF.

        ls_response_data-abapgit_comment-author-name     = lv_user.
        ls_response_data-abapgit_comment-author-email    = lv_email.
        ls_response_data-abapgit_comment-committer-name  = lv_user.
        ls_response_data-abapgit_comment-committer-email = lv_email.

*------ Create Response Content Handler
        DATA(lo_response_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
          st_name      = co_st_name
          root_name    = co_root_name
          content_type = co_content_type_v1 ).

*------ Prepare Response
        response->set_body_data( content_handler = lo_response_content_handler data = ls_response_data ).
        response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_abapgit_exception cx_uuid_error zcx_abapgit_not_found INTO DATA(lx_exception).
        ROLLBACK WORK.
        zcx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_file_links.

    CONSTANTS:
      lc_file_rel_fetch_local  TYPE string VALUE 'http://www.sap.com/adt/abapgit/file/relations/fetch/localversion',
      lc_file_rel_fetch_remote TYPE string VALUE 'http://www.sap.com/adt/abapgit/file/relations/fetch/remoteversion',
      lc_root_path             TYPE string VALUE '/sap/bc/adt/abapgit'.

    DATA(lo_atom_util) = cl_adt_atom_utility=>create_instance( ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_file_rel_fetch_local
        href = |{ lc_root_path }/repos/{ iv_repo_key }/files?filename={ iv_filename }&version=local|
        type = |fetch_link|
      CHANGING
        links = rt_links ).

    lo_atom_util->append_link(
      EXPORTING
        rel  = lc_file_rel_fetch_remote
        href = |{ lc_root_path }/repos/{ iv_repo_key }/files?filename={ iv_filename }&version=remote|
        type = |fetch_link|
      CHANGING
        links = rt_links ).

  ENDMETHOD.


  METHOD get_object_adt_uri.

    DATA: lv_name    TYPE seu_objkey,
          lv_type    TYPE wbobjtype,
          lv_request TYPE REF TO cl_wb_request.
    TRY.
        DATA(lv_mapper) = cl_adt_uri_mapper=>get_instance( ).

        lv_name  = CONV #( iv_obj_name ).
        lv_type-objtype_tr = is_wbtype-objtype_tr.
        lv_type-subtype_wb = is_wbtype-subtype_wb.

        TRY.
            CALL METHOD lv_mapper->('IF_ADT_URI_MAPPER~GET_ADT_OBJECT_REF_URI')
              EXPORTING
                name = lv_name
                type = lv_type
              RECEIVING
                uri  = rv_adt_uri.
          CATCH cx_sy_dyn_call_error.
            CREATE OBJECT lv_request
              EXPORTING
                p_global_type = lv_type
                p_object_name = lv_name
                p_operation   = swbm_c_op_display
              EXCEPTIONS
                OTHERS        = 0.
            rv_adt_uri = lv_mapper->if_adt_uri_mapper~map_wb_request_to_objref( lv_request )->ref_data-uri.
        ENDTRY.
      CATCH cx_adt_uri_mapping.
    ENDTRY.

  ENDMETHOD.


  METHOD get_object_wb_type.
    cl_wb_object=>create_from_transport_key(
      EXPORTING
          p_obj_name = CONV trobj_name( iv_obj_name )
          p_object   = iv_obj_type
      RECEIVING
          p_wb_object = DATA(lr_wb_object)
      EXCEPTIONS
          objecttype_not_existing = 1
          OTHERS = 2 ).
    IF lr_wb_object IS NOT INITIAL.
      lr_wb_object->get_global_wb_key( IMPORTING p_object_type = rs_wbtype ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.