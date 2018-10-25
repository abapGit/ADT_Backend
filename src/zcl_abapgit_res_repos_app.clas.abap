CLASS zcl_abapgit_res_repos_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_adt_rest_rfc_application~get_static_uri_path
        REDEFINITION .
  PROTECTED SECTION.
    METHODS: get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_RES_REPOS_APP IMPLEMENTATION.


  METHOD get_application_title.
    result = 'abapGit Repositories'.
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = '/sap/bc/adt/abapgit'.
  ENDMETHOD.


  METHOD register_resources.

    DATA lt_accepted_types TYPE if_adt_discovery_collection=>ty_accepts.

    APPEND zcl_abapgit_res_repos=>co_content_type_repo_v1 TO lt_accepted_types.

    registry->register_discoverable_resource(
        url             = '/repos'
        accepted_types = lt_accepted_types
        handler_class   = 'ZCL_ABAPGIT_RES_REPOS'
        description     = 'Repositories'
        category_scheme = 'http://www.sap.com/adt/categories/abapgit'
        category_term   = 'repos'
        ).

    CLEAR lt_accepted_types.
    APPEND zcl_abapgit_res_repo_info_ext=>co_content_type_request_v1 TO lt_accepted_types.

    registry->register_discoverable_resource(
        url             = '/externalrepoinfo'
        accepted_types = lt_accepted_types
        handler_class   = 'ZCL_ABAPGIT_RES_REPO_INFO_EXT'
        description     = 'External Repository Info'
        category_scheme = 'http://www.sap.com/adt/categories/abapgit'
        category_term   = 'externalrepoinfo'
        ).

    registry->register_resource(
        template        = '/repos/{key}'
        handler_class   = 'ZCL_ABAPGIT_RES_REPO'
        ).

    registry->register_resource(
        template        = '/repos/{key}/pull'
        handler_class   = 'ZCL_ABAPGIT_RES_REPO_PULL'
    ).

  ENDMETHOD.
ENDCLASS.
