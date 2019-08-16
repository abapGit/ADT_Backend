CLASS zcl_abapgit_res_repos_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.

  PROTECTED SECTION.

    METHODS: get_application_title REDEFINITION,
      register_resources    REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_res_repos_app IMPLEMENTATION.


  METHOD get_application_title.
    result = 'abapGit Repositories'(001).
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = '/sap/bc/adt/abapgit' ##NO_TEXT.
  ENDMETHOD.


  METHOD register_resources.

    registry->register_discoverable_resource(
        url             = '/repos'
        accepted_types  = VALUE #(
                                    ( |'application/abapgit.adt.repo.v1+xml'| )
                                    ( |'application/abapgit.adt.repo.v2+xml'| )
                                 ) "zcl_abapgit_res_repos=>co_content_type_repo_v1/2
        handler_class   = 'ZCL_ABAPGIT_RES_REPOS'
        description     = 'Repositories'(002)
        category_scheme = 'http://www.sap.com/adt/categories/abapgit'
        category_term   = 'repos' ) ##NO_TEXT.

    registry->register_discoverable_resource(
        url             = '/externalrepoinfo'
        accepted_types  = VALUE #(
                                    ( |application/abapgit.adt.repo.info.ext.request.v1+xml| )
                                 ) "zcl_abapgit_res_repo_info_ext=>co_content_type_request_v1
        handler_class   = 'ZCL_ABAPGIT_RES_REPO_INFO_EXT'
        description     = 'External Repository Info'(003)
        category_scheme = 'http://www.sap.com/adt/categories/abapgit'
        category_term   = 'externalrepoinfo' ) ##NO_TEXT.

    registry->register_resource(
        template        = '/repos/{key}'
        handler_class   = 'ZCL_ABAPGIT_RES_REPO' ).

    registry->register_resource(
        template        = '/repos/{key}/pull'
        handler_class   = 'ZCL_ABAPGIT_RES_REPO_PULL' ).

  ENDMETHOD.
ENDCLASS.
