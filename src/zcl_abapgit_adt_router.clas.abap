CLASS zcl_abapgit_adt_router DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS register_resources
        REDEFINITION .
    METHODS get_application_title
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_ADT_ROUTER IMPLEMENTATION.


  METHOD get_application_title.

    result = 'abapGit'.

  ENDMETHOD.


  METHOD register_resources.

    registry->register_discoverable_resource(
      url             = '/abapgit/repos'
      handler_class   = 'ZCL_ABAPGIT_ADT'
      description     = 'abapGit'
      category_scheme = 'http://www.abapgit.org/adt/categories/abapgit'
      category_term   = 'services' ).

    registry->register_resource(
      template      = |/abapgit/repos/\{{ zcl_abapgit_adt=>c_uri_key }\}|
      handler_class = 'ZCL_ABAPGIT_ADT' ).

    registry->register_resource(
      template      = |/abapgit/repos/\{{ zcl_abapgit_adt=>c_uri_key }\}/status|
      handler_class = 'ZCL_ABAPGIT_ADT' ).

    registry->register_resource(
      template      = |/abapgit/repos/\{{ zcl_abapgit_adt=>c_uri_key }\}/pull|
      handler_class = 'ZCL_ABAPGIT_ADT' ).

    registry->register_resource(
      template      = |/abapgit/repos/\{{ zcl_abapgit_adt=>c_uri_key }\}/pull_checks|
      handler_class = 'ZCL_ABAPGIT_ADT' ).

    registry->register_resource(
      template      = |/abapgit/repos/\{{ zcl_abapgit_adt=>c_uri_key }\}/purge|
      handler_class = 'ZCL_ABAPGIT_ADT' ).

  ENDMETHOD.
ENDCLASS.
