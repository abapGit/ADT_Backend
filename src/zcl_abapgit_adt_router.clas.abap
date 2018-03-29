class ZCL_ABAPGIT_ADT_ROUTER definition
  public
  inheriting from CL_ADT_DISC_RES_APP_BASE
  create public .

public section.
protected section.

  methods REGISTER_RESOURCES
    redefinition .
  methods GET_APPLICATION_TITLE
    redefinition .
private section.
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

  ENDMETHOD.
ENDCLASS.
