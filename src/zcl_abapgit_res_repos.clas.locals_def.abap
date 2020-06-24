*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_abapgit_provider.
  METHODS:
    validate_package IMPORTING iv_package TYPE devclass
                     RAISING   zcx_abapgit_exception,
    is_tr_check_required IMPORTING iv_package TYPE devclass
                         RETURNING VALUE(rv_is_required) TYPE abap_bool
                         RAISING   zcx_abapgit_exception,
    list_repositories RETURNING VALUE(rt_list) TYPE zif_abapgit_definitions=>ty_repo_ref_tt
                      RAISING   zcx_abapgit_exception,
    validate_transport_request IMPORTING iv_transport_request TYPE trkorr
                               RAISING   zcx_abapgit_exception,
    set_authentication_info IMPORTING iv_user     TYPE string
                                      iv_password TYPE string,
    perform_import IMPORTING is_request_data TYPE zcl_abapgit_res_repos=>ty_request_data
                   RAISING   zcx_abapgit_exception.
ENDINTERFACE.
CLASS lcl_abapgit_provider DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_abapgit_provider.
ENDCLASS.
CLASS ltcl_abapgit_repos_resource DEFINITION DEFERRED.
CLASS zcl_abapgit_res_repos DEFINITION LOCAL FRIENDS ltcl_abapgit_repos_resource.