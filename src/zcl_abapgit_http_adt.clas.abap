CLASS zcl_abapgit_http_adt DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_http
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS determine_access_level
      IMPORTING
        !iv_url               TYPE string
      RETURNING
        VALUE(rv_repo_access) TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HTTP_ADT IMPLEMENTATION.


  METHOD determine_access_level.

* todo,

    DATA: lv_uri                 TYPE string,
          lv_scheme              TYPE string,
          li_client              TYPE REF TO if_http_client,
          lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config,
          lv_text                TYPE string,
          lo_client              TYPE REF TO zcl_abapgit_http_client,
          lv_service             TYPE string VALUE 'upload'.

    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = zcl_abapgit_url=>host( iv_url )
        ssl_id             = 'ANONYM'
        proxy_host         = lo_proxy_configuration->get_proxy_url( iv_url )
        proxy_service      = lo_proxy_configuration->get_proxy_port( iv_url )
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " make sure:
          " a) SSL is setup properly in STRUST
          lv_text = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'.
        WHEN OTHERS.
          lv_text = 'While creating HTTP Client'.           "#EC NOTEXT

      ENDCASE.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF lo_proxy_configuration->get_proxy_authentication( iv_url ) = abap_true.
      zcl_abapgit_proxy_auth=>run( li_client ).
    ENDIF.

    CREATE OBJECT lo_client
      EXPORTING
        ii_client = li_client.

    IF is_local_system( iv_url ) = abap_true.
      li_client->send_sap_logon_ticket( ).
    ENDIF.

    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).                             "#EC NOTEXT
    lv_uri = zcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             lv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    zcl_abapgit_login_manager=>load( iv_uri    = iv_url
                                     ii_client = li_client ).

    zcl_abapgit_exit=>get_instance( )->http_client( ii_client = li_client iv_url = iv_url ).


    lo_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      rv_repo_access = 'PRIVATE'.
    ELSE.
      rv_repo_access = 'PUBLIC'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
