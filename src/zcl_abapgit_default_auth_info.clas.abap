CLASS zcl_abapgit_default_auth_info DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS set_auth_info
      IMPORTING
        !iv_user     TYPE string
        !iv_password TYPE string .
    CLASS-METHODS get_passoword
      RETURNING
        VALUE(rv_password) TYPE string .
    CLASS-METHODS get_user
      RETURNING
        VALUE(rv_user) TYPE string .
    CLASS-METHODS refresh .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_password TYPE string .
    CLASS-DATA gv_user TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_DEFAULT_AUTH_INFO IMPLEMENTATION.


  METHOD get_passoword.
    rv_password = gv_password.
  ENDMETHOD.


  METHOD get_user.
    rv_user = gv_user.
  ENDMETHOD.


  METHOD refresh.
    CLEAR: gv_user, gv_password.
  ENDMETHOD.


  METHOD set_auth_info.
    gv_user = iv_user.
    gv_password = iv_password.
  ENDMETHOD.
ENDCLASS.
