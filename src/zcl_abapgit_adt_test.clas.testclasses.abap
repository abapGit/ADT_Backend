*"* use this source file for your ABAP unit test classes

CLASS ltcl_test DEFINITION INHERITING FROM zcl_abapgit_adt_test FOR TESTING
    RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    METHODS: create FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD create.

    DATA(lv_body) = '<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">' &&
      '<asx:values>' &&
      '<ROOT>' &&
      '<URL>http://goo.com</URL>' &&
      '<BRANCH_NAME>refs/heads/master</BRANCH_NAME>' &&
      '<PACKAGE>$ONLINE</PACKAGE>' &&
      '</ROOT>' &&
      '</asx:values>' &&
      '</asx:abap>'.

    DATA(response) = post(
      iv_uri  = '/sap/bc/adt/abapgit/repos'
      iv_body = lv_body ).

* TODO

  ENDMETHOD.

ENDCLASS.
