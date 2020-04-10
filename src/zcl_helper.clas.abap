class ZCL_HELPER definition
  public
  final
  create public .

public section.

  class-methods READ_TEXT_AND_REPLACE
    importing
      !ID type THEAD-TDID
      !NAME type THEAD-TDNAME
      !OBJECT type THEAD-TDOBJECT
      !TDSPRAS type THEAD-TDSPRAS
      !TEXT_SYMBOL type ZTEXT_SYMBOL_T
    returning
      value(LINES) type TLINE_TAB .
  class-methods GOS_EXTERNAL_LINK
    importing
      !I_OBJECTTYPE type BORIDENT-OBJTYPE
      !I_OBJECTKEY type BORIDENT-OBJKEY
      !I_URL type SOLI-LINE
      !I_URLDES type SOOD1-OBJDES
      !OBJECT_TYPE type SOODK-OBJTP
      !RELTYPE type BRELTYP-RELTYPE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HELPER IMPLEMENTATION.


  METHOD gos_external_link.

    DATA: folder_id TYPE soodk,
          object_id TYPE soodk,
          objhead   TYPE STANDARD TABLE OF soli.

    SELECT SINGLE brelguid FROM srgbtbrel INTO @DATA(brelguid)
           WHERE instid_a = @i_objectkey
           AND   typeid_a = @i_objecttype
           AND   catid_a  = 'BO'
           AND   reltype  = @reltype.

    CHECK sy-subrc <> 0.

* Recupero folder id.
    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
      EXPORTING
        owner                 = CONV soud-usrnam( sy-uname )
        region                = 'B'
      IMPORTING
        folder_id             = folder_id
      EXCEPTIONS
        communication_failure = 1
        owner_not_exist       = 2
        system_failure        = 3
        x_error               = 4.

* Inserisco oggetto
    DATA(objcont) = VALUE soli_tab( ( '&KEY&' && i_url ) ).

    CALL FUNCTION 'SO_OBJECT_INSERT'
      EXPORTING
        folder_id                  = folder_id
        object_type                = CONV soodk-objtp( object_type )
        object_hd_change           = VALUE sood1( objsns = 'O'
                                                  objla  = sy-langu
                                                  objdes = i_urldes )
      IMPORTING
        object_id                  = object_id
      TABLES
        objcont                    = objcont
        objhead                    = objhead
      EXCEPTIONS
        active_user_not_exist      = 1
        communication_failure      = 2
        component_not_available    = 3
        dl_name_exist              = 4
        folder_not_exist           = 5
        folder_no_authorization    = 6
        object_type_not_exist      = 7
        operation_no_authorization = 8
        owner_not_exist            = 9
        parameter_error            = 10
        substitute_not_active      = 11
        substitute_not_defined     = 12
        system_failure             = 13
        x_error                    = 14.

* Creo relazione binaria con l'oggetto
    CALL FUNCTION 'BINARY_RELATION_CREATE'
      EXPORTING
        obj_rolea      = VALUE borident( objkey  = i_objectkey
                                         objtype = i_objecttype )
        obj_roleb      = VALUE borident( objkey  = folder_id-objtp &&
                                                   folder_id-objyr &&
                                                   folder_id-objno &&
                                                   object_id-objtp &&
                                                   object_id-objyr &&
                                                   object_id-objno
                                         objtype = 'MESSAGE')
        relationtype   = reltype
      EXCEPTIONS
        no_model       = 1
        internal_error = 2
        unknown        = 3.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.


  METHOD read_text_and_replace.

    DATA: header TYPE thead.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = id
        language                = tdspras
        name                    = name
        object                  = object
      IMPORTING
        header                  = header
      TABLES
        lines                   = lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc EQ 0.

      IF lines( text_symbol ) > 0.

* Inizializzo i Text Symbols
        CALL FUNCTION 'INIT_TEXTSYMBOL'.

* Dichiaro le variabili dinamiche da sostituire
        LOOP AT text_symbol ASSIGNING FIELD-SYMBOL(<fs_text_symbol>).

          CALL FUNCTION 'SET_TEXTSYMBOL'
            EXPORTING
              header  = header
              name    = <fs_text_symbol>-name
              value   = <fs_text_symbol>-value
              replace = abap_true.

        ENDLOOP.

* Sostituzione delle variabili dichiarate
        CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
          EXPORTING
            header    = header
            endline   = lines( lines )
            startline = 1
          TABLES
            lines     = lines.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
