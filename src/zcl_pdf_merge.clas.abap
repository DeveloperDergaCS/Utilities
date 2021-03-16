CLASS zcl_pdf_merge DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_PDF_MERGE
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS sbdst .

    TYPES:
      BEGIN OF pdf,
        xdata TYPE xstring,
      END OF pdf .
    TYPES:
      pdf_t TYPE STANDARD TABLE OF pdf .

    METHODS add_pdf
      IMPORTING
        !iv_pdf TYPE xstring .
    METHODS constructor .
    METHODS get_merged
      EXPORTING
        VALUE(size)   TYPE i
        VALUE(rv_pdf) TYPE xstring .
    CLASS-METHODS go_merge_pdf
      IMPORTING
        VALUE(it_pdf) TYPE pdf_t OPTIONAL
      EXPORTING
        VALUE(ev_pdf) TYPE xstring
        VALUE(size)   TYPE i .
    CLASS-METHODS get_attach
      IMPORTING
        !iv_instid TYPE sibfboriid
        !iv_typeid TYPE sibftypeid
        !iv_catid  TYPE sibfcatid
      EXPORTING
        !et_pdf    TYPE pdf_t .
    CLASS-METHODS print_pdf
      IMPORTING
        VALUE(dest)            TYPE tsp01-rqdest
        VALUE(copies)          TYPE tsp01-rqcopies
        VALUE(immediate_print) TYPE tsp01-rq1dispo
        VALUE(auto_delete)     TYPE tsp01-rq2dispo
        VALUE(pdf_data)        TYPE xstring
        VALUE(pdf_size)        TYPE i .
  PROTECTED SECTION.
*"* protected components of class ZCL_PDF_MERGE
*"* do not include other source files here!!!
  PRIVATE SECTION.

    DATA mv_pdf_merged TYPE xstring .
    DATA:
      mv_runid(10) TYPE c .
    DATA:
      mv_filecount(5) TYPE n .
    DATA mv_path TYPE string .
    DATA:
      mt_tmp_files TYPE TABLE OF string .
    DATA file_size TYPE i .

    METHODS delete_tmp_file
      IMPORTING
        !iv_filename TYPE string .
    METHODS get_tmp_filename
      RETURNING
        VALUE(rv_filename) TYPE string .
    METHODS merge_pdf .
    METHODS read_tmp_file
      IMPORTING
        VALUE(iv_filename) TYPE string
      EXPORTING
        VALUE(rv_pdf)      TYPE xstring
        VALUE(size)        TYPE i .
    METHODS write_tmp_file
      IMPORTING
        !iv_filename TYPE string
        !iv_pdf      TYPE xstring .
    CLASS-METHODS pdf_get_pages
      IMPORTING
        !iv_pdf_data TYPE xstring
      EXPORTING
        !ev_pages    TYPE i .
ENDCLASS.



CLASS ZCL_PDF_MERGE IMPLEMENTATION.


  METHOD add_pdf.

    DATA: lv_filename TYPE string.

    CHECK iv_pdf IS NOT INITIAL.

    lv_filename = get_tmp_filename( ).

    APPEND lv_filename TO mt_tmp_files.

    CALL METHOD write_tmp_file
      EXPORTING
        iv_filename = lv_filename
        iv_pdf      = iv_pdf.

  ENDMETHOD.


  METHOD constructor.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '1'
        object                  = 'ZPDFMERGE'
      IMPORTING
        number                  = mv_runid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        logical_path               = 'ZPDFMERGE'
        file_name                  = 'PDFMERGE'
      IMPORTING
        file_name_with_path        = mv_path
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.

  ENDMETHOD.


  METHOD delete_tmp_file.

    OPEN DATASET iv_filename FOR OUTPUT IN BINARY MODE.

    DELETE DATASET iv_filename.

    CLOSE DATASET iv_filename.

  ENDMETHOD.


  METHOD get_attach.

    CONSTANTS:
      c_bds_classname TYPE sbdst_classname  VALUE 'DEVC_STXD_BITMAP',
      c_bds_classtype TYPE sbdst_classtype  VALUE 'OT',
      c_bds_mimetype  TYPE bds_mimetp       VALUE 'application/octet-stream',
      c_bds_original  TYPE sbdst_doc_var_tg VALUE 'OR'.

    DATA: is_object             TYPE  sibflporb,
          it_relation_options   TYPE  obl_t_relt,
          et_links              TYPE  obl_t_link,
          ls_relation_options   TYPE  obl_s_relt,
          t_objhead             TYPE TABLE OF soli,
          t_objcont             TYPE TABLE OF soli,
          t_context             TYPE TABLE OF solix,
          connect_info          TYPE TABLE OF toav0,
          w_doc_id              TYPE sofolenti1-doc_id,
          w_doc_da              TYPE sofolenti1,
          w_inle                TYPE i,
          ls_pdf                TYPE pdf,
          mv_runid              TYPE char10,
          tdname                TYPE stxbitmaps-tdname,
          i_igs_image_converter TYPE REF TO cl_igs_image_converter,
          blob                  TYPE w3mimetabtype,
          blob_size             TYPE w3param-cont_len,
          blob_type             TYPE w3param-cont_type,
          l_bds_object          TYPE REF TO cl_bds_document_set,
          wf_res                TYPE stxbitmaps-resolution,
          l_width_tw            TYPE stxbitmaps-widthtw,
          l_height_tw           TYPE stxbitmaps-heighttw,
          l_width_pix           TYPE stxbitmaps-widthpix,
          l_height_pix          TYPE stxbitmaps-heightpix,
          l_bds_content         TYPE sbdst_content,
          l_bds_components      TYPE sbdst_components,
          l_bds_component       TYPE bapicompon,
          l_bds_signature       TYPE sbdst_signature,
          ls_bds_signature      TYPE bapisignat,
          l_object_key          TYPE sbdst_object_key,
          wa_stxbitmaps         TYPE stxbitmaps,
          l_bds_properties      TYPE sbdst_properties,
          l_bds_propertie       TYPE bapiproper,
          int_command           TYPE TABLE OF tline,
          wa_command            TYPE tline,
          wa_options            TYPE itcpo,
          wa_thead              TYPE thead,
          int_otf               TYPE TABLE OF itcoo,
          int_pdf               TYPE TABLE OF tline,
          l_bds_bytecount       TYPE i,
          l_bytecount           TYPE i,
          lv_error              TYPE string,
          pdf_size              TYPE i.


    FIELD-SYMBOLS: <links>       TYPE obl_s_link,
                   <bitmap_file> TYPE STANDARD TABLE.

    is_object-instid = iv_instid.
    is_object-typeid = iv_typeid.
    is_object-catid  = iv_catid.

    ls_relation_options-sign   = 'I'.
    ls_relation_options-option = 'EQ'.
    ls_relation_options-low    = 'ATTA'.

    APPEND ls_relation_options TO it_relation_options.

    TRY.

        CALL METHOD cl_binary_relation=>read_links_of_binrels
          EXPORTING
            is_object           = is_object
            it_relation_options = it_relation_options
            ip_role             = 'GOSAPPLOBJ'
            ip_no_buffer        = 'X'
          IMPORTING
            et_links            = et_links.

        LOOP AT et_links ASSIGNING <links>.

          CLEAR:   t_objcont, t_objhead, t_context.
          REFRESH: t_objcont, t_objhead, t_context.

          MOVE <links>-instid_b TO w_doc_id.

* Lettura del singolo allegato.
          CALL FUNCTION 'SO_DOCUMENT_READ_API1'
            EXPORTING
              document_id                = w_doc_id
            IMPORTING
              document_data              = w_doc_da
            TABLES
              object_header              = t_objhead
              object_content             = t_objcont
              contents_hex               = t_context
            EXCEPTIONS
              document_id_not_exist      = 1
              operation_no_authorization = 2
              x_error                    = 3
              OTHERS                     = 4.

* Conversione del PDF in formato Xstring.
          MOVE w_doc_da-doc_size TO w_inle.


          CASE w_doc_da-obj_type.

            WHEN 'PDF'.

              CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
                EXPORTING
                  input_length = w_inle
                IMPORTING
                  buffer       = ls_pdf-xdata
                TABLES
                  binary_tab   = t_context
                EXCEPTIONS
                  failed       = 1
                  OTHERS       = 2.

            WHEN 'JPG' OR 'BMP' OR space.

* Stacco il numeratore creato per le BITMAP
              CLEAR: mv_runid.

              CALL FUNCTION 'NUMBER_GET_NEXT'
                EXPORTING
                  nr_range_nr             = '1'
                  object                  = 'ZBITMAP'
                IMPORTING
                  number                  = mv_runid
                EXCEPTIONS
                  interval_not_found      = 1
                  number_range_not_intern = 2
                  object_not_found        = 3
                  quantity_is_0           = 4
                  quantity_is_not_1       = 5
                  interval_overflow       = 6
                  buffer_overflow         = 7
                  OTHERS                  = 8.

              CONCATENATE 'Z' mv_runid INTO tdname.

* Controllo se l'immagine se è di tipo jpeg perchè da questo formato non posso creare
* un grafico nel Business Document Service visto che è obbligatorio che l'immagine
* abbia un formato di tipo bitmap.
              IF w_doc_da-obj_type = 'JPG' OR
                 w_doc_da-obj_type = space.

*...se è di tipo jpeg effettuo la conversione in bitmap
                CREATE OBJECT i_igs_image_converter .

                IF w_doc_da-obj_type = 'JPG'.
                  i_igs_image_converter->input  = 'image/jpeg'.
                ELSEIF w_doc_da-obj_type = space.
                  i_igs_image_converter->input = 'image/png'.
                ENDIF.

                i_igs_image_converter->output = 'image/x-ms-bmp'.

                blob_size = w_doc_da-doc_size.

                CALL METHOD i_igs_image_converter->set_image
                  EXPORTING
                    blob      = t_context
                    blob_size = blob_size.

                CALL METHOD i_igs_image_converter->execute
                  EXCEPTIONS
                    communication_error = 1
                    internal_error      = 2
                    external_error      = 3
                    OTHERS              = 4.

                IF sy-subrc = 0.

                  CLEAR blob_size.
                  CALL METHOD i_igs_image_converter->get_image
                    IMPORTING
                      blob      = blob
                      blob_size = blob_size
                      blob_type = blob_type.

                  ASSIGN blob TO <bitmap_file>.

                ELSE.

                  CALL METHOD i_igs_image_converter->get_error
                    IMPORTING
                      message = lv_error.

                ENDIF.

              ELSE.

                ASSIGN t_context TO <bitmap_file>.

              ENDIF.

              CLEAR: l_width_tw,
                     l_height_tw,
                     l_width_pix,
                     l_height_pix,
                     wf_res,
                     l_bds_bytecount.

              REFRESH l_bds_content.

              l_bytecount = blob_size.

* Conversione Bitmap
              CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP_BDS'
                EXPORTING
                  color                    = abap_true
                  format                   = 'BMP'
                  resident                 = space
                  bitmap_bytecount         = l_bytecount
                  compress_bitmap          = space
                IMPORTING
                  width_tw                 = l_width_tw
                  height_tw                = l_height_tw
                  width_pix                = l_width_pix
                  height_pix               = l_height_pix
                  dpi                      = wf_res
                  bds_bytecount            = l_bds_bytecount
                TABLES
                  bitmap_file              = <bitmap_file>
                  bitmap_file_bds          = l_bds_content
                EXCEPTIONS
                  format_not_supported     = 1
                  no_bmp_file              = 2
                  bmperr_invalid_format    = 3
                  bmperr_no_colortable     = 4
                  bmperr_unsup_compression = 5
                  bmperr_corrupt_rle_data  = 6
                  OTHERS                   = 7.

              CHECK sy-subrc = 0.

* Salvo la bitmap nel Business Document Service
              CREATE OBJECT l_bds_object.

              l_bds_component-doc_count  = '1'.
              l_bds_component-comp_count = '1'.
              l_bds_component-mimetype   = c_bds_mimetype.
              l_bds_component-comp_size  = l_bds_bytecount.

              APPEND l_bds_component TO l_bds_components.
              CLEAR l_bds_component.

              ls_bds_signature-doc_count  = '1'.

              APPEND ls_bds_signature TO l_bds_signature.
              CLEAR ls_bds_signature.

              CALL METHOD l_bds_object->create_with_table
                EXPORTING
                  classname  = c_bds_classname
                  classtype  = c_bds_classtype
                  components = l_bds_components
                  content    = l_bds_content
                CHANGING
                  signature  = l_bds_signature
                  object_key = l_object_key
                EXCEPTIONS
                  OTHERS     = 1.

              READ TABLE l_bds_signature INTO ls_bds_signature INDEX 1.

              wa_stxbitmaps-tdname     = tdname.
              wa_stxbitmaps-tdobject   = 'GRAPHICS'.
              wa_stxbitmaps-tdid       = 'BMAP'.
              wa_stxbitmaps-tdbtype    = 'BCOL'.
              wa_stxbitmaps-docid      = ls_bds_signature-doc_id.
              wa_stxbitmaps-widthpix   = l_width_pix.
              wa_stxbitmaps-heightpix  = l_height_pix.
              wa_stxbitmaps-widthtw    = l_width_tw.
              wa_stxbitmaps-heighttw   = l_height_tw.
              wa_stxbitmaps-resolution = wf_res.
              wa_stxbitmaps-resident   = space.
              wa_stxbitmaps-autoheight = abap_true.
              wa_stxbitmaps-bmcomp     = space.

              INSERT INTO stxbitmaps VALUES wa_stxbitmaps.

              IF sy-subrc <> 0.

                UPDATE stxbitmaps FROM wa_stxbitmaps.

                IF sy-subrc <> 0.
                  MESSAGE e285(td) WITH tdname 'STXBITMAPS'.
                ENDIF.

              ENDIF.

* Settaggio attributi BITMAP in BDS
              REFRESH: l_bds_properties.

              l_bds_propertie-prop_name  = 'DESCRIPTION'.
              l_bds_propertie-prop_value = 'BMP'.

              APPEND l_bds_propertie TO l_bds_properties.
              CLEAR l_bds_propertie.

              CALL METHOD l_bds_object->change_properties
                EXPORTING
                  classname  = c_bds_classname
                  classtype  = c_bds_classtype
                  object_key = l_object_key
                  doc_id     = ls_bds_signature-doc_id
                  doc_ver_no = '1'
                  doc_var_id = '1'
                CHANGING
                  properties = l_bds_properties
                EXCEPTIONS
                  OTHERS     = 1.

* Recupero OTF
              CLEAR: wa_command.
              REFRESH int_command.

              CALL FUNCTION 'SAPSCRIPT_GENERATE_BMP_COMMAND'
                EXPORTING
                  bm_name    = tdname
                  bm_object  = 'GRAPHICS'
                  bm_id      = 'BMAP'
                  bm_type    = 'BCOL'
                IMPORTING
                  bm_command = wa_command.

              APPEND wa_command TO int_command.
              CLEAR wa_command.

              CLEAR wa_options.
              wa_options-tdgetotf = abap_true.
              wa_options-tddest = 'LOCL'.

              CALL FUNCTION 'PRINT_TEXT'
                EXPORTING
                  dialog  = space
                  header  = wa_thead
                  options = wa_options
                TABLES
                  lines   = int_command
                  otfdata = int_otf.

* Recupero il PDF  dell'immagine.
              CALL FUNCTION 'CONVERT_OTF'
                EXPORTING
                  format                = 'PDF'
                IMPORTING
                  bin_filesize          = pdf_size
                  bin_file              = ls_pdf-xdata
                TABLES
                  otf                   = int_otf
                  lines                 = int_pdf
                EXCEPTIONS
                  err_max_linewidth     = 1
                  err_format            = 2
                  err_conv_not_possible = 3
                  OTHERS                = 4.

* Elimino il grafico bitmatp creato
              CALL FUNCTION 'SAPSCRIPT_DELETE_GRAPHIC_BDS'
                EXPORTING
                  i_object       = 'GRAPHICS'
                  i_name         = tdname
                  i_id           = 'BMAP'
                  i_btype        = 'BCOL'
                  dialog         = space
                EXCEPTIONS
                  enqueue_failed = 1
                  delete_failed  = 2
                  not_found      = 3
                  canceled       = 4
                  OTHERS         = 5.

          ENDCASE.

          APPEND ls_pdf TO et_pdf.

        ENDLOOP.

      CATCH cx_obl_parameter_error .
      CATCH cx_obl_internal_error .
      CATCH cx_obl_model_error .

    ENDTRY.

  ENDMETHOD.


  METHOD get_merged.

    IF lines( mt_tmp_files ) NE 0.
      merge_pdf( ).
    ENDIF.

    rv_pdf = mv_pdf_merged.
    size   = file_size.

  ENDMETHOD.


  METHOD get_tmp_filename.

    ADD 1 TO mv_filecount.

    CONCATENATE mv_path mv_runid '_' mv_filecount '.pdf' INTO rv_filename.

*    APPEND rv_filename TO mt_tmp_files.

  ENDMETHOD.


  METHOD go_merge_pdf.

    DATA: lo_pdfmerge TYPE REF TO zcl_pdf_merge.

    FIELD-SYMBOLS: <pdf> TYPE pdf.

* Instanzio la classe per la gestione del merge
    CREATE OBJECT lo_pdfmerge.

* Creazione dei file PDF su una cartella di appoggio sul server
* che rappresenteranno le singole pagine del nuovo PDF
    LOOP AT it_pdf ASSIGNING <pdf>.
      lo_pdfmerge->add_pdf( <pdf>-xdata ).
    ENDLOOP.

* Procedo al merge dei PDF creati precedentemente e alla cancellazione
* dei file pdf temporanei creati precedentemente
    CLEAR: ev_pdf,
           size.

    lo_pdfmerge->get_merged( IMPORTING  rv_pdf = ev_pdf
                                        size   = size ).

    FREE lo_pdfmerge.

  ENDMETHOD.


  METHOD merge_pdf.

    DATA: lv_merged_path      TYPE string,
          lv_merged_path_app  TYPE string,
          lv_param_app        TYPE string,
          lv_param_output     TYPE string,
          lv_param            TYPE btcxpgpar,
          lv_tmp_files        TYPE string,
          merged_path         TYPE i,
          param               TYPE i,
          sum                 TYPE i,
          lv_appo             TYPE string,
          lv_line             TYPE i,
          lv_status           TYPE btcxpgstat,
          lv_exitcode         TYPE btcxpgexit,
          lv_tabix            TYPE sy-tabix VALUE 1,
          lt_tmp_files_merged TYPE TABLE OF string.

    DESCRIBE TABLE mt_tmp_files LINES lv_line.

* Gestione di creazione di più file di merge se la stringa del comando supera
* i 255 Caratteri
    DO.

      IF lv_tabix > lv_line.
        EXIT.
      ENDIF.

* Stacco il nuovo nome del file di merge
      lv_merged_path = get_tmp_filename( ).

* Recupero la lunghezza riservata al comando e al file di merge
* che mi servirà per controllare la lunghezza massima della stringa del comando
      CONCATENATE 'cat output' lv_merged_path INTO lv_param_output SEPARATED BY space.
      merged_path = strlen( lv_param_output ) + 1.

      LOOP AT mt_tmp_files INTO lv_appo FROM lv_tabix.

        lv_tabix = sy-tabix.

* Controllo che non ho già creato un file di merge in precedenza
        IF lv_merged_path_app IS NOT INITIAL.

* Costruisco la stringa con i singoli file + il file di merge precedentemente
* creato
          CONCATENATE lv_merged_path_app lv_param_app lv_appo INTO lv_param_app SEPARATED BY space.
          CLEAR lv_merged_path_app.

        ELSE.

* Costruisco la stringa con i singoli file
          CONCATENATE lv_param_app lv_appo INTO lv_param_app SEPARATED BY space.

        ENDIF.

* Recupero la lunghezza della stringa delle stringa con l'elenco dei file
        param = strlen( lv_param_app ).
* la sommo a quella riservata per il file di merge
        sum   = merged_path + param.

* Controllo che la stringa non superi i 255 carattari
        IF sum > 255.

* Se supera passo alla creazione del file di merge
          CLEAR lv_param_app.
          EXIT.

        ELSE.

* Se non supera continuo con la costruzione della stringa della lista dei file singoli
          lv_tabix = sy-tabix + 1.
          lv_param = lv_param_app.

        ENDIF.

      ENDLOOP.

* Costruzione del comando PDFtkServer
      CONCATENATE lv_param 'cat output' lv_merged_path INTO lv_param SEPARATED BY space.

* Mantengo memorizzato il file di merge appena creato nel caso in cui ne dovessimo
* creare altri
      lv_merged_path_app = lv_merged_path.

* Aggiungo alla lista dei file risultanti dal merge
      APPEND lv_merged_path TO lt_tmp_files_merged.

* Eseguo il comando Esterno di PDFtkServer
      CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
        EXPORTING
          commandname                   = 'ZPDFMERGE'
          additional_parameters         = lv_param
        IMPORTING
          status                        = lv_status
          exitcode                      = lv_exitcode
        EXCEPTIONS
          no_permission                 = 1
          command_not_found             = 2
          parameters_too_long           = 3
          security_risk                 = 4
          wrong_check_call_interface    = 5
          program_start_error           = 6
          program_termination_error     = 7
          x_error                       = 8
          parameter_expected            = 9
          too_many_parameters           = 10
          illegal_command               = 11
          wrong_asynchronous_parameters = 12
          cant_enq_tbtco_entry          = 13
          jobcount_generation_error     = 14
          OTHERS                        = 15.

    ENDDO.

* Leggo la xstring del nuovo file
    read_tmp_file( EXPORTING iv_filename = lv_merged_path
                   IMPORTING rv_pdf      = mv_pdf_merged
                             size        = file_size ).

* Elimino tutti i file temporanei
    LOOP AT lt_tmp_files_merged INTO lv_tmp_files.
      delete_tmp_file( lv_tmp_files ).
    ENDLOOP.

    LOOP AT mt_tmp_files INTO lv_tmp_files.
      delete_tmp_file( lv_tmp_files ).
    ENDLOOP.

  ENDMETHOD.


  METHOD pdf_get_pages.

    DATA: lv_cont  TYPE string,
          lv_lines TYPE i,
          lv_pages TYPE numc5,
          lv_temp  TYPE string.

    DATA: lt_result TYPE match_result_tab.

    FIELD-SYMBOLS: <fs_result> LIKE LINE OF lt_result,
                   <fs_subm>   LIKE LINE OF <fs_result>-submatches.

    ev_pages = 0.

    CALL 'ICT_DISPATCH' ID 'did'    FIELD 'append_xstring_to_string'
                        ID 'source' FIELD iv_pdf_data
                        ID 'dest'   FIELD lv_cont.

    FIND REGEX `/N (.{1,5})/` IN lv_cont IGNORING CASE RESULTS lt_result.

    IF sy-subrc NE 0.
      FIND ALL OCCURRENCES OF REGEX `/count (.{1,4})/` IN lv_cont IGNORING CASE RESULTS lt_result.
    ENDIF.

    lv_lines = lines( lt_result ).

    IF lv_lines IS NOT INITIAL.

      READ TABLE lt_result ASSIGNING <fs_result> INDEX lv_lines.

      IF sy-subrc EQ 0.

        READ TABLE <fs_result>-submatches ASSIGNING <fs_subm> INDEX 1.

        IF sy-subrc EQ 0.

          lv_temp = lv_cont+<fs_subm>-offset(<fs_subm>-length).
          CONDENSE lv_temp NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lv_temp
            IMPORTING
              output = lv_pages.

          ev_pages = lv_pages.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD print_pdf.

    DATA: lv_handle     TYPE sy-tabix,
          lv_spoolid    TYPE rspoid,
          lv_partname   TYPE adspart,
          lv_globaldir  TYPE text1024,
          lv_dstfile    TYPE text1024,
          lv_filesize   TYPE i,
          lv_pages      TYPE i,
          doctype       TYPE adsdoctype VALUE 'ADSP',
          spool_pdf(40).

    CALL FUNCTION 'ADS_SR_OPEN'
      EXPORTING
        dest            = dest
        doctype         = doctype
        copies          = copies
        immediate_print = immediate_print
        auto_delete     = auto_delete
      IMPORTING
        handle          = lv_handle
        spoolid         = lv_spoolid
        partname        = lv_partname
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc NE 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    pdf_get_pages( EXPORTING iv_pdf_data = pdf_data
                   IMPORTING ev_pages    = lv_pages ).

    CONCATENATE lv_partname '.pdf' INTO spool_pdf.

    CALL FUNCTION 'ADS_WRITE_TO_FILE'
      EXPORTING
        filename                     = spool_pdf
        buffer                       = pdf_data
      EXCEPTIONS
        cannot_open_file             = 1
        open_dataset_no_authority    = 2
        open_dataset_internal_error  = 3
        open_dataset_too_many_files  = 4
        dataset_cant_close           = 5
        close_dataset_internal_error = 6
        cannot_close_file            = 7
        cannot_transfer_data         = 8
        transfer_internal_error      = 9
        dataset_write_error          = 10
        OTHERS                       = 11.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    CALL FUNCTION 'ADS_SR_CONFIRM'
      EXPORTING
        handle   = lv_handle
        partname = lv_partname
        size     = pdf_size
        pages    = lv_pages
        no_pdf   = ' '
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc NE 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

    CALL FUNCTION 'ADS_SR_CLOSE'
      EXPORTING
        handle = lv_handle
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDMETHOD.


  METHOD read_tmp_file.

    CHECK iv_filename IS NOT INITIAL.

    OPEN DATASET iv_filename FOR INPUT IN BINARY MODE.

    READ DATASET iv_filename INTO rv_pdf LENGTH size.

    CLOSE DATASET iv_filename.

  ENDMETHOD.


  METHOD write_tmp_file.

    OPEN DATASET iv_filename FOR OUTPUT IN BINARY MODE.

    TRANSFER iv_pdf TO iv_filename.

    CLOSE DATASET iv_filename.

  ENDMETHOD.
ENDCLASS.
