*&---------------------------------------------------------------------*
*&  Include           ZRFI_INV_OUTST_02_07_F00
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_BUKRS
*&---------------------------------------------------------------------*
*       Check the Bukrs
*----------------------------------------------------------------------*
*-------------------------------------Modification Log---------------------------------------------------*
*||---Technical Name-----|| Functional Name ||---Dev-Id---|| Change Date || TR  Number || Reason for Change
*||Soc by Hiren A.Bhoi   || Bharat gulabvani|| Devid-3990 || 11 -04-2023 || ATDK981563 || bug fix aof col Kam name
*||Soc by Satyen Trivedi || Neeki Govani    || Devid-4884 || 27-03-2024  || ATDK997901 || Value Mismatch Issue
*||Soc by Riya Patel     || Neeki Govani    || Devid-5113 || 03-04-2024  || ATDK9A01SU || Value in AMT Rel & 2 new columns
*||Soc by Jash Rajpara   || Neeki Govani    || Devid-5628 || 01-10-2024  || ATDK9A08RG || Value Mismatch-Doc Type & Export Sales Column Add
*||Soc by Jash Rajpara   || Sainath Kalva   || Devid-     || 25.11.2024  || ATDK9A08RG || clear default system date and change structure for ZFM_SD_DTR_OUTSTDDATA rfc
*||Soc by Jash Rajpara   || Abdul Vayda     || Devid-6981 || 27.04.2025  || ATDK9A0MIX || Change logic of few fields.

FORM SUB_CHECK_BUKRS .
*   Local Data decleration
  DATA: LV_BUKRS TYPE BUKRS.
  SELECT SINGLE
         BUKRS
         INTO LV_BUKRS
         FROM T001
         WHERE BUKRS IN S_BUKRS.
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE 'Please give Valid Plant' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_CDATE
*&---------------------------------------------------------------------*
*       Check for the date
*----------------------------------------------------------------------*
FORM SUB_CHECK_CDATE .
*   Cut of Date should not be before the Posting start date
  IF S_INVDAT-HIGH IS NOT INITIAL AND P_CDATE < S_INVDAT-HIGH.
    MESSAGE 'Cut off date should be GE to Invoice date' TYPE 'E'.
  ELSEIF S_INVDAT-HIGH IS INITIAL AND P_CDATE < S_INVDAT-LOW.
    MESSAGE 'Cut off date should be GE to Invoice date' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_TAB_SEL_FINAL_FILL
*&---------------------------------------------------------------------*
*       Select the data and Fill in Final
*----------------------------------------------------------------------*
FORM SUB_TAB_SEL_FINAL_FILL .
*   Local data decleration
  DATA: LT_BSID         TYPE STANDARD TABLE OF TY_BSID INITIAL SIZE 0,
        LW_BSID         TYPE TY_BSID,
        LW_BSID_PRE     TYPE TY_BSID,
        LT_BSID_SORT    TYPE STANDARD TABLE OF TY_BSID INITIAL SIZE 0, "Unique data based on company code, fiscal year, Document
        LT_BSID_WR      TYPE STANDARD TABLE OF TY_BSID INITIAL SIZE 0,  "With referance
        LT_BSID_NR      TYPE STANDARD TABLE OF TY_BSID INITIAL SIZE 0,  "With out referance
        LT_BSAD         TYPE STANDARD TABLE OF TY_BSID INITIAL SIZE 0,
        LW_BSAD         TYPE TY_BSID,
        LT1_BSID        TYPE STANDARD TABLE OF TY1_BSID INITIAL SIZE 0,
        LW1_BSID        TYPE TY1_BSID,
        LT_ACDOCA       TYPE STANDARD TABLE OF TY_ACDOCA INITIAL SIZE 0,
        LW_ACDOCA       TYPE TY_ACDOCA,
        LT_ACDOCA_SORT  TYPE STANDARD TABLE OF TY_ACDOCA INITIAL SIZE 0,
        LT_CEPCT        TYPE STANDARD TABLE OF TY_PRCTR INITIAL SIZE 0,
        LW_CEPCT        TYPE TY_PRCTR,
        LT_KNA1         TYPE HASHED TABLE OF TY_KNA1 WITH UNIQUE KEY KUNNR INITIAL SIZE 0,
        LW_KNA1         TYPE TY_KNA1,
        LW_FINAL        TYPE TY_FINAL,
        LW_FINAL_PRE    TYPE TY_FINAL,
        LT_VBFA         TYPE STANDARD TABLE OF TY_VBFA INITIAL SIZE 0,
        LT_VBAK         TYPE STANDARD TABLE OF TY_VBAK INITIAL SIZE 0,
        LT_VBAK_SORT    TYPE STANDARD TABLE OF TY_VBAK INITIAL SIZE 0,
        LT_VBKD         TYPE STANDARD TABLE OF TY_VBKD INITIAL SIZE 0,
        LT_TVZBT        TYPE STANDARD TABLE OF TY_TVZBT INITIAL SIZE 0,
        LT_TVV1T        TYPE STANDARD TABLE OF TY_TVV1T INITIAL SIZE 0,
        LT_TVGRT        TYPE STANDARD TABLE OF TY_TVGRT  INITIAL SIZE 0, "Manager Des
        LT_TVKBT        TYPE STANDARD TABLE OF TY_TVKBT  INITIAL SIZE 0, "Manager Des
        LT_VBRK         TYPE STANDARD TABLE OF TY_VBRK INITIAL SIZE 0,  "Where document type is Non RV
        LT_VBRK_RV      TYPE STANDARD TABLE OF TY_VBRK INITIAL SIZE 0, "All document type
        LT_VBRK_RV_SORT TYPE STANDARD TABLE OF TY_VBRK INITIAL SIZE 0,
        LT_VBELN        TYPE STANDARD TABLE OF VBRK-VBELN WITH DEFAULT KEY,
        LT_T052         TYPE STANDARD TABLE OF TY_T052 INITIAL SIZE 0,
        LT_TVZBT_FI     TYPE STANDARD TABLE OF TY_TVZBT INITIAL SIZE 0.
*   Slect the Open delivery till today
  IF ( SY-UNAME EQ 'RFCUSER' ).   """ added by jash rajpara dt: 25.11.2024
    CLEAR: S_INVDAT[].
  ENDIF.
    SELECT a~bukrs a~kunnr a~umsks a~umskz a~augdt a~augbl a~zuonr a~gjahr a~belnr a~buzei a~budat
           a~bldat a~cpudt a~zfbdt a~waers a~xblnr a~blart a~shkzg a~dmbtr a~wrbtr a~zterm a~rebzg a~rebzj
           a~prctr a~zbd3t a~zbd2t a~zbd1t a~sgtxt b~awkey
      INTO TABLE lt_bsid
      FROM bsid AS a
      INNER JOIN bkpf AS b ON b~bukrs = a~bukrs
                          AND b~belnr = a~belnr
                          AND b~gjahr = a~gjahr
      WHERE a~bukrs IN s_bukrs
        AND a~kunnr IN s_kunnr
        AND a~blart IN ( 'RV','DA','DR','DS','DL','DG', 'UE', 'AB', 'DZ','JV','SA','SB', 'DB', 'JM' ) " SB : SOC : 4453   "" JM DB SOC: 5628
        AND a~budat IN s_invdat
        AND a~dmbtr NE '0.00'
        AND a~xstov <> 'X'.

  IF LT_BSID IS NOT INITIAL.
*    SORT LT_BSID BY PRCTR.
*    DELETE LT_BSID WHERE PRCTR NOT IN S_PRCTR[].
    SELECT BUKRS KUNNR UMSKS UMSKZ AUGDT AUGBL ZUONR GJAHR BELNR BUZEI
           DMBTR REBZG REBZJ
      INTO TABLE LT1_BSID
      FROM BSID
      FOR ALL ENTRIES IN LT_BSID
      WHERE BUKRS IN S_BUKRS
      AND   ZUONR = LT_BSID-ZUONR
      AND BLART  <> 'RV'
      AND REBZG = LT_BSID-BELNR
      AND REBZJ = LT_BSID-GJAHR
      AND   BUDAT <= P_CDATE.
  ENDIF.

*     Bsad Select the data that is open on cutoff date
  SELECT a~bukrs a~kunnr a~umsks a~umskz a~augdt a~augbl a~zuonr a~gjahr a~belnr a~buzei a~budat
         a~bldat a~cpudt a~zfbdt a~waers a~xblnr a~blart a~shkzg a~dmbtr a~wrbtr a~zterm a~rebzg a~rebzj
         a~prctr a~zbd3t a~zbd2t a~zbd1t b~awkey
         APPENDING TABLE lt_bsid
    FROM bsad AS a
    INNER JOIN bkpf AS b ON b~bukrs = a~bukrs
                        AND b~belnr = a~belnr
                        AND b~gjahr = a~gjahr
    WHERE a~bukrs IN s_bukrs
      AND a~kunnr IN s_kunnr
      AND a~blart IN ( 'RV','DA','DR','DS','DL','DG', 'UE', 'AB', 'DZ','JV','SA','SB', 'DB', 'JM' ) " SB : SOC : 4453 "" JM DB SOC: 5628
      AND a~augdt > p_cdate  "Document that was cleared after this date
      AND a~prctr IN s_prctr.

  IF LT_BSID IS NOT INITIAL.
*   Fetching the data for due date calculation FI document Non RV
    LT_BSID_SORT[] = LT_BSID[].
    SORT LT_BSID_SORT BY ZTERM.
    DELETE ADJACENT DUPLICATES FROM LT_BSID_SORT COMPARING ZTERM.
    IF LT_BSID_SORT IS NOT INITIAL.
*         Populating the short term des
      SELECT ZTERM VTEXT
        INTO TABLE LT_TVZBT_FI
        FROM TVZBT
        FOR ALL ENTRIES IN LT_BSID_SORT
        WHERE ZTERM = LT_BSID_SORT-ZTERM.

      SELECT ZTERM ZTAGG ZTAG1 ZTAG2 ZTAG3
      FROM T052
      INTO TABLE LT_T052
      FOR ALL ENTRIES IN LT_BSID_SORT
      WHERE ZTERM = LT_BSID_SORT-ZTERM.
      IF lt_vbrk_rv IS NOT INITIAL.
      ENDIF.
    ENDIF.
*      Build list of billing documents
    lt_vbeln = VALUE #( FOR ls_bsid IN lt_bsid ( ls_bsid-awkey(10) ) ).
    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.
    IF lt_vbeln IS NOT INITIAL.
*       Payment Term
      SELECT vbeln zterm vtweg spart vkorg
        FROM vbrk
        INTO TABLE lt_vbrk_rv
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line.

*********************************** SOC : 5113 ******************************
      BREAK ABAP03.
      DATA : LV_PARVW TYPE VBPA-PARVW.
      CLEAR : LV_PARVW.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          INPUT  = 'CA'
        IMPORTING
          OUTPUT = LV_PARVW.
        SELECT a~vbeln a~parvw a~lifnr b~name1 b~sortl
          FROM vbpa AS a
            INNER JOIN lfa1 AS b
              ON a~lifnr = b~lifnr
                INTO TABLE it_vbpa
                FOR ALL ENTRIES IN lt_vbeln
                  WHERE a~vbeln = lt_vbeln-table_line
                    AND a~parvw = lv_parvw.
      IF IT_VBPA IS NOT INITIAL.
        SORT IT_VBPA BY VBELN.
      ENDIF.
      CLEAR : LV_PARVW.
*********************************** EOC : 5113 ******************************

      IF SY-SUBRC = 0.
        LT_VBRK_RV_SORT = LT_VBRK_RV.
        SORT  LT_VBRK_RV_SORT BY ZTERM.
        DELETE ADJACENT DUPLICATES FROM LT_VBRK_RV_SORT COMPARING ZTERM.
        IF LT_VBRK_RV_SORT IS NOT INITIAL.
          SELECT ZTERM SPRAS VTEXT
          INTO TABLE LT_TVZBT
          FROM TVZBT
          FOR ALL ENTRIES IN LT_VBRK_RV_SORT
          WHERE ZTERM = LT_VBRK_RV_SORT-ZTERM
          AND SPRAS = 'EN'.
        ENDIF.
        CLEAR: LT_VBRK_RV_SORT.
      ENDIF.
    *    Selection of VBFA
      SELECT vbelv posnv vbeln vbtyp_v
        INTO TABLE lt_vbfa
        FROM vbfa
        FOR ALL ENTRIES IN lt_vbeln           "LT_BSID_SORT
        WHERE vbeln = lt_vbeln-table_line    "LT_BSID_SORT-XBLNR+0(10)
        AND   vbtyp_v = 'C'.
      IF sy-subrc = 0.
        SELECT VBELN KVGR1 VKGRP VKBUR ZLOCN
        INTO TABLE LT_VBAK
        FROM VBAK
        FOR ALL ENTRIES IN LT_VBFA
        WHERE VBELN = LT_VBFA-VBELV.
        IF SY-SUBRC = 0.
          LT_VBAK_SORT = LT_VBAK.
          SORT LT_VBAK_SORT BY KVGR1.
          DELETE ADJACENT DUPLICATES FROM LT_VBAK_SORT COMPARING KVGR1.
          IF LT_VBAK_SORT IS NOT INITIAL.
            SELECT  KVGR1 BEZEI
            INTO TABLE LT_TVV1T
            FROM TVV1T
            FOR ALL ENTRIES IN LT_VBAK_SORT
            WHERE SPRAS = SY-LANGU
             AND KVGR1 = LT_VBAK_SORT-KVGR1.
          ENDIF.
*           Des for Manager
          LT_VBAK_SORT = LT_VBAK.
          SORT LT_VBAK_SORT BY VKGRP.
          DELETE ADJACENT DUPLICATES FROM LT_VBAK_SORT COMPARING VKGRP.
          "Added By Hiren bhoi on 11-04-2023 Devid-3990
*          IF LT_VBAK_SORT IS NOT INITIAL.
          IF LT_VBAK IS NOT INITIAL.
            SELECT VKBUR BEZEI
               INTO TABLE LT_TVKBT
            FROM TVKBT
            FOR ALL ENTRIES IN LT_VBAK "LT_VBAK_SORT
            WHERE SPRAS = SY-LANGU
              AND VKBUR = LT_VBAK-VKBUR. "LT_VBAK_SORT-VKBUR.
          ENDIF. " LT_VBAK IS NOT INITIAL.
          "End Of Hiren Devid-3990

          IF LT_VBAK_SORT IS NOT INITIAL.
            SELECT  VKGRP BEZEI
            INTO TABLE LT_TVGRT
            FROM TVGRT
            FOR ALL ENTRIES IN LT_VBAK_SORT
            WHERE SPRAS = SY-LANGU
              AND VKGRP = LT_VBAK_SORT-VKGRP.
          ENDIF.
        ENDIF.   "LT_VBAK
        SELECT VBELN POSNR BZIRK
        INTO TABLE LT_VBKD
        FROM VBKD
        FOR ALL ENTRIES IN LT_VBFA
        WHERE VBELN = LT_VBFA-VBELV.
        IF SY-SUBRC = 0.
        ENDIF.
      ENDIF.
    ENDIF.  "For BLART RV
    SELECT KUNNR LAND1 NAME1 NAME2
    INTO TABLE LT_KNA1
    FROM KNA1
    FOR ALL ENTRIES IN LT_BSID
     WHERE KUNNR = LT_BSID-KUNNR.

*     ACDOCA for Profit center
    SELECT RBUKRS GJAHR BELNR BUZEI PRCTR RACCT BLDAT BSCHL REBZG "Added rebzg 4884
    INTO TABLE LT_ACDOCA
    FROM ACDOCA
    FOR ALL ENTRIES IN LT_BSID
    WHERE RBUKRS = LT_BSID-BUKRS
     AND  GJAHR = LT_BSID-GJAHR
     AND  BELNR = LT_BSID-BELNR
     AND  BUZEI = LT_BSID-BUZEI
     AND  PRCTR IN S_PRCTR
     AND RACCT IN S_RACCT. "Added by pratik 8.1.19
  ENDIF.

*   populating the LT_FINAL
  SORT LT_BSID BY BUKRS GJAHR BELNR BUZEI.
  LT_BSID_SORT[] = LT_BSID[].
  DELETE ADJACENT DUPLICATES FROM LT_BSID_SORT COMPARING BUKRS GJAHR BELNR BUZEI.
*  Buliding a two seprate table, one for where referance field is emplty and other where
*  referance field is does not emplty.
*  SORT lt_bsid BY rebzg. " Not required..missing binary search
  LT_BSID_WR[] = LT_BSID[].
  DELETE LT_BSID_WR WHERE REBZG IS INITIAL.   "With out REBZG
  SORT LT_BSID_WR BY BUKRS  REBZJ REBZG.

  LT_BSID_NR[] = LT_BSID[].
*  DELETE LT_BSID_NR WHERE REBZG IS NOT INITIAL AND REBZG NE 'V'.
  SORT LT_BSID_NR BY BUKRS GJAHR  BELNR BUZEI.

*   Bulding a ACDOCA table with unique entry
  SORT LT_ACDOCA BY RBUKRS GJAHR BELNR BUZEI.
  IF LT_ACDOCA IS NOT INITIAL.
    "Fetch Profit center description
    SELECT PRCTR, KTEXT ,LTEXT
      FROM CEPCT
      INTO TABLE @LT_CEPCT
      FOR ALL ENTRIES IN @LT_ACDOCA
      WHERE PRCTR EQ @LT_ACDOCA-PRCTR AND
            SPRAS = 'E'.
    SORT LT_CEPCT BY PRCTR ASCENDING..

  ENDIF. " lt_acdoca is NOT INITIAL.
  LT_ACDOCA_SORT[] = LT_ACDOCA[].
  DELETE ADJACENT DUPLICATES FROM LT_ACDOCA_SORT COMPARING RBUKRS GJAHR BELNR BUZEI.

  IF S_PRCTR IS INITIAL.
    IF LT_BSID_SORT IS NOT INITIAL.


      SELECT * FROM BSE_CLR INTO TABLE GT_BSE FOR ALL ENTRIES IN LT_BSID_SORT WHERE BUKRS = LT_BSID_SORT-BUKRS
                                                                                  AND BELNR = LT_BSID_SORT-BELNR
                                                                                  AND GJAHR = LT_BSID_SORT-GJAHR
                                                                                  AND BUZEI = LT_BSID_SORT-BUZEI.
      IF GT_BSE IS NOT INITIAL.

****************************************** SOC : 5113 **********************************************
        BREAK ABAP03.
        REFRESH : GT_BSE_CLRR[].
        GT_BSE_CLRR[] = GT_BSE[].
        SORT GT_BSE_CLRR BY CLRIN.
        DELETE GT_BSE_CLRR WHERE CLRIN NE '2'.
        IF GT_BSE_CLRR IS NOT INITIAL.
          SORT GT_BSE_CLRR BY BUKRS BELNR GJAHR.
          SELECT BUKRS BELNR GJAHR REBZG DMBTR WRBTR    "" wrbtr soc: 5628
            FROM BSID
              INTO TABLE IT_BSIDR
                FOR ALL ENTRIES IN GT_BSE_CLRR
                  WHERE BUKRS = GT_BSE_CLRR-BUKRS_CLR
                    AND BELNR = GT_BSE_CLRR-BELNR_CLR
                    AND GJAHR = GT_BSE_CLRR-GJAHR_CLR
                    AND REBZG = GT_BSE_CLRR-BELNR.
          IF IT_BSIDR IS NOT INITIAL.
            SORT IT_BSIDR BY BUKRS BELNR GJAHR REBZG.
          ENDIF.
        ENDIF.
****************************************** EOC : 5113 **********************************************

        ""Added rebzg rebzj augdt 4884
        SELECT RBUKRS BELNR GJAHR BUDAT XREVERSED REBZG REBZJ AUGDT FROM ACDOCA INTO TABLE GT_BKPF
                                                     FOR ALL ENTRIES IN GT_BSE
                                                     WHERE RBUKRS = GT_BSE-BUKRS_CLR
                                                     AND BELNR = GT_BSE-BELNR_CLR
                                                     AND GJAHR = GT_BSE-GJAHR_CLR.

*                                                     AND BUDAT IN S_INVDAT.
                                                            "BOC 4884
        IF GT_BKPF[] IS NOT INITIAL.
          GT_BKPFN[] = GT_BKPF[].
          DELETE GT_BKPFN WHERE REBZG IS INITIAL.
          IF GT_BKPFN[] IS NOT INITIAL.
            SELECT BUKRS ,  BELNR , GJAHR , BLART
              FROM BKPF
              FOR ALL ENTRIES IN @GT_BKPFN
              WHERE BUKRS = @GT_BKPFN-BUKRS
              AND   BELNR = @GT_BKPFN-REBZG
              AND   GJAHR = @GT_BKPFN-REBZJ
              AND   BLART = 'AB'
              INTO TABLE @GT_BKPFN1.
            SORT : GT_BKPFN1 BY BUKRS   BELNR GJAHR  BLART.
          ENDIF.
        ENDIF.
                                                            "EOC 4884
      ENDIF.
    ENDIF.
    GT_BKPFN[] = GT_BKPF[].
    SORT : GT_BKPFN BY BUKRS BELNR GJAHR REBZG.
*    DELETE gt_bkpfn WHERE rebzg = ' '.
*    DELETE gt_bkpfn WHERE augdt IS NOT INITIAL .
    LOOP AT LT_BSID_SORT INTO LW_BSID.
      PERFORM SUB_FINAL_TABLE_POPULATION USING LW_BSID
                                               LT_KNA1
                                               LT_ACDOCA
                                               LT_CEPCT
                                               LT_BSID_WR
                                               LT_BSID_NR
                                               LT_VBFA
                                               LT_VBAK
                                               LT_VBKD
                                               LT_TVZBT
                                               LT_TVZBT_FI
                                               LT_VBRK_RV
                                               LT_TVV1T
                                               LT_TVGRT
                                               IT_VBPA " SOC : 5113
                                               LT_T052
                                               LT1_BSID
                                               LT_TVKBT
                                         CHANGING LW_FINAL
                                                  GT_FINAL
                                                  LW_BSID_PRE.
    ENDLOOP.

  ELSE.
    IF LT_ACDOCA_SORT IS NOT INITIAL.
      SELECT * FROM BSE_CLR INTO TABLE GT_BSE FOR ALL ENTRIES IN LT_ACDOCA_SORT WHERE BUKRS = LT_ACDOCA_SORT-RBUKRS
                                                                                  AND BELNR = LT_ACDOCA_SORT-BELNR
                                                                                  AND GJAHR = LT_ACDOCA_SORT-GJAHR
                                                                                  AND BUZEI = LT_ACDOCA_SORT-BUZEI.

      IF GT_BSE IS NOT INITIAL.

****************************************** SOC : 5113 **********************************************
        BREAK ABAP03.
        REFRESH : GT_BSE_CLRR[].
        GT_BSE_CLRR[] = GT_BSE[].
        SORT GT_BSE_CLRR BY CLRIN.
        DELETE GT_BSE_CLRR WHERE CLRIN NE '2'.
        IF GT_BSE_CLRR IS NOT INITIAL.
          SORT GT_BSE_CLRR BY BUKRS BELNR GJAHR.
          SELECT BUKRS BELNR GJAHR REBZG DMBTR
                 WRBTR      "" added by jash rajpara dev id: 5628 dt: 09.09.24
            FROM BSID
              INTO TABLE IT_BSIDR
                FOR ALL ENTRIES IN GT_BSE_CLRR
                  WHERE BUKRS = GT_BSE_CLRR-BUKRS_CLR
                    AND BELNR = GT_BSE_CLRR-BELNR_CLR
                    AND GJAHR = GT_BSE_CLRR-GJAHR_CLR
                    AND REBZG = GT_BSE_CLRR-BELNR.
          IF IT_BSIDR IS NOT INITIAL.
            SORT IT_BSIDR BY BUKRS BELNR GJAHR REBZG.
          ENDIF.
        ENDIF.
****************************************** EOC : 5113 **********************************************

        "Added 4884 rebzg rebzj  augdt
        SELECT RBUKRS BELNR GJAHR BUDAT XREVERSED  REBZG REBZJ  AUGDT FROM ACDOCA INTO TABLE GT_BKPF FOR ALL ENTRIES IN GT_BSE
                                                                              WHERE RBUKRS = GT_BSE-BUKRS_CLR
                                                                                AND BELNR = GT_BSE-BELNR_CLR
                                                                                AND GJAHR = GT_BSE-GJAHR_CLR.
*                                                                                AND BUDAT IN S_INVDAT.
*                                                                                AND XREVERSED = 'X'.
                                                            "BOC 4884
        IF GT_BKPF[] IS NOT INITIAL.
          GT_BKPFN[] = GT_BKPF[].
          DELETE GT_BKPFN WHERE REBZG IS INITIAL.
          IF GT_BKPFN[] IS NOT INITIAL.
            SELECT BUKRS ,  BELNR , GJAHR , BLART
              FROM BKPF
              FOR ALL ENTRIES IN @GT_BKPFN
              WHERE BUKRS = @GT_BKPFN-BUKRS
              AND   BELNR = @GT_BKPFN-REBZG
              AND   GJAHR = @GT_BKPFN-REBZJ
              AND   BLART = 'AB'
              INTO TABLE @GT_BKPFN1.
            SORT : GT_BKPFN1 BY BUKRS   BELNR GJAHR  BLART.
          ENDIF.
        ENDIF.
                                                            "EOC 4884
      ENDIF.
    ENDIF.
    GT_BKPFN[] = GT_BKPF[].
    SORT : GT_BKPFN BY BUKRS BELNR GJAHR REBZG.
*    DELETE gt_bkpfn WHERE rebzg = ' '.
*    DELETE gt_bkpfn WHERE augdt IS NOT INITIAL .
    LOOP AT LT_ACDOCA_SORT INTO LW_ACDOCA.
      READ TABLE LT_BSID INTO LW_BSID  WITH KEY   BUKRS = LW_ACDOCA-RBUKRS
                                                  GJAHR  = LW_ACDOCA-GJAHR
                                                  BELNR  = LW_ACDOCA-BELNR
                                                  BUZEI  = LW_ACDOCA-BUZEI
                                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        PERFORM SUB_FINAL_TABLE_POPULATION USING LW_BSID
                                            LT_KNA1
                                            LT_ACDOCA
                                            LT_CEPCT
                                            LT_BSID_WR
                                            LT_BSID_NR
                                            LT_VBFA
                                            LT_VBAK
                                            LT_VBKD
                                            LT_TVZBT
                                            LT_TVZBT_FI
                                            LT_VBRK_RV
                                            LT_TVV1T
                                            LT_TVGRT
                                            IT_VBPA " SOC : 5113
                                            LT_T052
                                            LT1_BSID
                                            LT_TVKBT
                                      CHANGING LW_FINAL
                                               GT_FINAL
                                               LW_BSID_PRE.
        CLEAR:LW_BSID.
      ENDIF.
    ENDLOOP.
  ENDIF.

*** ---Calculations for UE Document type ---***
  LOOP AT GT_FINAL ASSIGNING FIELD-SYMBOL(<FS_FINAL>).
    IF <FS_FINAL>-ZUONR CP 'CRA*' OR <FS_FINAL>-ZUONR CP 'BAT*'  .
      <FS_FINAL>-AWKEY  = <FS_FINAL>-ZUONR .
    ENDIF.
  ENDLOOP.
  UNASSIGN :<FS_FINAL>.


  GT_FINAL1[] = GT_FINAL[].

  LOOP AT GT_FINAL INTO LW_FINAL WHERE BLART = 'UE'.
    LOOP AT GT_FINAL1 INTO DATA(LW_FINAL1) WHERE REBZG = LW_FINAL-BELNR AND BLART = 'UE'.
      LV_AMT_REL = LV_AMT_REL + LW_FINAL1-DMBTR.
      CLEAR:LW_FINAL1.
    ENDLOOP.
    IF LV_AMT_REL < 0.
      LV_AMT_REL = LV_AMT_REL * -1.
    ENDIF.

    LW_FINAL-AMT_REL = LW_FINAL-AMT_REL + LV_AMT_REL.
    LW_FINAL-NET_OS = LW_FINAL-DMBTR - LW_FINAL-AMT_REL."LW_FINAL-NET_OS + LV_NET_OS.
    LW_FINAL-AMT_OVD = LW_FINAL-NET_OS."LW_FINAL-AMT_OVD + LV_AMT_OVD.

    MODIFY GT_FINAL FROM LW_FINAL.
    DELETE GT_FINAL WHERE REBZG = LW_FINAL-BELNR.
    CLEAR:  LV_WRBTR, LV_DMBTR,LV_AMT_REL,LV_AMT_OVD,LV_NET_OS,LV_NOT_DUE,LW_FINAL.
  ENDLOOP.






*--------------------------Start of Add by Jay Thakkar------------------------------------------*
  LOOP AT GT_FINAL INTO DATA(GW_FINAL_SORTNO).
    SELECT SINGLE MATNR
      FROM VBRP
      INTO GW_FINAL_SORTNO-MATNR
      WHERE VBELN = GW_FINAL_SORTNO-XBLNR+0(10).

    "added by basanta . 02.12.2022
    SELECT SINGLE VBELN,POSNR,VGBEL
                  FROM VBRP
                  INTO  @DATA(LS_VBRP) WHERE VBELN = @GW_FINAL_SORTNO-AWKEY(10).
    IF SY-SUBRC EQ 0.
      SELECT SINGLE VBELN,POSNR,PARVW,KUNNR
                    FROM VBPA
                    INTO @DATA(LS_VBPA) WHERE VBELN = @LS_VBRP-VGBEL AND PARVW = 'WE'.
      IF SY-SUBRC EQ 0.
        GW_FINAL_SORTNO-SHP_TO_PARTY = LS_VBPA-KUNNR.

        IF LS_VBPA-KUNNR IS NOT INITIAL.
          SELECT SINGLE ORT01 FROM KNA1 INTO GW_FINAL_SORTNO-ORT01 WHERE KUNNR = LS_VBPA-KUNNR.
        ENDIF.

      ENDIF.
    ENDIF.
    "ended by basanta.


    MODIFY GT_FINAL FROM GW_FINAL_SORTNO.
    CLEAR: GW_FINAL_SORTNO.

  ENDLOOP.

  IF GT_FINAL IS NOT INITIAL.
    SELECT BUKRS, BELNR, GJAHR, BUZEI, HKONT, HWSTE, KBETR
      FROM BSET
      INTO TABLE @DATA(GT_BSET)
      FOR ALL ENTRIES IN @LT_BSID
      WHERE BUKRS = @LT_BSID-BUKRS
      AND BELNR = @LT_BSID-BELNR
      AND GJAHR = @LT_BSID-GJAHR
      AND HKONT = '0000194519'.

    IF GT_BSET IS NOT INITIAL.
      LOOP AT GT_FINAL INTO DATA(GW_FINAL_TCS).
        READ TABLE GT_BSET INTO DATA(GW_BSET) WITH KEY BELNR = GW_FINAL_TCS-BELNR BUKRS = GW_FINAL_TCS-BUKRS.
        IF SY-SUBRC = 0.
          GW_FINAL_TCS-KBETR = GW_BSET-KBETR / 10.
        ENDIF.

        GW_FINAL_TCS-HWSTE = REDUCE HWSTE( INIT X = LV_HWSTE
                                                  FOR LS IN GT_BSET
                                                  WHERE ( BUKRS = GW_FINAL_TCS-BUKRS AND
                                                  BELNR = GW_FINAL_TCS-BELNR )
                                                  NEXT X = X + LS-HWSTE ).

        MODIFY GT_FINAL FROM GW_FINAL_TCS.
        CLEAR: GW_FINAL_TCS.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "BY Hiren Surati 15.07.2022 Start
  "BY Hiren Surati 15.07.2022 End

*---------------------------End of Add by Jay Thakkar--------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       Sub ALV Display
*----------------------------------------------------------------------*
FORM SUB_ALV_DISPLAY USING GT_FINAL TYPE TY_T_FINAL .
*   Local data decleration
  DATA:
    LW_LAYOUT    TYPE LVC_S_LAYO,   "ALV layout.
* Reference Variable for custom Container
    LV_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
    LV_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT,
    LT_EXCLUDE   TYPE UI_FUNCTIONS,
    LW_VARIANT   TYPE DISVARIANT,
    LW_PRINT     TYPE LVC_S_PRNT,
    LW_STABLE    TYPE LVC_S_STBL.

  CONSTANTS:
    LC_FIELD_BUKRS              TYPE LVC_FNAME VALUE 'BUKRS',
    LC_FIELD_KUNNR              TYPE LVC_FNAME VALUE 'KUNNR',
    LC_FIELD_RACCT              TYPE LVC_FNAME VALUE 'RACCT',
    LC_FIELD_KNAME              TYPE LVC_FNAME VALUE 'KNAME',
    LC_FIELD_PRCTR              TYPE LVC_FNAME VALUE 'PRCTR',
    LC_FIELD_AWKEY              TYPE LVC_FNAME VALUE 'AWKEY',
    LC_FIELD_BELNR              TYPE LVC_FNAME VALUE 'BELNR',
    LC_FIELD_BUDAT              TYPE LVC_FNAME VALUE 'BUDAT',
    LC_FIELD_BLDAT              TYPE LVC_FNAME VALUE 'BLDAT',
    LC_FIELD_BLART              TYPE LVC_FNAME VALUE 'BLART',
    LC_FIELD_WAERS              TYPE LVC_FNAME VALUE 'WAERS',
    LC_FIELD_BSCHL              TYPE LVC_FNAME VALUE 'BSCHL',
    LC_FIELD_WRBTR              TYPE LVC_FNAME VALUE 'WRBTR',
    LC_FIELD_DMBTR              TYPE LVC_FNAME VALUE 'DMBTR',
    LC_FIELD_CR_PRD             TYPE LVC_FNAME VALUE 'CR_PRD',
    LC_FIELD_DUE_DATE           TYPE LVC_FNAME VALUE 'DUE_DATE',
    LC_FIELD_AMT_REL            TYPE LVC_FNAME VALUE 'AMT_REL',
    LC_FIELD_NET_OS             TYPE LVC_FNAME VALUE 'NET_OS',
    LC_FIELD_NOT_DUE            TYPE LVC_FNAME VALUE 'NOT_DUE',
    LC_FIELD_OD_DAYS            TYPE LVC_FNAME VALUE 'OD_DAYS',
    LC_FIELD_OD_DAYSDT          TYPE LVC_FNAME VALUE 'OD_DAYSDT',
    LC_FIELD_AMT_OVD            TYPE LVC_FNAME VALUE 'AMT_OVD',
    LC_FIELD_INVREV             TYPE LVC_FNAME VALUE 'INVREV',
    LC_FIELD_LR_NO              TYPE LVC_FNAME VALUE 'LR_NO',
    LC_FIELD_LR_DATE            TYPE LVC_FNAME VALUE 'LR_DATE',
    LC_FIELD_BRAND              TYPE LVC_FNAME VALUE 'BRAND',
    LC_FIELD_KAM                TYPE LVC_FNAME VALUE 'KAM',
    LC_FIELD_KAMNAME            TYPE LVC_FNAME VALUE 'KAMNAME',
    LC_FIELD_MANAGER            TYPE LVC_FNAME VALUE 'MANAGER',
    LC_FIELD_PAY_TERMS          TYPE LVC_FNAME VALUE 'PAY_TERMS',
    LC_FIELD_XBLNR              TYPE LVC_FNAME VALUE 'XBLNR',
    LC_FIELD_ZOUNR              TYPE LVC_FNAME VALUE 'ZUONR',
    LC_FIELD_SGTXT              TYPE LVC_FNAME VALUE 'SGTXT',
    LC_FIELD_VTWEG              TYPE LVC_FNAME VALUE 'VTWEG',
    LC_FIELD_SPART              TYPE LVC_FNAME VALUE 'SPART',

    LC_FIELD_KDMAT              TYPE LVC_FNAME VALUE 'KDMAT',
    LC_FIELD_BSTKD              TYPE LVC_FNAME VALUE 'BSTKD',
    LC_FIELD_IHREZ              TYPE LVC_FNAME VALUE 'IHREZ',

    LC_FIELD_BSTDK              TYPE LVC_FNAME VALUE 'BSTDK',
    LC_FIELD_ZLOC               TYPE LVC_FNAME VALUE 'ZLOCN',
    LC_FIELD_LTEXT              TYPE LVC_FNAME VALUE 'LTEXT',

    LC_FIELD_BZIRK              TYPE LVC_FNAME VALUE 'BZIRK',
    LC_FIELD_MATNR              TYPE LVC_FNAME VALUE 'MATNR',

    LC_FIELD_HWSTE              TYPE LVC_FNAME VALUE 'HWSTE',
    LC_FIELD_KBETR              TYPE LVC_FNAME VALUE 'KBETR',

    LC_FIELD_AGEING             TYPE LVC_FNAME VALUE 'AGEING',


    LC_FIELD_DUE_PERT_MTH       TYPE LVC_FNAME VALUE 'DUE_PERT_MTH',
    LC_FIELD_DUE_NEXT_MTH       TYPE LVC_FNAME VALUE 'DUE_NEXT_MTH',
    LC_FIELD_DUE_AFT_NEXT_MTH   TYPE LVC_FNAME VALUE 'DUE_AFT_NEXT_MTH',

    LC_FIELD_SHIP_TO_PARTY      TYPE LVC_FNAME VALUE 'SHP_TO_PARTY',
    LC_FIELD_SHIP_TO_PARTY_ADDR TYPE LVC_FNAME VALUE 'ORT01',
    LC_FIELD_NAME1              TYPE LVC_FNAME VALUE 'NAME1', " SOC : 5113
    LC_FIELD_SORTL              TYPE LVC_FNAME VALUE 'SORTL', " SOC : 5113
***    soc by jash rajpara dev id: 5628
    LC_FIELD_WRBTR1             TYPE LVC_FNAME VALUE 'WRBTR1',
    LC_FIELD_DIFFW              TYPE LVC_FNAME VALUE 'DIFFW',
    LC_FIELD_NET_OS_DOC         TYPE LVC_FNAME VALUE 'NET_OS_DOC'.
***    eoc by jash rajpara dev id: 5628

  IF GV_GRID_9002 IS  INITIAL.
    PERFORM  SUB_FIELDCAT_INIT_9002 USING :
    LC_FIELD_BUKRS          'Company Code',
    LC_FIELD_KUNNR          'Customer Code',
    LC_FIELD_RACCT          'Reconciliation A/C',
    LC_FIELD_KNAME          'Customer',
    LC_FIELD_PRCTR          'Profit Center',
    LC_FIELD_AWKEY          'Inv',   "AWKEY
    LC_FIELD_XBLNR          'Reference No.',
    LC_FIELD_BELNR          'Document Number',
    LC_FIELD_BUDAT          'Posting Date',
    LC_FIELD_BLDAT          'Document Date',
    LC_FIELD_BLART          'Document Type',
    LC_FIELD_WAERS          'Currency Key',
    LC_FIELD_BSCHL          'Posting Key',
    LC_FIELD_WRBTR          'Amount in doc currency',
    LC_FIELD_DMBTR          'Amount in local currency',
    LC_FIELD_CR_PRD         'Cr_Prd',
    LC_FIELD_DUE_DATE       'Due Date',
    LC_FIELD_AMT_REL        'Amt realised',
    LC_FIELD_NET_OS         'Net Os',
    LC_FIELD_NOT_DUE        'Not Due',
    LC_FIELD_OD_DAYS        'Od Days',
    LC_FIELD_OD_DAYSDT      'Od Days Doc Date', "Add by Shashank on 26th Nov 2019
    LC_FIELD_AMT_OVD        'Amt Ovd',
    LC_FIELD_INVREV         'Interest Receivable', "Add by Shashank on 26th Nov 2019
    LC_FIELD_LR_NO          'LR No',
    LC_FIELD_LR_DATE        'LR Date',
    LC_FIELD_BRAND          'Brand',
    LC_FIELD_KAM            'Sales Region', "Change by Swapnil on 17.06.2019
    LC_FIELD_KAMNAME        'KAM Name',
    LC_FIELD_MANAGER        'Manager',
    LC_FIELD_PAY_TERMS      'Pay Terms',
    LC_FIELD_ZOUNR          'Assignment',
    LC_FIELD_SGTXT          'Text',
    LC_FIELD_VTWEG          'Distribution Channel',
    LC_FIELD_SPART          'Division',
    LC_FIELD_KDMAT          'Customer ref',
    LC_FIELD_BSTKD          'CUSTOMER PO No',
    LC_FIELD_IHREZ          'customer’s marketing person',
    LC_FIELD_BSTDK          'PO Date',
    LC_FIELD_ZLOC           'Location',
    LC_FIELD_LTEXT          'Profit Center Description',
    LC_FIELD_BZIRK          'Sales District',                 "Added by Jay Thakkar
    LC_FIELD_MATNR          'Sort No',                        "Added by Jay Thakkar
    LC_FIELD_HWSTE          'TCS Value',                      "Added by Jay Thakkar
    LC_FIELD_KBETR          'TCS %',                          "Added by Jay Thakkar

    LC_FIELD_AGEING      'Ageing',                       "added by basanta 26.11.21
    LC_FIELD_DUE_PERT_MTH      'Due in Particular Month',
    LC_FIELD_DUE_NEXT_MTH      'Due in Next Month',
    LC_FIELD_DUE_AFT_NEXT_MTH      'Due After Next Month',

    LC_FIELD_SHIP_TO_PARTY  'Ship to Party',     "ADDED BY BASANTA 02.12.2022
    LC_FIELD_SHIP_TO_PARTY_ADDR  'Ship to Party Address',     "ADDED BY BASANTA 02.12.2022
    LC_FIELD_NAME1  'Agent Name',          " SOC : 5113
    LC_FIELD_SORTL  'Agent Zone Code'.     " SOC : 5113
***    soc by jash rajpara dev id: 5628
    IF C2 EQ 'X'.
      PERFORM  SUB_FIELDCAT_INIT_9002 USING :
      LC_FIELD_WRBTR1  'Amt Realised in Doc Curr',"" soc 5628
      LC_FIELD_NET_OS_DOC   'Net OS in Doc Curr',
      LC_FIELD_DIFFW   'Not Due In Doc Curr'.
    ENDIF.
***    eoc by jash rajpara dev id: 5628
*   Populating the Layout
    LW_LAYOUT-BOX_FNAME      = 'CHECK_BOX'.
    LW_LAYOUT-ZEBRA          = 'X'.
    LW_LAYOUT-SEL_MODE       = 'A'.


    CREATE OBJECT LV_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CUSTOM_CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.
    IF SY-SUBRC = 0.
* create an instance of alv control

      CREATE OBJECT GV_GRID_9002
        EXPORTING
          I_PARENT        = LV_CONTAINER
          I_FCAT_COMPLETE = ABAP_ON.
      IF SY-SUBRC = 0.
        PERFORM SUB_EXCLUDE_FUNCTION CHANGING LT_EXCLUDE.

        LW_VARIANT-REPORT   = SY-REPID.
        LW_VARIANT-USERNAME = SY-UNAME.
* Formatted Output Table is Sent to Control
*        IF sy-tcode <> 'SE37'.
        BREAK V00046.

        LOOP AT GT_FINAL ASSIGNING FIELD-SYMBOL(<FS_FINAL>).
          CLEAR : LV_NEXTMONTH , LV_AFT_NEXTMONTH.
          IF SY-DATUM(6) = <FS_FINAL>-DUE_DATE(6).
            <FS_FINAL>-DUE_PERT_MTH = <FS_FINAL>-NET_OS.
          ENDIF.

          CALL FUNCTION 'OIL_GET_NEXT_MONTH'
            EXPORTING
              I_DATE = SY-DATUM
            IMPORTING
              E_DATE = LV_NEXTMONTH.


          CALL FUNCTION 'OIL_GET_NEXT_MONTH'
            EXPORTING
              I_DATE = LV_NEXTMONTH
            IMPORTING
              E_DATE = LV_AFT_NEXTMONTH.

          IF <FS_FINAL>-DUE_DATE(6) = LV_NEXTMONTH(6).
            <FS_FINAL>-DUE_NEXT_MTH = <FS_FINAL>-NET_OS.
          ENDIF.

          IF <FS_FINAL>-DUE_DATE(6) >= LV_AFT_NEXTMONTH(6).
            <FS_FINAL>-DUE_AFT_NEXT_MTH = <FS_FINAL>-NET_OS.
          ENDIF.


        ENDLOOP.


        CALL METHOD GV_GRID_9002->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING
            IS_LAYOUT                     = LW_LAYOUT
            IS_PRINT                      = LW_PRINT
            IT_TOOLBAR_EXCLUDING          = LT_EXCLUDE[]
            IS_VARIANT                    = LW_VARIANT
            I_SAVE                        = 'A'
            I_DEFAULT                     = ABAP_ON
          CHANGING
            IT_OUTTAB                     = GT_FINAL
            IT_FIELDCATALOG               = GT_FIELDCAT_9002
          EXCEPTIONS
            INVALID_PARAMETER_COMBINATION = 1
            PROGRAM_ERROR                 = 2
            TOO_MANY_LINES                = 3
            OTHERS                        = 4.
        IF SY-SUBRC = 0.
          CALL METHOD GV_GRID_9002->CHECK_CHANGED_DATA.

*   no need to do
        ENDIF.                              " IF sy-subrc

*        ENDIF. " SY-TCODE <> 'SE37'
      ENDIF.                               " IF sy-subrc <> 0.
    ENDIF.   "  IF sy-subrc = 0.
  ELSE.
    IF GT_FINAL IS  INITIAL.
      MESSAGE 'No Data found for Display'(015) TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      CALL METHOD GV_GRID_9002->CHECK_CHANGED_DATA.

      CALL METHOD GV_GRID_9002->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = LW_STABLE
        EXCEPTIONS
          FINISHED  = 1
          OTHERS    = 2.
      IF SY-SUBRC = 0.
*          no need to do
      ENDIF.
    ENDIF.
  ENDIF.    "GV_GRID initial

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'Z_GUI_9002'.
*  SET TITLEBAR 'Z_TITEL_9002'.
  PERFORM SUB_ALV_DISPLAY USING GT_FINAL.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SUB_FINAL_TABLE_POPULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_BSID  text
*      -->P_LT_KNA1  text
*      -->P_LT_ACDOCA  text
*      <--P_GT_FINAL  text
*      <--P_LW_BSID_PRE  text
*----------------------------------------------------------------------*
FORM SUB_FINAL_TABLE_POPULATION  USING    FP_LW_BSID TYPE TY_BSID
                                          FP_LT_KNA1 TYPE TY_T_KNA1
                                          FP_LT_ACDOCA TYPE TY_T_ACDOCA
                                          FP_LT_CEPCT TYPE TY_T_CEPCT
                                          LT_BSID_WR TYPE TY_T_BSID
                                          LT_BSID_NR TYPE TY_T_BSID
                                          FP_LT_VBFA TYPE TY_T_VBFA
                                          FP_LT_VBAK TYPE TY_T_VBAK
                                          FP_LT_VBKD TYPE TY_T_VBKD
                                          FP_LT_TVZBT TYPE TY_T_TVZBT
                                          FP_LT_TVZBT_FI TYPE TY_T_TVZBT
                                          FP_LT_VBRK_RV TYPE TY_T_VBRK
                                          FP_LT_TVV1T TYPE TY_T_TVV1T
                                          FP_LT_TVGRT TYPE TY_T_TVGRT
                                          FP_IT_VBPA  TYPE TY_T_VBPA " SOC : 5113
                                          FP_LT_T052  TYPE TY_T_T052
                                          LT_BSID TYPE TY1_T_BSID
                                          FP_LT_TVKBT TYPE TY_T_TVKBT
                                 CHANGING FP_LW_FINAL TYPE TY_FINAL
                                          FP_GT_FINAL TYPE TY_T_FINAL
                                          FP_LW_BSID_PRE TYPE TY_BSID.
*   Local data calculation
  DATA: LW_BSID_WR           TYPE TY_BSID,
        LW_BSID_NR           TYPE TY_BSID,
        LW1_BSID             TYPE TY1_BSID,
        LV_BSID_WR           TYPE SY-TABIX,
        LV_BSID_NR           TYPE SY-TABIX,
        LV_TOTAL_AMT_DMBTR   TYPE WRF_PPW_VALUE_MAX,
        LV_TOTAL_AMT_WRBTR   TYPE WRF_PPW_VALUE_MAX,
        LV_PARTIAL_AMT_DMBTR TYPE WRF_PPW_VALUE_MAX,
        LV_PARTIAL_AMT_WRBTR TYPE WRF_PPW_VALUE_MAX.

  PERFORM SUB_ONE_TIME_CALCULATION USING FP_LW_BSID
                                         FP_LT_KNA1
                                         FP_LT_ACDOCA
                                         FP_LT_CEPCT
                                         FP_LT_VBFA
                                         FP_LT_VBAK
                                         FP_LT_VBKD
                                         FP_LT_TVZBT
                                         FP_LT_TVZBT_FI
                                         FP_LT_VBRK_RV
                                         FP_LT_TVV1T
                                         FP_LT_TVGRT
                                         FP_IT_VBPA " SOC : 5113
*                                         FP_ " SOC : 5113
                                         FP_LT_T052
                                         FP_LT_TVKBT
                                   CHANGING FP_LW_FINAL.
*    Currency calculation
*    Partial payment calculation
  READ TABLE LT_BSID_WR TRANSPORTING NO FIELDS WITH KEY BUKRS = FP_LW_BSID-BUKRS
                                                        REBZJ = FP_LW_BSID-GJAHR
                                                        REBZG = FP_LW_BSID-BELNR
                                                        BINARY SEARCH.

  LV_PARTIAL_AMT_WRBTR = 0.
  LOOP AT LT_BSID INTO LW1_BSID WHERE BUKRS = FP_LW_BSID-BUKRS
                                       AND REBZG = FP_LW_BSID-BELNR
                                       AND BUZEI = FP_LW_BSID-BUZEI
                                       AND REBZJ = FP_LW_BSID-GJAHR.
    IF SY-SUBRC = 0.
      LV_PARTIAL_AMT_WRBTR = LV_PARTIAL_AMT_WRBTR + LW1_BSID-DMBTR.
    ENDIF.

  ENDLOOP.

*    Total Amt calculation
  READ TABLE LT_BSID_NR TRANSPORTING NO FIELDS WITH KEY BUKRS = FP_LW_BSID-BUKRS
                                                        GJAHR = FP_LW_BSID-GJAHR
                                                        BELNR = FP_LW_BSID-BELNR
                                                        BUZEI = FP_LW_BSID-BUZEI.
*                                                        BINARY SEARCH.
  IF SY-SUBRC = 0.
    LV_BSID_WR = SY-TABIX.
    LOOP AT LT_BSID_NR INTO LW_BSID_NR FROM LV_BSID_WR.
      IF LW_BSID_NR-BUKRS  = FP_LW_BSID-BUKRS
         AND LW_BSID_NR-BELNR  =  FP_LW_BSID-BELNR
         AND LW_BSID_NR-GJAHR =  FP_LW_BSID-GJAHR
         AND LW_BSID_NR-BUZEI =  FP_LW_BSID-BUZEI.
        LV_TOTAL_AMT_DMBTR = LV_TOTAL_AMT_DMBTR + LW_BSID_NR-DMBTR.
        LV_TOTAL_AMT_WRBTR = LV_TOTAL_AMT_WRBTR + LW_BSID_NR-WRBTR.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  BREAK ABAP03.
*****        Populate the Adjustment Amount  ******DoNot Calculate for document type AB and DZ
  IF FP_LW_FINAL-BLART <> 'AB' AND FP_LW_FINAL-BLART <> 'DZ'.
    SORT : GT_BSE BY BELNR_CLR DESCENDING GJAHR_CLR DESCENDING.
    LOOP AT GT_BSE  INTO GW_BSE WHERE BUKRS = FP_LW_BSID-BUKRS
                                  AND GJAHR = FP_LW_BSID-GJAHR
                                  AND BELNR = FP_LW_BSID-BELNR
                                  AND BUZEI = FP_LW_BSID-BUZEI.
*************************************** SOC : 5113 **************************************************
**      soc by jash rajpara dt: 27.06.25 dev id: 6981
*      FP_LW_FINAL-DIFFW       = GW_BSE-DIFFW. "" ADDED BY JASH RAJPARA DEV ID:5628 DT 09.09.24
*      FP_LW_FINAL-NET_OS_DOC  = GW_BSE-DIFFW.
**      eoC BY JASH RAJPARA DT: 27.06.25 DEV ID: 6981

      READ TABLE IT_BSIDR INTO WA_BSIDR WITH KEY BUKRS = GW_BSE-BUKRS_CLR BELNR = GW_BSE-BELNR_CLR
                                                 GJAHR = GW_BSE-GJAHR_CLR REBZG = GW_BSE-BELNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        FP_LW_FINAL-AMT_REL = FP_LW_FINAL-AMT_REL + WA_BSIDR-DMBTR.
**        soc by jash rajpara dt: 27.06.25 dev id: 6981

*        FP_LW_FINAL-WRBTR1    = WA_BSIDR-WRBTR." - FP_LW_FINAL-DIFFW.   "" ADDED BY JASH RAJPARA DEV ID:5628 DT 09.09.24
        FP_LW_FINAL-WRBTR1 = FP_LW_FINAL-WRBTR1 + WA_BSIDR-WRBTR.
**        EOC BY JASH RAJPARA DT: 27.06.25 DEV ID: 6981
*        IF FP_LW_FINAL-OD_DAYS > 30.
*          FP_LW_FINAL-NE
*          ENDIF.
      ENDIF.
        CLEAR : WA_BSIDR.
*      READ TABLE GT_BKPF INTO GW_BKPF WITH KEY BUKRS = GW_BSE-BUKRS_CLR BELNR = GW_BSE-BELNR_CLR GJAHR = GW_BSE-GJAHR_CLR.
************************************************* EOC : 4453 ********************************************
*      IF SY-SUBRC = 0 AND GW_BKPF-XREVERSED <> 'X'.
*        FP_LW_FINAL-AMT_REL = FP_LW_FINAL-AMT_REL + ( GW_BSE-DMBTR - GW_BSE-DIFHW )."LV_PARTIAL_AMT_WRBTR.
*        IF GW_BSE-DIFFW IS NOT INITIAL.
*          FP_LW_FINAL-AMT_REL =  FP_LW_FINAL-AMT_REL - GW_BSE-WSKTO.
*        ENDIF.
*        EXIT.  "Added 4884
*      ENDIF.
************************************************* EOC : 4453 ********************************************
*   CLEAR:GW_BSE,GW_BKPF.
*************************************** SOC : 5113 **************************************************
        CLEAR:GW_BSE.

      ENDLOOP.
    ENDIF.

    FP_LW_FINAL-DMBTR = LV_TOTAL_AMT_DMBTR.
    FP_LW_FINAL-WRBTR = LV_TOTAL_AMT_WRBTR.
**  soc by jash rajpara dt: 28.06.25 dev id: 6981
    FP_LW_FINAL-NET_OS_DOC = FP_LW_FINAL-WRBTR - FP_LW_FINAL-WRBTR1.
    FP_LW_FINAL-DIFFW = FP_LW_FINAL-NET_OS_DOC.
**  eoc by jash rajpara dt: 28.06.25dev id: 6981
**  soc by jash rajpara dt: 28.09.24
    IF FP_LW_FINAL-NET_OS_DOC IS INITIAL.
      FP_LW_FINAL-NET_OS_DOC = LV_TOTAL_AMT_WRBTR.
    ENDIF.
    IF ( FP_LW_FINAL-DIFFW IS INITIAL AND FP_LW_FINAL-DUE_DATE > P_CDATE ).
      FP_LW_FINAL-DIFFW = FP_LW_FINAL-NET_OS_DOC.
    ENDIF.
**  eoc by jash rajpara dt: 28.09.2024
*  FP_LW_FINAL-AMT_REL = LV_PARTIAL_AMT_WRBTR.
    FP_LW_FINAL-NET_OS =  FP_LW_FINAL-DMBTR - FP_LW_FINAL-AMT_REL."LV_TOTAL_AMT_WRBTR - FP_LW_FINAL-AMT_REL."LV_PARTIAL_AMT_WRBTR.
    IF FP_LW_FINAL-OD_DAYS < 0.
*  Not Due is amout if the over policy due is not approched
      FP_LW_FINAL-NOT_DUE = FP_LW_FINAL-NET_OS.
      FP_LW_FINAL-OD_DAYS = '0'.
    ELSE.
*   Amt Ovd  "Amount Over Dues
      FP_LW_FINAL-AMT_OVD = FP_LW_FINAL-NET_OS.
    ENDIF.
    IF FP_LW_FINAL-BLART <> 'DZ'.
      IF FP_LW_FINAL-AMT_OVD > 0.
        FP_LW_FINAL-INVREV = ( FP_LW_FINAL-AMT_OVD * P_INTP * ( FP_LW_FINAL-OD_DAYSDT / 365 ) ) / 100.
      ENDIF.
    ELSE.
      FP_LW_FINAL-INVREV = 0.
    ENDIF.

************************************* SOC : 5113 ***************************************
    CLEAR : WA_VBPA.
    READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN = FP_LW_FINAL-AWKEY BINARY SEARCH.
    IF SY-SUBRC = 0.
      FP_LW_FINAL-NAME1 = WA_VBPA-NAME1.
      FP_LW_FINAL-SORTL = WA_VBPA-SORTL.
    ENDIF.
    CLEAR : WA_VBPA.
************************************* EOC : 5113 ***************************************

*****-- For Credit Document amount should be -Ve --***
    IF FP_LW_BSID-SHKZG = 'H'.
      IF FP_LW_FINAL-WRBTR IS NOT INITIAL.
        FP_LW_FINAL-WRBTR =     FP_LW_FINAL-WRBTR * -1.
        FP_LW_FINAL-WRBTR1 =     FP_LW_FINAL-WRBTR1 * -1.
*      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*        CHANGING
*          VALUE = FP_LW_FINAL-WRBTR.

      ENDIF.
      IF FP_LW_FINAL-DMBTR IS NOT INITIAL.
        FP_LW_FINAL-DMBTR = FP_LW_FINAL-DMBTR * -1.
      ENDIF.
      IF FP_LW_FINAL-NET_OS IS NOT INITIAL.
        FP_LW_FINAL-NET_OS = FP_LW_FINAL-NET_OS * -1.
      ENDIF.
**   soc by jash rajpara dt: 28.09.2024 dev id: 5628
      IF FP_LW_FINAL-NET_OS_DOC IS NOT INITIAL.
        FP_LW_FINAL-NET_OS_DOC = FP_LW_FINAL-NET_OS_DOC * -1.
      ENDIF.
      IF FP_LW_FINAL-DIFFW IS NOT INITIAL.
        FP_LW_FINAL-DIFFW = FP_LW_FINAL-DIFFW * -1.
      ENDIF.
**    eoc by jash rajparar dt: 28.09.24 dev id: 5628
      IF FP_LW_FINAL-NOT_DUE IS NOT INITIAL.
        FP_LW_FINAL-NOT_DUE = FP_LW_FINAL-NOT_DUE * -1.
      ENDIF.
      IF FP_LW_FINAL-AMT_OVD IS NOT INITIAL.
        FP_LW_FINAL-AMT_OVD = FP_LW_FINAL-AMT_OVD * -1.
      ENDIF.
      IF FP_LW_FINAL-INVREV IS NOT INITIAL.
        FP_LW_FINAL-INVREV = FP_LW_FINAL-INVREV * -1.
      ENDIF.
    ENDIF.

    IF FP_LW_FINAL-BLART = 'AB' OR FP_LW_FINAL-BLART = 'DZ'.
      CLEAR:GW_BSE.
      READ TABLE GT_BSE  INTO GW_BSE WITH KEY BUKRS_CLR = FP_LW_BSID-BUKRS " COMMENTED BY RIYA ON 15.11.2022
                                              GJAHR_CLR = FP_LW_BSID-GJAHR
                                              BELNR_CLR = FP_LW_BSID-BELNR.

      IF SY-SUBRC <> 0. " COMMENTED BY RIYA ON 15.11.2022
**    IF SY-SUBRC = 0. " ADDED BY RIYA ON 15.11.2022
        APPEND FP_LW_FINAL TO FP_GT_FINAL.
                                                            "BOC 4884
      ELSE.
        IF GW_BSE-CLRIN = '2'.
          IF FP_LW_BSID-REBZG IS NOT INITIAL.

            READ TABLE GT_BKPFN INTO GW_BKPF WITH KEY BUKRS = GW_BSE-BUKRS_CLR
                                                     BELNR = GW_BSE-BELNR_CLR
                                                     GJAHR = GW_BSE-GJAHR_CLR
                                                     REBZG = FP_LW_BSID-REBZG BINARY SEARCH.
            IF SY-SUBRC <> 0.
              APPEND FP_LW_FINAL TO FP_GT_FINAL.
            ELSE.

              IF LINE_EXISTS( GT_BKPFN1[ BUKRS = GW_BSE-BUKRS_CLR
                                         BELNR = GW_BKPF-REBZG
                                         GJAHR = GW_BKPF-REBZJ ] ).
                APPEND FP_LW_FINAL TO FP_GT_FINAL.

              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE GT_BKPFN INTO GW_BKPF WITH KEY BUKRS = GW_BSE-BUKRS_CLR
                                                   BELNR = GW_BSE-BELNR_CLR
                                                   GJAHR = GW_BSE-GJAHR_CLR
                                                   REBZG = ' ' BINARY SEARCH.
            IF SY-SUBRC = 0.
              APPEND FP_LW_FINAL TO FP_GT_FINAL.
            ENDIF.
          ENDIF.
        ENDIF.
                                                            "EOC 4884
      ENDIF. " COMMENTED BY RIYA ON 15.11.2022
    ELSE.
      APPEND FP_LW_FINAL TO FP_GT_FINAL.
    ENDIF.


    CLEAR: FP_LW_FINAL,LV_TOTAL_AMT_DMBTR,LV_TOTAL_AMT_WRBTR.
ENDFORM.
*----------------------------------------------------------------------*
*      -->P_LC_FIELD_CHECK  text
*      -->P_1101   text
*----------------------------------------------------------------------*
FORM SUB_FIELDCAT_INIT_9002  USING   P_FIELD TYPE LVC_FNAME
                                     P_TEXT TYPE  LVC_TXTCOL.
*  Local data decleration
  DATA: LW_FIELDCAT TYPE LVC_S_FCAT.  "WA for Filedcat
*   Layout for ALV
  CLEAR LW_FIELDCAT.
  GV_POS = GV_POS + 1.
  LW_FIELDCAT-COL_POS   = GV_POS.            " Column Position
  LW_FIELDCAT-FIELDNAME = P_FIELD.           " Field name
  LW_FIELDCAT-TABNAME   = 'GT_FINAL'.          " Table Name
  LW_FIELDCAT-COLTEXT   = P_TEXT.            " Selection Text
  LW_FIELDCAT-COL_OPT   = ABAP_TRUE.
  APPEND LW_FIELDCAT TO GT_FIELDCAT_9002.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM SUB_EXCLUDE_FUNCTION  CHANGING PT_EXCLUDE TYPE UI_FUNCTIONS.
  DATA LS_EXCLUDE TYPE UI_FUNC.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUB_ONE_TIME_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FP_LW_BSID  text
*      -->P_FP_LT_KNA1  text
*      -->P_FP_LT_ACDOCA  text
*      <--P_FP_LW_FINAL  text
*      <--P_FP_LW_FINAL_KUNNR  text
*      <--P_=  text
*      <--P_FP_LW_BSID_PRE_KUNNR  text
*----------------------------------------------------------------------*
FORM SUB_ONE_TIME_CALCULATION  USING    FP_LW_BSID TYPE TY_BSID
                                        FP_LT_KNA1 TYPE TY_T_KNA1
                                        FP_LT_ACDOCA TYPE TY_T_ACDOCA
                                        FP_LT_CEPCT TYPE TY_T_CEPCT
                                        FP_LT_VBFA TYPE TY_T_VBFA
                                        FP_LT_VBAK TYPE TY_T_VBAK
                                        FP_LT_VBKD TYPE TY_T_VBKD
                                        FP_LT_TVZBT TYPE TY_T_TVZBT
                                        FP_LT_TVZBT_FI TYPE TY_T_TVZBT
                                        FP_LT_VBRK_RV TYPE TY_T_VBRK
                                          FP_LT_TVV1T TYPE TY_T_TVV1T
                                          FP_LT_TVGRT TYPE TY_T_TVGRT
                                          FP_IT_VBPA TYPE TY_T_VBPA " SOC : 5113
                                        FP_LT_T052  TYPE TY_T_T052
                                        LT_TVKBT    TYPE TY_T_TVKBT
                               CHANGING FP_LW_FINAL TYPE TY_FINAL.
*   local data decleration
  DATA: LW_KNA1      TYPE TY_KNA1,
        LW_ACDOCA    TYPE TY_ACDOCA,
        LW_CEPCT     TYPE TY_PRCTR,
        LV_DUE_DATE  TYPE DATUM,
        LT_LINE      TYPE STANDARD TABLE OF TLINE INITIAL SIZE 0,
        LW_LINE      TYPE TLINE,
        LW_VBFA      TYPE TY_VBFA,
        LW_VBAK      TYPE TY_VBAK,
        LW_VBKD      TYPE TY_VBKD,
        LW_TVZBT     TYPE TY_TVZBT,
        LW_VBRK      TYPE TY_VBRK,
        LV_READ_NAME TYPE THEAD-TDNAME,
        LW_TVV1T     TYPE TY_TVV1T,
        LW_TVGRT     TYPE TY_TVGRT,
        LW_T052      TYPE TY_T052,
        LW_FAEDE     TYPE FAEDE,
        LW_FAEDE_IM  TYPE FAEDE.
*     Bukrs
  FP_LW_FINAL-BUKRS = FP_LW_BSID-BUKRS.
  FP_LW_FINAL-REBZG = FP_LW_BSID-REBZG.
*      Populating the single time calculation data for lw_final
  FP_LW_FINAL-KUNNR = FP_LW_BSID-KUNNR.    "Customer
*      Customer name
  READ TABLE FP_LT_KNA1 INTO LW_KNA1 WITH KEY KUNNR = FP_LW_BSID-KUNNR.
  IF SY-SUBRC = 0.
    CONCATENATE LW_KNA1-NAME1
                LW_KNA1-NAME2 INTO FP_LW_FINAL-KNAME SEPARATED BY ''.
  ENDIF.
*        Populate the Profit center
  READ TABLE FP_LT_ACDOCA INTO LW_ACDOCA WITH KEY RBUKRS = FP_LW_BSID-BUKRS
                                                  GJAHR  = FP_LW_BSID-GJAHR
                                                  BELNR = FP_LW_BSID-BELNR
                                                  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FP_LW_FINAL-PRCTR = LW_ACDOCA-PRCTR.

    READ TABLE FP_LT_CEPCT INTO LW_CEPCT WITH KEY PRCTR = LW_ACDOCA-PRCTR.
    IF SY-SUBRC = 0.
      FP_LW_FINAL-LTEXT = LW_CEPCT-LTEXT.
    ENDIF.
    "Added by Pratik 8.1.19
    FP_LW_FINAL-BLDAT = LW_ACDOCA-BLDAT.

    FP_LW_FINAL-RACCT = LW_ACDOCA-RACCT.

    FP_LW_FINAL-BSCHL = LW_ACDOCA-BSCHL.
  ENDIF.
  FP_LW_FINAL-AWKEY = FP_LW_BSID-AWKEY.   "INV  "In below calculation this is used

******************************************* SOC : 5113 ****************************************
  READ TABLE FP_IT_VBPA INTO WA_VBPA WITH KEY FP_LW_FINAL-AWKEY BINARY SEARCH.
  IF SY-SUBRC = 0.
    FP_LW_FINAL-NAME1 = WA_VBPA-NAME1.
    FP_LW_FINAL-SORTL = WA_VBPA-SORTL.
  ENDIF.
******************************************* EOC : 5113 ****************************************

  DATA: LV_VBELN TYPE VBAP-VBELN,
        LV_POSNR TYPE VBAP-POSNR.

  SELECT SINGLE A~WERKS B~KDMAT B~VBELN B~POSNR
    FROM VBRP AS A
    INNER JOIN VBAP AS B
    ON B~VBELN = A~AUBEL
    INTO ( FP_LW_FINAL-WERKS, FP_LW_FINAL-KDMAT, LV_VBELN, LV_POSNR )
    WHERE A~VBELN = FP_LW_FINAL-AWKEY
    AND B~POSNR = A~AUPOS.

    IF LV_VBELN IS NOT INITIAL AND LV_POSNR IS NOT INITIAL.

        SELECT SINGLE BSTKD IHREZ BSTDK
          FROM VBKD
          INTO ( FP_LW_FINAL-BSTKD, FP_LW_FINAL-IHREZ ,FP_LW_FINAL-BSTDK )
          WHERE VBELN = LV_VBELN
          AND POSNR = LV_POSNR.

          IF FP_LW_FINAL-BSTKD IS INITIAL AND FP_LW_FINAL-IHREZ IS INITIAL.
            SELECT SINGLE BSTKD IHREZ BSTDK
              FROM VBKD
              INTO ( FP_LW_FINAL-BSTKD, FP_LW_FINAL-IHREZ ,FP_LW_FINAL-BSTDK )
              WHERE VBELN = LV_VBELN.
            ENDIF.

          ENDIF.

        SELECT SINGLE BZIRK_AUFT INTO FP_LW_FINAL-REGIO_TXT FROM VBRP
                               WHERE VBELN = FP_LW_FINAL-AWKEY.
          FP_LW_FINAL-BELNR = FP_LW_BSID-BELNR.   "Doc
          FP_LW_FINAL-WAERS = FP_LW_BSID-WAERS.   "Cur
          FP_LW_FINAL-BUDAT = FP_LW_BSID-BUDAT.   "Inv Date
          FP_LW_FINAL-BUDAT = FP_LW_BSID-BUDAT. "Posting Date
          FP_LW_FINAL-BLART = FP_LW_BSID-BLART. "Posting Date
          FP_LW_FINAL-XBLNR = FP_LW_BSID-XBLNR.
          FP_LW_FINAL-ZUONR = FP_LW_BSID-ZUONR.
          FP_LW_FINAL-SGTXT = FP_LW_BSID-SGTXT.
          IF FP_LW_BSID-ZBD3T IS NOT INITIAL.
            FP_LW_FINAL-CR_PRD = FP_LW_BSID-ZBD3T.
          ELSEIF FP_LW_BSID-ZBD2T IS NOT INITIAL.
            FP_LW_FINAL-CR_PRD = FP_LW_BSID-ZBD2T.
          ELSE.
            FP_LW_FINAL-CR_PRD = FP_LW_BSID-ZBD1T.
          ENDIF.

          READ TABLE FP_LT_VBFA INTO LW_VBFA WITH KEY VBELN = FP_LW_FINAL-AWKEY.             "FP_LW_BSID-XBLNR.
          IF SY-SUBRC = 0.
            READ TABLE FP_LT_VBAK INTO LW_VBAK WITH KEY VBELN = LW_VBFA-VBELV.
            IF SY-SUBRC = 0.
              FP_LW_FINAL-ZLOCN   =   LW_VBAK-ZLOCN. " Location
            ENDIF.
            CLEAR : LW_VBFA, LW_VBAK.
          ENDIF.
*   Due Date calculation
          IF ( FP_LW_BSID-BLART = 'RV' ).
*  LR No
            LV_READ_NAME  = FP_LW_BSID-XBLNR.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                ID                      = 'Z004'
                LANGUAGE                = SY-LANGU
                NAME                    = LV_READ_NAME
                OBJECT                  = 'VBBK'
              TABLES
                LINES                   = LT_LINE
              EXCEPTIONS
                ID                      = 1
                LANGUAGE                = 2
                NAME                    = 3
                NOT_FOUND               = 4
                OBJECT                  = 5
                REFERENCE_CHECK         = 6
                WRONG_ACCESS_TO_ARCHIVE = 7
                OTHERS                  = 8.
            IF SY-SUBRC = 0.
              READ TABLE LT_LINE INTO LW_LINE INDEX 1.
              IF SY-SUBRC = 0.
                FP_LW_FINAL-LR_NO = LW_LINE-TDLINE.
              ENDIF.
            ENDIF.
*    LR Date
            LV_READ_NAME  = FP_LW_BSID-XBLNR.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                ID                      = 'Z005'
                LANGUAGE                = SY-LANGU
                NAME                    = LV_READ_NAME
                OBJECT                  = 'VBBK'
              TABLES
                LINES                   = LT_LINE
              EXCEPTIONS
                ID                      = 1
                LANGUAGE                = 2
                NAME                    = 3
                NOT_FOUND               = 4
                OBJECT                  = 5
                REFERENCE_CHECK         = 6
                WRONG_ACCESS_TO_ARCHIVE = 7
                OTHERS                  = 8.
            IF SY-SUBRC = 0.
              READ TABLE LT_LINE INTO LW_LINE INDEX 1.
              IF SY-SUBRC = 0.
                FP_LW_FINAL-LR_DATE = LW_LINE-TDLINE.
              ENDIF.
            ENDIF.
*    Brand, KAM, MANAGER, Pay term
            READ TABLE FP_LT_VBFA INTO LW_VBFA WITH KEY VBELN = FP_LW_FINAL-AWKEY.             "FP_LW_BSID-XBLNR.
            IF SY-SUBRC = 0.
              READ TABLE FP_LT_VBAK INTO LW_VBAK WITH KEY VBELN = LW_VBFA-VBELV.
              IF SY-SUBRC = 0.
*        Brand Des
                READ TABLE FP_LT_TVV1T INTO LW_TVV1T WITH KEY KVGR1 = LW_VBAK-KVGR1.
                IF SY-SUBRC = 0.
                  FP_LW_FINAL-KVGR1 = LW_VBAK-KVGR1.
                  FP_LW_FINAL-BRAND = LW_TVV1T-BEZEI.
                ENDIF.
*       Manager Des
                READ TABLE  FP_LT_TVGRT INTO LW_TVGRT WITH KEY VKGRP = LW_VBAK-VKGRP.
                IF SY-SUBRC = 0.

                  FP_LW_FINAL-MANAGER = LW_TVGRT-BEZEI."LW_TVGRT-VKGRP.
                  FP_LW_FINAL-KAMNAME = LW_TVGRT-BEZEI.
                ENDIF.

                READ TABLE  LT_TVKBT INTO DATA(LW_TVKBT) WITH KEY VKBUR = LW_VBAK-VKBUR.
                IF SY-SUBRC = 0.
                  FP_LW_FINAL-KAMNAME = LW_TVKBT-BEZEI.

                  CLEAR:LW_TVKBT.
                ENDIF.
              ENDIF.
              READ TABLE FP_LT_VBKD INTO LW_VBKD WITH KEY VBELN = LW_VBFA-VBELV.
              IF SY-SUBRC = 0.
                SELECT SINGLE BZTXT FROM T171T INTO FP_LW_FINAL-KAM WHERE BZIRK = LW_VBKD-BZIRK AND SPRAS = 'EN'.
*        FP_LW_FINAL-KAM = LW_VBKD-BZIRK.    "KAM
                  FP_LW_FINAL-BZIRK = LW_VBKD-BZIRK.  "Added by Jay Thakkar
                ENDIF.
              ENDIF.
*     for payment term

              READ TABLE FP_LT_VBRK_RV INTO LW_VBRK WITH KEY VBELN = FP_LW_FINAL-AWKEY.  "FP_LW_BSID-XBLNR.
              IF SY-SUBRC = 0.
*---Distribution Channel----*
                FP_LW_FINAL-VTWEG = LW_VBRK-VTWEG.
*---Division----------------*
                FP_LW_FINAL-SPART = LW_VBRK-SPART.

*---Sales Orgn
                FP_LW_FINAL-VKORG = LW_VBRK-VKORG.

*      Payment Due date
                CALL FUNCTION 'J_1A_SD_CI_DUEDATE_GET'
                  EXPORTING
                    IV_VBELN                 = LW_VBRK-VBELN
                    IV_ZTERM                 = LW_VBRK-ZTERM
*                   IV_RATNR                 = 1
                  IMPORTING
                    EV_NETDATE               = LV_DUE_DATE
                  EXCEPTIONS
                    FI_DOCUMENT_NOT_FOUND    = 1
                    PAYMENT_TERMS_INCOMPLETE = 2
                    INVOICE_NOT_FOUND        = 3
                    OTHERS                   = 4.
                IF SY-SUBRC = 0.
* Implement suitable error handling here
                ENDIF.
                READ TABLE FP_LT_TVZBT INTO LW_TVZBT WITH KEY ZTERM = LW_VBRK-ZTERM.
                IF SY-SUBRC = 0.
                  FP_LW_FINAL-PAY_TERMS = LW_TVZBT-VTEXT.  "Payment Terms
                ENDIF.
              ENDIF.
            ELSE.
*    Due payment calculation
              READ TABLE FP_LT_T052 INTO LW_T052 WITH KEY ZTERM = FP_LW_BSID-ZTERM.
              IF SY-SUBRC = 0.
*      Populating the data in FAEDE for payment term
                LW_FAEDE-SHKZG =  FP_LW_BSID-SHKZG.
                LW_FAEDE-ZFBDT =  FP_LW_BSID-BLDAT.
                LW_FAEDE-ZBD1T = LW_T052-ZTAG1.
                LW_FAEDE-ZBD2T = LW_T052-ZTAG2.
                LW_FAEDE-ZBD3T = LW_T052-ZTAG3.
                LW_FAEDE-KOART = 'D'.
                CALL FUNCTION 'DETERMINE_DUE_DATE'
                  EXPORTING
                    I_FAEDE                    = LW_FAEDE
                  IMPORTING
                    E_FAEDE                    = LW_FAEDE_IM
                  EXCEPTIONS
                    ACCOUNT_TYPE_NOT_SUPPORTED = 1
                    OTHERS                     = 2.
                IF SY-SUBRC <> 0.
* Implement suitable error handling here
                ENDIF.
              ENDIF.
              LV_DUE_DATE = LW_FAEDE_IM-NETDT.
*     Payment term Des
              READ TABLE FP_LT_TVZBT_FI INTO LW_TVZBT WITH KEY ZTERM = FP_LW_BSID-ZTERM.
              IF SY-SUBRC = 0.
                FP_LW_FINAL-PAY_TERMS = LW_TVZBT-VTEXT.  "Payment Terms
              ENDIF.
              "" Added by Rohit
              READ TABLE FP_LT_VBRK_RV INTO LW_VBRK WITH KEY VBELN = FP_LW_FINAL-AWKEY.  "FP_LW_BSID-XBLNR.
              IF SY-SUBRC = 0.
*---Distribution Channel----*
                FP_LW_FINAL-VTWEG = LW_VBRK-VTWEG.
*---Division----------------*
                FP_LW_FINAL-SPART = LW_VBRK-SPART.
              ENDIF.

            ENDIF.
*   Populating the Due Date
            FP_LW_FINAL-DUE_DATE = LV_DUE_DATE.

            IF FP_LW_FINAL-DUE_DATE IS INITIAL.
              FP_LW_FINAL-DUE_DATE = FP_LW_FINAL-BUDAT.
            ENDIF.
*  Out Standing Days
            IF FP_LW_FINAL-DUE_DATE IS NOT INITIAL.
              FP_LW_FINAL-OD_DAYS = P_CDATE - FP_LW_FINAL-DUE_DATE." FP_LW_BSID-ZFBDT. "LV_DUE_DATE.  "Net amout and Due amount logic is implemented outside this
            ENDIF.

            IF FP_LW_FINAL-BLDAT IS NOT INITIAL.
              FP_LW_FINAL-OD_DAYSDT = P_CDATE - FP_LW_FINAL-BLDAT.
            ENDIF.

            IF FP_LW_FINAL-OD_DAYS >= 0 AND  FP_LW_FINAL-OD_DAYS <= 30 .              "soc by basanta 26.11.21
              FP_LW_FINAL-AGEING = 'Due 0-30 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 31 AND  FP_LW_FINAL-OD_DAYS <= 60 .
              FP_LW_FINAL-AGEING = 'Due 31-60 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 61 AND  FP_LW_FINAL-OD_DAYS <= 90 .
              FP_LW_FINAL-AGEING = 'Due 61-90 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 91 AND  FP_LW_FINAL-OD_DAYS <= 120 .
              FP_LW_FINAL-AGEING = 'Due 91-120 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 121 AND  FP_LW_FINAL-OD_DAYS <= 150 .
              FP_LW_FINAL-AGEING = 'Due 121-150 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 151 AND  FP_LW_FINAL-OD_DAYS <= 180 .
              FP_LW_FINAL-AGEING = 'Due 151-180 Days'.
            ELSEIF FP_LW_FINAL-OD_DAYS >= 180 .
              FP_LW_FINAL-AGEING = 'Due >180 Days'.
            ENDIF.                                                                         "eoc by basanta 26.11.21






ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE  SY-UCOMM.
    WHEN 'BACK'.
      CLEAR : GT_FINAL.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CLEAR: GT_FINAL.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR: GT_FINAL.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTHORITY_CHECK .
  LOOP AT S_BUKRS INTO DATA(LWA_BUKRS).
    AUTHORITY-CHECK OBJECT 'TR_BUKRS'
        ID 'BUKRS' FIELD S_BUKRS-LOW.

    IF SY-SUBRC <> 0.
      MESSAGE E000(ZFI01) WITH S_BUKRS-LOW.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PORTAL_OUTSTANDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PORTAL_OUTSTANDING .
  TYPES:
    BEGIN OF TY_FINAL,
      RACCT       TYPE CHAR10,
      NAME1(35)   TYPE C,
      PRCTR       TYPE CHAR10,
      KTEXT       TYPE CHAR20,
      BELNR       TYPE CHAR10,
      ZFBDT       TYPE CHAR8,
      DUE_D       TYPE CHAR8,
      HSL(25)     TYPE  C,
      DMBTR(13)   TYPE C,
      BUKRS(25)   TYPE  C,
      CNAME(25)   TYPE  C,
      VKORG(25)   TYPE  C,
      VTWEG(25)   TYPE  C,
      SPART(25)   TYPE  C,
      KUNNR(25)   TYPE  C,
      KNAME(35)   TYPE  C,
      VKGRP(25)   TYPE  C,
      BEZEI_G(25) TYPE  C,
      VKBUR(25)   TYPE  C,
      BEZEI_O(25) TYPE  C,
      LR_NO(25)   TYPE C,
    END OF TY_FINAL.

  DATA:
    LT_FINAL TYPE STANDARD TABLE OF TY_FINAL,
    LW_FINAL TYPE TY_FINAL.

  "Add Header Name to Final ITAB
  APPEND VALUE #( BELNR = 'Invoice No' RACCT = 'CustNo'
  DMBTR = 'Invoice Amt' DUE_D = 'Due Dt' HSL   = 'Outstanding AMT' KTEXT = 'Profit Center Text'
  NAME1 = 'Cust Name' PRCTR = 'Profit Cnt' ZFBDT = 'Base Dt' BUKRS = 'Company Code'  CNAME = 'Company Name'
  VKORG = 'Sales Org' VTWEG = 'Dist Channel' SPART = 'Division' KUNNR = 'Agent CODE'  KNAME = 'Agent Name'
  VKGRP = 'Sales Group' BEZEI_G = 'Sale Group Description' VKBUR = 'Sales Office'  BEZEI_O = 'Sales Office Description'
  LR_NO = 'LR Number'
  ) TO LT_FINAL.

  "Fetch Profit center description
  SELECT PRCTR, KTEXT
    FROM CEPCT
    INTO TABLE @DATA(LT_CEPCT)
    FOR ALL ENTRIES IN @GT_FINAL
    WHERE PRCTR EQ @GT_FINAL-PRCTR AND
          SPRAS = 'E'.
    SORT LT_CEPCT BY PRCTR ASCENDING.

    "Fetch bill to party from invoice
    SELECT VBELN, KUNNR
      FROM VBPA
      INTO TABLE @DATA(LT_VBPA)
      FOR ALL ENTRIES IN @GT_FINAL
      WHERE VBELN EQ @GT_FINAL-AWKEY+0(10) AND
            PARVW EQ 'RE' .
      SORT LT_VBPA BY VBELN ASCENDING.

      "Commission agent FROM Customer master
      SELECT VKORG, VTWEG, SPART, KUNNR, LIFNR
        FROM KNVP
        INTO TABLE @DATA(LT_KNVP)
        FOR ALL ENTRIES IN @GT_FINAL
        WHERE VKORG EQ @GT_FINAL-VKORG AND
              VTWEG EQ @GT_FINAL-VTWEG AND
              SPART EQ @GT_FINAL-SPART AND
              PARVW EQ 'VA'.
        SORT LT_KNVP BY VKORG VTWEG SPART KUNNR ASCENDING.

        "Fetch Sales Group and Sales office based on bill to party
        SELECT VKORG, VTWEG, SPART, KUNNR, VKGRP, VKBUR
          FROM KNVV
          INTO TABLE @DATA(LT_KNVV)
          FOR ALL ENTRIES IN @GT_FINAL
          WHERE VKORG EQ @GT_FINAL-VKORG AND
                VTWEG EQ @GT_FINAL-VTWEG AND
                SPART EQ @GT_FINAL-SPART.
          SORT LT_KNVV BY VKORG VTWEG SPART KUNNR ASCENDING.

          SELECT BUKRS, BUTXT
            FROM T001
            INTO TABLE @DATA(LT_T001)
            FOR ALL ENTRIES IN @GT_FINAL
            WHERE BUKRS EQ @GT_FINAL-BUKRS.
            SORT LT_T001 BY BUKRS ASCENDING.

            SELECT KUNNR, NAME1
              FROM KNA1
              INTO TABLE @DATA(LT_KNA1)
              FOR ALL ENTRIES IN @GT_FINAL
              WHERE KUNNR EQ @GT_FINAL-KUNNR.
              SORT LT_KNA1 BY KUNNR ASCENDING.

              SELECT LIFNR, NAME1
                FROM LFA1
                INTO TABLE @DATA(LT_LFA1)
                FOR ALL ENTRIES IN @LT_KNVP
                WHERE LIFNR EQ @LT_KNVP-LIFNR.
                SORT LT_LFA1 BY LIFNR ASCENDING.

                "Fetch Sales GROUP description
                SELECT VKBUR, BEZEI
                  FROM TVKBT
                  INTO TABLE @DATA(LT_TVKBT)
                  FOR ALL ENTRIES IN @LT_KNVV
                  WHERE VKBUR EQ @LT_KNVV-VKBUR AND
                  SPRAS EQ 'E'.
                  SORT LT_TVKBT BY VKBUR ASCENDING.

                  "Fetch Sales office description
                  SELECT VKGRP, BEZEI
                    FROM TVGRT
                    INTO TABLE @DATA(LT_TVGRT)
                    FOR ALL ENTRIES IN @LT_KNVV
                    WHERE VKGRP EQ @LT_KNVV-VKGRP AND
                          SPRAS EQ 'E'.
                    SORT LT_TVGRT BY VKGRP ASCENDING.


                    DATA:
                      LV_DMBTR TYPE STRING,
                      LV_HSL   TYPE STRING.

                    BREAK V00046.
                    LOOP AT GT_FINAL INTO DATA(GW_FINALKP).
                      DATA(LV_KTEXT) = VALUE #( LT_CEPCT[ PRCTR = GW_FINALKP-PRCTR ]-KTEXT OPTIONAL ).
                      DATA(LV_KUNNR) = VALUE #( LT_VBPA[ VBELN = GW_FINALKP-AWKEY ]-KUNNR OPTIONAL ).
                      DATA(LV_LIFNR) = VALUE #( LT_KNVP[ VKORG = GW_FINALKP-VKORG VTWEG = GW_FINALKP-VTWEG
                            SPART = GW_FINALKP-SPART KUNNR = LV_KUNNR ]-LIFNR OPTIONAL ).
                      DATA(LV_BUTXT) = VALUE #( LT_T001[ BUKRS = GW_FINALKP-BUKRS ]-BUTXT OPTIONAL ).
                      DATA(LV_NAME1) = VALUE #( LT_KNA1[ KUNNR = GW_FINALKP-KUNNR ]-NAME1 OPTIONAL ).
                      DATA(LV_NAME2) = VALUE #( LT_LFA1[ LIFNR = LV_LIFNR ]-NAME1 OPTIONAL ).
                      DATA(LW_KNVP) = VALUE #( LT_KNVV[ VKORG = GW_FINALKP-VKORG VTWEG = GW_FINALKP-VTWEG
                            SPART = GW_FINALKP-SPART KUNNR = LV_KUNNR ] OPTIONAL ).
                      DATA(LV_BEZEI_G) = VALUE #( LT_TVGRT[ VKGRP = LW_KNVP-VKGRP ]-BEZEI OPTIONAL ).
                      DATA(LV_BEZEI_O) = VALUE #( LT_TVKBT[ VKBUR = LW_KNVP-VKBUR ]-BEZEI OPTIONAL ).

                      DATA NAME             TYPE THEAD-TDNAME.
                      DATA LINES            TYPE STANDARD TABLE OF TLINE.

                      NAME = GW_FINALKP-AWKEY.
                      IF NAME IS NOT INITIAL.
                        CALL FUNCTION 'READ_TEXT'
                          EXPORTING
                            CLIENT                  = SY-MANDT
                            ID                      = 'Z004'
                            LANGUAGE                = 'E'
                            NAME                    = NAME
                            OBJECT                  = 'VBBK'
                          TABLES
                            LINES                   = LINES
                          EXCEPTIONS
                            ID                      = 1
                            LANGUAGE                = 2
                            NAME                    = 3
                            NOT_FOUND               = 4
                            OBJECT                  = 5
                            REFERENCE_CHECK         = 6
                            WRONG_ACCESS_TO_ARCHIVE = 7
                            OTHERS                  = 8.
                        IF LINES IS NOT INITIAL.
                          DATA(LV_LR) = VALUE #( LINES[ 1 ]-TDLINE ).
                        ENDIF."IF lines IS NOT INITIAL.
                      ENDIF."IF name IS NOT INITIAL.

                      LV_DMBTR = GW_FINALKP-WRBTR.
                      IF GW_FINALKP-WRBTR LE 0.
                        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                          CHANGING
                            VALUE = LV_DMBTR.
                      ENDIF."IF gw_finalkp-wrbtr le 0.

                      LV_HSL = GW_FINALKP-NET_OS.
                      IF GW_FINALKP-NET_OS LE 0.
                        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                          CHANGING
                            VALUE = LV_HSL.
                      ENDIF."IF gw_finalkp-net_os le 0.

                      APPEND VALUE #( BELNR = GW_FINALKP-AWKEY
                      RACCT = GW_FINALKP-KUNNR
                      DMBTR = LV_DMBTR
                      DUE_D = GW_FINALKP-DUE_DATE
                      HSL   = LV_HSL
                      KTEXT = LV_KTEXT
                      NAME1 = LV_NAME1
                      PRCTR = GW_FINALKP-PRCTR
                      ZFBDT = GW_FINALKP-BLDAT
                      BUKRS = GW_FINALKP-BUKRS
                      CNAME = LV_BUTXT
                      VKORG = GW_FINALKP-VKORG
                      VTWEG = GW_FINALKP-VTWEG
                      SPART = GW_FINALKP-SPART
                      KUNNR = LV_LIFNR
                      KNAME = LV_NAME2
                      VKGRP = LW_KNVP-VKGRP
                      BEZEI_G = LV_BEZEI_G
                      VKBUR = LW_KNVP-VKBUR
                      BEZEI_O = LV_BEZEI_O
                    LR_NO = LV_LR
                      ) TO LT_FINAL.
                      CLEAR: GW_FINALKP, LV_LIFNR, LV_KTEXT, LV_KUNNR, LV_BUTXT, LV_NAME1,
                      LV_NAME2, LV_LR, LINES, NAME, LV_BEZEI_G, LV_BEZEI_O, LV_HSL, LV_DMBTR.
                    ENDLOOP."LOOP AT gt_final INTO DATA(gw_finalkp).

                    IF LT_FINAL[] IS NOT INITIAL.
                      CALL FUNCTION 'ZFI_CUST_OPEN_BALANCE' IN BACKGROUND TASK DESTINATION 'ZECCTOPI'
                        TABLES
                          CUST_BAL_TAB = LT_FINAL.
                      COMMIT WORK.
                    ENDIF."IF lt_final[] IS NOT INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BI_OUTSTANDING_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  BI_OUTSTANDING_AMT.
****************************Added by Surbhi Dadhich*****************************
  IF SY-BATCH = 'X'.
    REFRESH : IT_OUTSTAND_BI.
    IF GT_FINAL IS NOT INITIAL.
      SORT GT_FINAL BY KUNNR WERKS.
      LOOP AT GT_FINAL INTO DATA(LS_FINAL) GROUP BY ( K1 = LS_FINAL-KUNNR K2 = LS_FINAL-WERKS ) INTO DATA(L_BEAM).
        LOOP AT GROUP L_BEAM INTO DATA(LW_BEAM).
          WA_OUTSTAND_BI-DMBTR = WA_OUTSTAND_BI-DMBTR + LW_BEAM-DMBTR.
        ENDLOOP.
        WA_OUTSTAND_BI-KUNNR = LW_BEAM-KUNNR.
        IF LW_BEAM-WERKS = '1013' OR LW_BEAM-WERKS = '1014' OR LW_BEAM-WERKS = '1015'.
          WA_OUTSTAND_BI-VERTICAL = 'FABRIC'.
        ELSEIF LW_BEAM-WERKS = '8501'.
          WA_OUTSTAND_BI-VERTICAL = 'ADL'.
        ELSEIF LW_BEAM-WERKS = '8502'.
          WA_OUTSTAND_BI-VERTICAL = 'RTW'.
        ENDIF.
        APPEND WA_OUTSTAND_BI TO IT_OUTSTAND_BI.
        CLEAR : WA_OUTSTAND_BI, LW_BEAM, LS_FINAL.
      ENDLOOP.
    ENDIF.
    IF IT_OUTSTAND_BI IS NOT INITIAL.
      MODIFY ZSD_OUTSTAND_BI FROM TABLE IT_OUTSTAND_BI.
      IF SY-SUBRC = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.
****************************Added by Surbhi Dadhich*****************************
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN_TEXT .
  A1INT = 'Interest Percent'.
  A2INT = '%'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_VARIENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VARIENT .

  CHECK SY-BATCH IS INITIAL.
  CLEAR PO_VARNT.
  MOVE SY-SLSET TO PO_VARNT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_TABL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_TABL .

  CHECK PO_VARNT IS NOT INITIAL.
  CLEAR:GT_FINAL[].

  SELECT * FROM ZFI_SALESINV INTO CORRESPONDING FIELDS OF TABLE GT_FINAL
           WHERE BACK_JOB = PO_VARNT .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_DATA_TABLE .
  DATA: LT_SALESINV TYPE STANDARD TABLE OF ZFI_SALESINV,
        LW_SALESINV TYPE ZFI_SALESINV,

        L_NO        TYPE I.

  FREE:LT_SALESINV,LW_SALESINV.

  CHECK GT_FINAL IS NOT INITIAL.
  CHECK PO_VARNT IS NOT INITIAL.

  LOOP AT GT_FINAL INTO DATA(LW_FINAL).

    DATA: LV_VBELN TYPE VBAP-VBELN,
          LV_POSNR TYPE VBAP-POSNR.

    SELECT SINGLE B~KDMAT B~VBELN B~POSNR
      FROM VBRP AS A
      INNER JOIN VBAP AS B
      ON B~VBELN = A~AUBEL
      INTO ( LW_FINAL-KDMAT, LV_VBELN, LV_POSNR )
      WHERE A~VBELN = LW_FINAL-AWKEY
      AND B~POSNR = A~AUPOS.

      IF LV_VBELN IS NOT INITIAL AND LV_POSNR IS NOT INITIAL.

        SELECT SINGLE BSTKD IHREZ
          FROM VBKD
          INTO ( LW_FINAL-BSTKD, LW_FINAL-IHREZ )
          WHERE VBELN = LV_VBELN
          AND POSNR = LV_POSNR.

          IF LW_FINAL-BSTKD IS INITIAL AND LW_FINAL-IHREZ IS INITIAL.
            SELECT SINGLE BSTKD IHREZ
              FROM VBKD
              INTO ( LW_FINAL-BSTKD, LW_FINAL-IHREZ )
              WHERE VBELN = LV_VBELN.
            ENDIF.

          ENDIF.

          MOVE-CORRESPONDING LW_FINAL TO LW_SALESINV.

          L_NO = L_NO + 1 .

          LW_SALESINV-BACK_JOB = PO_VARNT .
          LW_SALESINV-SLNO     = L_NO .
          APPEND LW_SALESINV TO LT_SALESINV.
          CLEAR:LW_SALESINV,LW_FINAL.
        ENDLOOP.

        IF LT_SALESINV IS NOT INITIAL.
          DELETE FROM ZFI_SALESINV WHERE BACK_JOB = PO_VARNT .
          COMMIT WORK AND WAIT .

          MODIFY ZFI_SALESINV FROM TABLE LT_SALESINV .
          COMMIT WORK AND WAIT .
        ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXP_OUTSTANDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXP_OUTSTANDING .

  DATA : EXP_CUSTOUTSTD TYPE TABLE OF ZSD_DTR_CUST_OUTSTD_STR,
         S_EXPOUTSTD    TYPE ZSD_DTR_CUST_OUTSTD_STR.
  IF GT_FINAL IS NOT INITIAL.
    LOOP AT GT_FINAL INTO DATA(GS_FINAL).
      S_EXPOUTSTD-ZBUKRS_VF   = GS_FINAL-BUKRS.
      S_EXPOUTSTD-ZKUNNR      = GS_FINAL-KUNNR.
      S_EXPOUTSTD-ZNAME1      = GS_FINAL-KNAME.
      S_EXPOUTSTD-ZPRCTR      = GS_FINAL-PRCTR.
      S_EXPOUTSTD-ZREF_NO     = GS_FINAL-XBLNR.
      S_EXPOUTSTD-ZBLART      = GS_FINAL-BLART.
      S_EXPOUTSTD-AMT_REL     = GS_FINAL-WRBTR.
      S_EXPOUTSTD-ZBLDAT      = GS_FINAL-BLDAT.
      S_EXPOUTSTD-NET_OS      = GS_FINAL-NET_OS.
      S_EXPOUTSTD-ZSPART      = GS_FINAL-SPART.
      S_EXPOUTSTD-ZVTWEG      = GS_FINAL-VTWEG.
      APPEND S_EXPOUTSTD TO EXP_CUSTOUTSTD.
      CLEAR: S_EXPOUTSTD, GS_FINAL.
    ENDLOOP.

    EXPORT EXP_CUSTOUTSTD TO MEMORY ID 'ZFM_SD_DTR_OUTSTDDATA'.           "" exported to rfc ZFM_SD_DTR_OUTSTDDATA
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_customer .


  IF S_KUNNR[] IS INITIAL AND S_BUKRS[] IS NOT INITIAL AND S_RACCT[] IS NOT INITIAL.

    DATA: lt_knb1  TYPE STANDARD TABLE OF knb1,
          lt_kunnr TYPE RANGE OF kunnr.

    SELECT DISTINCT 'I' as sign,
                    'EQ' as option,
                    kunnr as low
      FROM knb1
      INTO TABLE @s_kunnr
      WHERE bukrs IN @S_BUKRS
        AND akont IN @S_RACCT.

  ENDIF.

ENDFORM.
