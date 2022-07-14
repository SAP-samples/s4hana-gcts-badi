*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGCTS_TOGGLE_BAD
*   generation date: 04.05.2022 at 10:56:01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGCTS_TOGGLE_BAD   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.