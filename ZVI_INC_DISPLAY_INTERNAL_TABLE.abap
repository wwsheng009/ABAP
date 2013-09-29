*&---------------------------------------------------------------------*
*&  Include           ZVI_INC_DISPLAY_INTERNAL_TABLE
*&---------------------------------------------------------------------*

TYPES: BEGIN OF _TY_LINE_DEBUG_INC,
  CHECK TYPE C,
  INDEX(3),
  TABDES TYPE CHAR20,
  TABNAME TYPE CHAR30,
  LINES TYPE N LENGTH 4,
END OF _TY_LINE_DEBUG_INC.

*DATA: _WA_LINE TYPE _TY_LINE_DEBUG_INC.


* 所用到的内表的列表。
DATA BEGIN OF _TAB_LIST_DEBUG OCCURS 0 .
        INCLUDE TYPE _TY_LINE_DEBUG_INC.
DATA FCAT TYPE LVC_T_FCAT.
DATA END OF _TAB_LIST_DEBUG.

DATA: _WA_FCAT TYPE LVC_S_FCAT.

DATA: SHOWED TYPE C VALUE ''.
  DATA: DISPLAYSUCCESS TYPE C VALUE ''.

FIELD-SYMBOLS: <_FS> TYPE ANY TABLE.

CLASS LCL_TABLE_DISPLAY   DEFINITION DEFERRED.


*宏，添加字段，这个宏一定要在_ADD_TABLE 之前使用。
DEFINE _ADD_FIELD_NAME_DEBUG.
  _WA_FCAT-FIELDNAME  = &1.
  _WA_FCAT-SCRTEXT_L  = &2.
  _WA_FCAT-SCRTEXT_M  = &2.
  _WA_FCAT-SCRTEXT_S  = &2.
  APPEND _WA_FCAT TO _TAB_LIST_DEBUG-FCAT.
  CLEAR _WA_FCAT.
END-OF-DEFINITION.


*宏，添加内表
DEFINE _ADD_TABLE_NAME_DEBUG.
  _TAB_LIST_DEBUG-TABNAME =  &1.
  _TAB_LIST_DEBUG-TABDES =  &2.
  APPEND _TAB_LIST_DEBUG.
  CLEAR _TAB_LIST_DEBUG.
END-OF-DEFINITION.


*调试选项
SELECTION-SCREEN BEGIN OF BLOCK _B2  .
SELECTION-SCREEN BEGIN OF LINE.
*
*  不处理
PARAMETERS _P1 TYPE C DEFAULT 'X' RADIOBUTTON GROUP _GR1 MODIF ID _BL.
SELECTION-SCREEN COMMENT 2(12) _COM1 FOR FIELD _P1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.

*选项1
PARAMETERS _P2 TYPE C RADIOBUTTON GROUP _GR1 MODIF ID _BL.
SELECTION-SCREEN COMMENT 2(12) _COM2 FOR FIELD _P2.
SELECTION-SCREEN END OF LINE.

*选项2
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS _P3 TYPE C RADIOBUTTON GROUP _GR1 MODIF ID _BL.
SELECTION-SCREEN COMMENT 2(12) _COM3 FOR FIELD _P3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK _B2.

*------------------------------------------------------------------------
* SELECTION SCREEN VALIDATION
*------------------------------------------------------------------------

*调整屏幕上的控件，一般情况下，这些数据不对最终用户开放
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
*    检查是否是开发人员
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'  ID 'ACTVT' FIELD '01'.
    IF SY-SUBRC <> 0 AND SCREEN-GROUP1 = '_BL'.
      SCREEN-INVISIBLE = '1'. "不可见但是选项还是可用的。
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

LOAD-OF-PROGRAM.
*  _T_B2 = '调试'.
  _COM1 = '执行原程序'.
  _COM2 = '单个内表内容显示'.
  _COM3 = 'ALV LIST 显示所有的内表数据'.

*&---------------------------------------------------------------------*
*&      Form  debug_me
*&---------------------------------------------------------------------*
*    让用户选择显示一个内表，还是全部内表。
*----------------------------------------------------------------------*
FORM _SHOW_DEBUG_DIALOG.
  IF SHOWED = 'X'.
    EXIT.
  ENDIF.
  IF _P1 = 'X'.
*    不处理
  ENDIF.

  IF _P2 = 'X'."调试程序选项
    PERFORM _TABLE_TO_SELECT_DEBUG.
    SHOWED = 'X'.
    EXIT.
  ENDIF.

  IF  _P3 = 'X'.
    PERFORM _DISPLAY_ALL_BLOCK_DEBUG.
    SHOWED = 'X'.
    EXIT.
  ENDIF.

ENDFORM .                    "debug_me




*&---------------------------------------------------------------------*
*&      Form  _TABLE_TO_SELECT_DEBUG
*&---------------------------------------------------------------------*
*   报表中用到的报表。
*----------------------------------------------------------------------*
FORM _TABLE_TO_SELECT_DEBUG.

  DATA: ANSWER TYPE STRING.              "用于存储用户选择

  DATA: SPOPLI LIKE SPOPLI OCCURS 0 WITH HEADER LINE. "定义供用户选择列表

  REFRESH SPOPLI.

  LOOP AT _TAB_LIST_DEBUG.
    CLEAR SPOPLI.
    SPOPLI-SELFLAG = ''.                     "设置选中

    SPOPLI-VAROPTION(30) = _TAB_LIST_DEBUG-TABDES.                "设置显示的文本
    SPOPLI-VAROPTION+20(20) = _TAB_LIST_DEBUG-TABNAME.
    APPEND SPOPLI.
  ENDLOOP.


  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      CURSORLINE         = 1
      MARK_FLAG          = ' '
      MARK_MAX           = 1
      START_COL          = 1             "设置开始的列
      START_ROW          = 1              "设置开始的行
      TEXTLINE1          = '请选择内表'                          "设置文本行内容1
*     TEXTLINE2          = ' '
*     TEXTLINE3          = ' '
      TITEL              = '选择内表' "设置标题
*     DISPLAY_ONLY       = ' '
    IMPORTING
      ANSWER             = ANSWER         "获得用户选择,这里返回的值对应是当前列表NO，比如第一个就返回1，第二个返回2。。。。。
    TABLES
      T_SPOPLI           = SPOPLI         "设置选择列表
    EXCEPTIONS
      NOT_ENOUGH_ANSWERS = 1
      TOO_MUCH_ANSWERS   = 2
      TOO_MUCH_MARKS     = 3
      OTHERS             = 4.
  IF SY-SUBRC <> 0.
*    WRITE:/ ANSWER."当选择取消时,answer = 'A'.
  ELSE.
*    WRITE /SPOPLI-VAROPTION.
  ENDIF.

  IF ANSWER <> 'A'.
    LOOP AT SPOPLI WHERE SELFLAG = 'X'.
      READ TABLE _TAB_LIST_DEBUG INDEX SY-TABIX.
      CONDENSE _TAB_LIST_DEBUG-TABNAME.
      ASSIGN (_TAB_LIST_DEBUG-TABNAME) TO <_FS>.
      IF SY-SUBRC <> 0.
        RETURN.
      ENDIF.
      IF <_FS> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      DATA : GR_TABLE TYPE REF TO CL_SALV_TABLE,
            LR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS.

      TRY.
          CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = GR_TABLE
          CHANGING
            T_TABLE      = <_FS>
            ).
        CATCH CX_SALV_MSG.
      ENDTRY.
      "显示列表

*列名设置
      DATA  LR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
      DATA  LR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.


      LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
      LR_COLUMNS->SET_OPTIMIZE('X').

*如果字段列表不为空
      LOOP AT _TAB_LIST_DEBUG-FCAT INTO _WA_FCAT.
        TRY.
            LR_COLUMN  ?= LR_COLUMNS->GET_COLUMN( _WA_FCAT-FIELDNAME ).
            LR_COLUMN->SET_LONG_TEXT( _WA_FCAT-SCRTEXT_L ).
            LR_COLUMN->SET_SHORT_TEXT( _WA_FCAT-SCRTEXT_S ).
            LR_COLUMN->SET_MEDIUM_TEXT( _WA_FCAT-SCRTEXT_M ).
          CATCH  CX_SALV_NOT_FOUND .
        ENDTRY.
      ENDLOOP.

*显示工具栏
      LR_FUNCTIONS = GR_TABLE->GET_FUNCTIONS( ).
      LR_FUNCTIONS->SET_ALL( 'X' ).
      GR_TABLE->DISPLAY( ).
    ENDLOOP.
  ENDIF.

ENDFORM.                    "frm_popup_to_select


*----------------------------------------------------------------------*
*以下是另外一个显示系统的定义

TYPE-POOLS: ABAP, SLIS.
*----------------------------------------------------------------------*

*这个类可以用于显示任何的内表内容
*----------------------------------------------------------------------*
*   LCL_TABLE_DISPLAY DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_TABLE_DISPLAY DEFINITION.

  PUBLIC SECTION.

*------------------------------类方法----------------------------------
    CLASS-METHODS:
*LIST方式显示
    DISPLAY_LIST          IMPORTING IN_TABNAME  TYPE ANY,
* GRID方式显示
    DISPLAY_GRID          IMPORTING IN_TABNAME  TYPE ANY,
*层次方式显示
    DISPLAY_HIER          IMPORTING IN_HEADER   TYPE ANY
                                    IN_DETAIL   TYPE ANY
                                    IN_LEVEL    TYPE I OPTIONAL,
*   以块的方式显示
    SET_BLOCK_MODE        IMPORTING IN_MODE     TYPE C,
    SET_BLOCK_TEXT        IMPORTING IN_TEXT     TYPE ANY,
    END_BLOCK_LIST        IMPORTING IN_PRINT    TYPE SLIS_PRINT_ALV OPTIONAL
                          RETURNING VALUE(SUCCESS) TYPE CHAR1
                          EXCEPTIONS DISPLAY_ERROR,
*根据表名创建
    CREATE_TABLE          IMPORTING IN_TABNAME  TYPE TABNAME
                          RETURNING VALUE(OUT_TABLE)    TYPE REF TO LCL_TABLE_DISPLAY
                          EXCEPTIONS CREATE_ERROR,

    REMOVE_TABLE          IMPORTING IN_TABLENAME TYPE TABNAME
                          EXCEPTIONS NO_PARAMETER,


*在本地缓存里查找是否已经存在用于显示的对象。
    GET_EXISTING_TABLE    IMPORTING   IN_TABNAME  TYPE ANY OPTIONAL
                                      IN_REPID    TYPE ANY OPTIONAL
                                      IN_STRUC    TYPE ANY OPTIONAL
                          RETURNING VALUE(OUT_TABLE) TYPE REF TO LCL_TABLE_DISPLAY
                          EXCEPTIONS
                            NO_PARAMETER
                            NOT_FOUND,
*   释放对象。
    REFRESH_OBJECTS.

*--------------------------------实例方法--------------------------------
    METHODS:
*    构造函数
       CONSTRUCTOR         IMPORTING IN_DATA TYPE STANDARD TABLE
                           EXCEPTIONS
                           CASTING_ERROR
                           EMPTY_FIELDCAT.

    METHODS:
*    获取字段列表
    GET_ALV_FIELDCAT   RETURNING VALUE(OUT_FIELDCAT) TYPE SLIS_T_FIELDCAT_ALV,
    GET_ALV_FIELDCAT_COUNT   RETURNING VALUE(FIELDCOUNT) TYPE INT4.

    METHODS:

    SET_TABLE_NAME     IMPORTING IN_TABNAME  TYPE ANY,

    SET_ALV_TITLE      IMPORTING IN_TITLE    TYPE ANY,

    SET_ALV_FIELDCAT   IMPORTING IN_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,

    SET_ALV_FIELDNOOUT IMPORTING IN_FIELD    TYPE ANY
                                 IN_NOOUT    TYPE C OPTIONAL
                                 IN_TECH     TYPE C OPTIONAL,
    SET_ALV_FIELDEDIT  IMPORTING IN_FIELD    TYPE ANY
                                 IN_EDIT     TYPE C OPTIONAL,

    SET_ALV_FIELDTEXT  IMPORTING IN_FIELD    TYPE ANY
                                 IN_FTEXT    TYPE ANY,

    SET_ALV_FIELDSUM   IMPORTING IN_FIELD    TYPE ANY
                                 IN_DOSUM    TYPE C OPTIONAL,

    SET_ALV_LINEBREAK  IMPORTING IN_FIELD    TYPE ANY,

    SET_ALV_SETTINGS   IMPORTING IN_SETTINGS TYPE ANY,

    SET_ALV_LAYOUT     IMPORTING IN_LAYOUT   TYPE ANY,

    SET_ALV_PRINT      IMPORTING IN_PRINT    TYPE ANY,

    SET_ALV_SORTING    IMPORTING IN_FIELD    TYPE ANY
                                 IN_DESC     TYPE C OPTIONAL
                                 IN_GROUP    TYPE ANY OPTIONAL
                                 IN_SUBTOT   TYPE C OPTIONAL,

    SET_ALV_KEYS       IMPORTING IN_LEVEL    TYPE I
                                 IN_KEY      TYPE C OPTIONAL,

    SET_ALV_EVENT      IMPORTING IN_NAME     TYPE ANY
                                 IN_FORM     TYPE ANY,
                                 SET_ALL_EVENTS.
  PROTECTED SECTION.
    DATA: G_TABLE_TYPE TYPE SLIS_LIST_TYPE.

    DATA: G_TITLE      TYPE LVC_TITLE,
          GT_FCAT      TYPE SLIS_T_FIELDCAT_ALV,
          GS_SETT      TYPE LVC_S_GLAY,
          GS_LAYO      TYPE SLIS_LAYOUT_ALV,
          GT_SORT      TYPE SLIS_T_SORTINFO_ALV,
          GT_EVNT      TYPE SLIS_T_EVENT,
          GS_PRIN      TYPE SLIS_PRINT_ALV.

    CLASS-METHODS: OUTPUT_HIERARCHY EXCEPTIONS DISPLAY_ERROR.
    CLASS-METHODS TRANSFORM CHANGING
                T_DFIES TYPE DDFIELDS OPTIONAL
                S_DFIES TYPE DFIES OPTIONAL .



  PRIVATE SECTION.

    CLASS-DATA:
*         内部所有用于显示的内表的集合。
          GT_TABLE_OBJ   TYPE TABLE OF REF TO LCL_TABLE_DISPLAY,
          G_HEADER_TABLE TYPE REF TO LCL_TABLE_DISPLAY,
          G_DETAIL_TABLE TYPE REF TO LCL_TABLE_DISPLAY.

    CLASS-DATA:
         G_VARIANT_LEVEL TYPE I.

    CLASS-DATA:
          G_BLOCK_MODE  TYPE C,
          G_BLOCK_TEXT  TYPE SLIS_TEXT40.

    TYPES: BEGIN OF TY_DEFIN,
      FIELDNAME     TYPE FIELDNAME,
      REF_TABNAME   TYPE TABNAME,
      REF_FIELDNAME TYPE FIELDNAME,
    END OF TY_DEFIN.

    DATA: G_REPID  TYPE REPID,
          G_STRUC  TYPE TABNAME,
          G_TABLE  TYPE TABNAME.

    DATA: GT_DATA  TYPE REF TO DATA.

    DATA: GT_DEFIN TYPE TABLE OF TY_DEFIN,
          G_LEVEL  TYPE TABNAME.

*获取结构的字段列表
    CLASS-METHODS:

     READ_STRUCTDESCR
              IMPORTING  R_STRUCTDESCR TYPE REF TO CL_ABAP_STRUCTDESCR
              RETURNING VALUE(T_DFIES) TYPE DDFIELDS .



    METHODS: OUTPUT_TABLE IMPORTING MODE TYPE C
    EXCEPTIONS DISPLAY_ERROR,
      OUTPUT_LIST,
      OUTPUT_GRID.



    METHODS:
    INIT_BLOCK_LIST,

    FILL_FIELDCAT
                    IMPORTING REPID TYPE REPID
                              STRUC TYPE TABNAME
                    CHANGING  FCAT  TYPE SLIS_T_FIELDCAT_ALV
                    EXCEPTIONS NO_DEFINITION,

    FILL_FIELDCAT2
                    IMPORTING REPID TYPE REPID
                              STRUC TYPE TABNAME
                          TYPE_DESC TYPE REF TO CL_ABAP_STRUCTDESCR
                    CHANGING FCAT   TYPE SLIS_T_FIELDCAT_ALV
                    EXCEPTIONS NO_DEFINITION,

    GET_DEFINITION
                    IMPORTING REPID TYPE REPID
                              STRUC TYPE TABNAME
                    CHANGING  ABAP  TYPE RSFB_SOURCE,

    RECURSIVE_DEFINITION
                    IMPORTING REPID TYPE REPID
                    CHANGING  ABAP  TYPE RSFB_SOURCE,

    MAP_STRUCTURE
                    IMPORTING SOURCE TYPE ANY
                    CHANGING DESTIN TYPE ANY,

    GET_DEFAULT_VARIANT
                    CHANGING OUT_VARIANT TYPE DISVARIANT.

ENDCLASS.                    "lcl_table_display DEFINITION

*&---------------------------------------------------------------------*
*&      Form  DEBUG_DISPLAY_ALL_BLOCK
*&---------------------------------------------------------------------*
*   全部的内表一起显示。
*----------------------------------------------------------------------*
FORM _DISPLAY_ALL_BLOCK_DEBUG.

  DATA: OB_TABLE TYPE REF TO  LCL_TABLE_DISPLAY.
  DATA: LAYOUT TYPE SLIS_LAYOUT_ALV .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL METHOD LCL_TABLE_DISPLAY=>SET_BLOCK_MODE( 'X' ).

  LOOP AT _TAB_LIST_DEBUG.

    OB_TABLE = LCL_TABLE_DISPLAY=>CREATE_TABLE( _TAB_LIST_DEBUG-TABNAME ).

    _TAB_LIST_DEBUG-LINES = OB_TABLE->GET_ALV_FIELDCAT_COUNT( ).

    IF _TAB_LIST_DEBUG-LINES > 99.
      LCL_TABLE_DISPLAY=>REMOVE_TABLE( IN_TABLENAME = _TAB_LIST_DEBUG-TABNAME ).
      CONTINUE.
    ENDIF.

    OB_TABLE->SET_ALV_LAYOUT( IN_LAYOUT = LAYOUT  ).

*更新用户设置的字段的名字。
    LOOP AT _TAB_LIST_DEBUG-FCAT INTO _WA_FCAT.
      CALL METHOD OB_TABLE->SET_ALV_FIELDTEXT( IN_FIELD = _WA_FCAT-FIELDNAME
        IN_FTEXT = _WA_FCAT-SCRTEXT_S ).
    ENDLOOP.


    CALL METHOD LCL_TABLE_DISPLAY=>DISPLAY_GRID( _TAB_LIST_DEBUG-TABNAME ).
  ENDLOOP.


  DISPLAYSUCCESS =  LCL_TABLE_DISPLAY=>END_BLOCK_LIST( ).

  IF DISPLAYSUCCESS = '' AND SHOWED = ''.
    PERFORM  _TABLE_TO_SELECT_DEBUG .
  ENDIF.



ENDFORM.                    "DEBUG_DISPLAY_ALL_BLOCK



*----------------------------------------------------------------------*
*   LCL_TABLE_DISPLAY IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_TABLE_DISPLAY IMPLEMENTATION.
***
*   Create table display
***
  METHOD CONSTRUCTOR.
    DATA: L_OBJECT TYPE REF TO LCL_TABLE_DISPLAY.
    DATA: LS_DATA  TYPE REF TO DATA.
    DATA: OB_DESC  TYPE REF TO CL_ABAP_STRUCTDESCR.
    DATA: L_FOUND  TYPE C,
          L_ABSOL  TYPE CHAR200,
          L_REPID  TYPE REPID,
          L_STRUC  TYPE TABNAME.
    FIELD-SYMBOLS: <TABLE> TYPE STANDARD TABLE,
                   <STRUC> TYPE ANY.
*   Get data and store it into attribute
    CREATE DATA ME->GT_DATA LIKE IN_DATA.
    ASSIGN ME->GT_DATA->* TO <TABLE>.

*   把传入的内表赋值给本地变量。
    <TABLE> = IN_DATA.
*   Get global data definition
*   创建一个行结构。
    CREATE DATA LS_DATA LIKE LINE OF <TABLE>.
    ASSIGN LS_DATA->* TO <STRUC>.

    CATCH SYSTEM-EXCEPTIONS ASSIGN_CASTING_ILLEGAL_CAST = 1.
      OB_DESC ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <STRUC> ).
    ENDCATCH.

    IF SY-SUBRC = 1.
      RAISE CASTING_ERROR.
    ENDIF.

*   Get program name and main type used to define table
    L_ABSOL = OB_DESC->ABSOLUTE_NAME.
    SPLIT L_ABSOL AT '\TYPE=' INTO L_REPID L_STRUC.

    SHIFT L_REPID UP TO '='.
    SHIFT L_REPID.

*    CHECK L_STRUC NP '%_*'.

    L_REPID = SY-REPID.

**   Set attributes
    ME->G_REPID = L_REPID.
    ME->G_STRUC = L_STRUC.
*
    ME->G_TABLE = L_STRUC.
*    REPLACE 'TY' WITH 'WT' INTO ME->G_TABLE.

*   Field catalog
*   检查本地是否已经存在相同名字的内表定义。
    CALL METHOD LCL_TABLE_DISPLAY=>GET_EXISTING_TABLE
      EXPORTING
        IN_REPID  = L_REPID
        IN_STRUC  = L_STRUC
      RECEIVING
        OUT_TABLE = L_OBJECT
      EXCEPTIONS
        NOT_FOUND = 1.

    IF SY-SUBRC = 0.
      ME->GT_FCAT = L_OBJECT->GET_ALV_FIELDCAT( ).
      CALL METHOD SET_TABLE_NAME
        EXPORTING
          IN_TABNAME = ME->G_TABLE.
    ELSE . "在字典中已经有定义
      CALL METHOD FILL_FIELDCAT2
        EXPORTING
          REPID     = L_REPID
          STRUC     = L_STRUC
          TYPE_DESC = OB_DESC
        CHANGING
          FCAT      = ME->GT_FCAT.

      IF ME->GT_FCAT IS INITIAL.
        RAISE EMPTY_FIELDCAT.
      ENDIF.
    ENDIF.

*   Keep list of tables
*   保存内表。
    APPEND ME TO GT_TABLE_OBJ.
  ENDMETHOD.                    "constructor

***
*   Display table in ALV list
***
  METHOD DISPLAY_LIST.
    DATA: L_OBJECT  TYPE REF TO LCL_TABLE_DISPLAY,
          L_TABNAME TYPE TABNAME,
          L_FOUND   TYPE C.

    L_TABNAME = IN_TABNAME.
    LOOP AT GT_TABLE_OBJ INTO L_OBJECT.
      IF L_OBJECT->G_TABLE = L_TABNAME.
        L_FOUND = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF L_FOUND IS INITIAL.
      L_OBJECT = LCL_TABLE_DISPLAY=>CREATE_TABLE( L_TABNAME ).
      IF G_BLOCK_MODE IS INITIAL.
        L_OBJECT->G_TABLE_TYPE = 4.
      ELSE.
        L_OBJECT->G_TABLE_TYPE = 2.
      ENDIF.
      CALL METHOD L_OBJECT->SET_ALL_EVENTS.
    ENDIF.
    CALL METHOD L_OBJECT->OUTPUT_LIST.
  ENDMETHOD.                    "display_list
***
*   Display table in ALV grid
***
  METHOD DISPLAY_GRID.
    DATA: L_OBJECT  TYPE REF TO LCL_TABLE_DISPLAY,
          L_TABNAME TYPE TABNAME,
          L_FOUND   TYPE C.

    L_TABNAME = IN_TABNAME.
    LOOP AT GT_TABLE_OBJ INTO L_OBJECT.
      IF L_OBJECT->G_TABLE = L_TABNAME.
        L_FOUND = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF L_FOUND IS INITIAL.
      L_OBJECT = LCL_TABLE_DISPLAY=>CREATE_TABLE( L_TABNAME ).
      IF G_BLOCK_MODE IS INITIAL.
        L_OBJECT->G_TABLE_TYPE = 4.
      ELSE.
        L_OBJECT->G_TABLE_TYPE = 2.
      ENDIF.
      CALL METHOD L_OBJECT->SET_ALL_EVENTS.
    ENDIF.
    IF G_BLOCK_MODE IS INITIAL.
      CALL METHOD L_OBJECT->OUTPUT_GRID.
    ELSE.
      CALL METHOD L_OBJECT->OUTPUT_LIST.
    ENDIF.
  ENDMETHOD.                    "display_grid
***
*   Display tables in ALV hierarchy
***
  METHOD DISPLAY_HIER.
    DATA: L_TABNAM1 TYPE TABNAME,
          L_TABNAM2 TYPE TABNAME,
          LT_FCAT1  TYPE SLIS_T_FIELDCAT_ALV,
          LT_FCAT2  TYPE SLIS_T_FIELDCAT_ALV,
          LS_FCAT1  TYPE SLIS_FIELDCAT_ALV,
          LS_FCAT2  TYPE SLIS_FIELDCAT_ALV.

    L_TABNAM1 = IN_HEADER.
    L_TABNAM2 = IN_DETAIL.
    CALL METHOD LCL_TABLE_DISPLAY=>GET_EXISTING_TABLE
      EXPORTING
        IN_TABNAME = L_TABNAM1
      RECEIVING
        OUT_TABLE  = G_HEADER_TABLE
      EXCEPTIONS
        NOT_FOUND  = 1.
    IF SY-SUBRC NE 0.
      G_HEADER_TABLE = LCL_TABLE_DISPLAY=>CREATE_TABLE( L_TABNAM1 ).
      IF G_BLOCK_MODE IS INITIAL.
        G_HEADER_TABLE->G_TABLE_TYPE = 1.
      ELSE.
        G_HEADER_TABLE->G_TABLE_TYPE = 3.
      ENDIF.
      CALL METHOD G_HEADER_TABLE->SET_ALL_EVENTS.
    ENDIF.
    CALL METHOD LCL_TABLE_DISPLAY=>GET_EXISTING_TABLE
      EXPORTING
        IN_TABNAME = L_TABNAM2
      RECEIVING
        OUT_TABLE  = G_DETAIL_TABLE
      EXCEPTIONS
        NOT_FOUND  = 1.
    IF SY-SUBRC NE 0.
      G_DETAIL_TABLE = LCL_TABLE_DISPLAY=>CREATE_TABLE( L_TABNAM2 ).
    ENDIF.

*   Set key fields
    IF IN_LEVEL IS INITIAL.
      LT_FCAT1 = G_HEADER_TABLE->GET_ALV_FIELDCAT( ).
      LT_FCAT2 = G_DETAIL_TABLE->GET_ALV_FIELDCAT( ).
      LOOP AT LT_FCAT1 INTO LS_FCAT1.
        LS_FCAT1-KEY = SPACE.
        LOOP AT LT_FCAT2 INTO LS_FCAT2
        WHERE FIELDNAME = LS_FCAT1-FIELDNAME.
          LS_FCAT2-KEY = SPACE.
          LS_FCAT2-KEY_SEL = 'X'.
          LS_FCAT2-TECH = 'X'.
          MODIFY LT_FCAT2 FROM LS_FCAT2 TRANSPORTING KEY.
        ENDLOOP.
        IF SY-SUBRC = 0.
          LS_FCAT1-KEY = 'X'.
        ENDIF.
        MODIFY LT_FCAT1 FROM LS_FCAT1 TRANSPORTING KEY.
      ENDLOOP.
      CALL METHOD G_HEADER_TABLE->SET_ALV_FIELDCAT
        EXPORTING
          IN_FIELDCAT = LT_FCAT1.
      CALL METHOD G_DETAIL_TABLE->SET_ALV_FIELDCAT
        EXPORTING
          IN_FIELDCAT = LT_FCAT2.
    ELSE.
      CALL METHOD G_HEADER_TABLE->SET_ALV_KEYS
        EXPORTING
          IN_LEVEL = IN_LEVEL.
      CALL METHOD G_DETAIL_TABLE->SET_ALV_KEYS
        EXPORTING
          IN_LEVEL = IN_LEVEL
          IN_KEY   = SPACE.
    ENDIF.

    CALL METHOD OUTPUT_HIERARCHY.
  ENDMETHOD.                    "display_hier
***
*   Set block mode
***
  METHOD SET_BLOCK_MODE.
    G_BLOCK_MODE = IN_MODE.
  ENDMETHOD.                    "set_block_mode
***
*   Set block text
***
  METHOD SET_BLOCK_TEXT.
    G_BLOCK_TEXT = IN_TEXT.
  ENDMETHOD.                    "set_block_text
***
*   Create new table
*   根据表的名称构造一个用于显示的ALV表格。
***
  METHOD CREATE_TABLE.
    DATA: L_OBJECT TYPE REF TO LCL_TABLE_DISPLAY.
    DATA: TABLENAME TYPE TABNAME.

    FIELD-SYMBOLS: <LOCAL> TYPE STANDARD TABLE.

    IF IN_TABNAME IS INITIAL.
      RAISE CREATE_ERROR.
    ENDIF.

    ASSIGN (IN_TABNAME) TO <LOCAL>  .

    IF NOT <LOCAL> IS ASSIGNED.
      RAISE CREATE_ERROR.
    ENDIF.

    CREATE OBJECT L_OBJECT
      EXPORTING
        IN_DATA        = <LOCAL>
      EXCEPTIONS
        CASTING_ERROR  = 1
        EMPTY_FIELDCAT = 2.

    IF SY-SUBRC NE 0.
      RAISE CREATE_ERROR.
    ENDIF.

    CALL METHOD L_OBJECT->SET_TABLE_NAME
      EXPORTING
        IN_TABNAME = IN_TABNAME.

*   Default print options
    L_OBJECT->GS_PRIN-NO_PRINT_SELINFOS  = 'X'.
    L_OBJECT->GS_PRIN-NO_COVERPAGE       = 'X'.
    L_OBJECT->GS_PRIN-NO_PRINT_LISTINFOS = 'X'.

    OUT_TABLE = L_OBJECT.
  ENDMETHOD.                    "create_table

  METHOD REMOVE_TABLE.
    DATA L_FOUND   TYPE C.
    DATA: L_OBJECT  TYPE REF TO LCL_TABLE_DISPLAY.
    IF IN_TABLENAME IS INITIAL.
      RAISE NO_PARAMETER.
    ENDIF.

    LOOP AT GT_TABLE_OBJ INTO L_OBJECT.
      IF L_OBJECT->G_TABLE = IN_TABLENAME.
        L_FOUND = 'X'.
*        DELETE GT_TABLE_OBJ FROM L_OBJECT .
        DELETE GT_TABLE_OBJ.
        FREE L_OBJECT.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD .                    "REMOVE_TABLE

***
*   Get existing table
***
  METHOD GET_EXISTING_TABLE.
    DATA: L_OBJECT  TYPE REF TO LCL_TABLE_DISPLAY,
          L_TABNAME TYPE TABNAME,
          L_REPID   TYPE REPID,
          L_STRUC   TYPE TABNAME,
          L_FOUND   TYPE C.

    L_TABNAME = IN_TABNAME.
    L_REPID   = IN_REPID.
    L_STRUC   = IN_STRUC.

    IF L_TABNAME IS INITIAL.
      IF L_REPID IS INITIAL AND
      L_STRUC IS INITIAL.
        RAISE NO_PARAMETER.
      ELSE.
*       Get last existing table with same definition
        LOOP AT GT_TABLE_OBJ INTO L_OBJECT.
          IF L_OBJECT->G_REPID = L_REPID AND
          L_OBJECT->G_STRUC = L_STRUC.
            L_FOUND = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
*     Get last existing table with same name
      LOOP AT GT_TABLE_OBJ INTO L_OBJECT.
        IF L_OBJECT->G_TABLE = L_TABNAME.
          L_FOUND = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF L_FOUND IS INITIAL.
      RAISE NOT_FOUND.
    ELSE.
      OUT_TABLE = L_OBJECT.
    ENDIF.
  ENDMETHOD.                    "get_existing_table

***
*   Output list
***
  METHOD OUTPUT_LIST.
    CALL METHOD OUTPUT_TABLE
      EXPORTING
        MODE = 'L'.
  ENDMETHOD.                    "output_list
***
*   Output grid
***
  METHOD OUTPUT_GRID.
    CALL METHOD OUTPUT_TABLE
      EXPORTING
        MODE = 'G'.
  ENDMETHOD.                    "output_grid
***
*   Output table
***
  METHOD OUTPUT_TABLE.
    DATA: L_OBJECT TYPE REF TO LCL_TABLE_DISPLAY.

    DATA: LS_VARI  TYPE DISVARIANT.

    FIELD-SYMBOLS: <TABLE> TYPE STANDARD TABLE.

    ASSIGN ME->GT_DATA->* TO <TABLE>.

    IF NOT G_BLOCK_MODE IS INITIAL.
      READ TABLE GT_TABLE_OBJ INTO L_OBJECT INDEX 1.
      IF SY-SUBRC = 0.
        IF L_OBJECT->G_TABLE = ME->G_TABLE.
          CALL METHOD INIT_BLOCK_LIST.
        ENDIF.
      ENDIF.
    ENDIF.

*   Get default user variant
    CALL METHOD GET_DEFAULT_VARIANT
      CHANGING
        OUT_VARIANT = LS_VARI.

    ME->GS_LAYO-COLWIDTH_OPTIMIZE = 'X'.
*   Display table contents
    IF MODE = 'G'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          I_CALLBACK_PROGRAM = ME->G_REPID
          I_GRID_TITLE       = ME->G_TITLE
          I_GRID_SETTINGS    = ME->GS_SETT
          IS_LAYOUT          = ME->GS_LAYO
          IT_FIELDCAT        = ME->GT_FCAT
          IT_SORT            = ME->GT_SORT
          I_SAVE             = 'U'
          IS_VARIANT         = LS_VARI
          IT_EVENTS          = ME->GT_EVNT
          IS_PRINT           = ME->GS_PRIN
        TABLES
          T_OUTTAB           = <TABLE>
        EXCEPTIONS
          PROGRAM_ERROR      = 1
          OTHERS             = 2.
      IF SY-SUBRC <> 0.
        RAISE DISPLAY_ERROR.
      ENDIF.
      CALL METHOD REFRESH_OBJECTS.
    ELSE.
      IF G_BLOCK_MODE IS INITIAL.
        CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
          EXPORTING
            I_CALLBACK_PROGRAM = ME->G_REPID
            IS_LAYOUT          = ME->GS_LAYO
            IT_FIELDCAT        = ME->GT_FCAT
            IT_SORT            = ME->GT_SORT
            I_SAVE             = 'U'
            IS_VARIANT         = LS_VARI
            IT_EVENTS          = ME->GT_EVNT
            IS_PRINT           = ME->GS_PRIN
          TABLES
            T_OUTTAB           = <TABLE>
          EXCEPTIONS
            OTHERS             = 0.
        IF SY-SUBRC <> 0.
          RAISE DISPLAY_ERROR.
        ENDIF.
        CALL METHOD REFRESH_OBJECTS.
      ELSE.
        CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
          EXPORTING
            IS_LAYOUT                  = ME->GS_LAYO
            IT_FIELDCAT                = ME->GT_FCAT
            I_TABNAME                  = ME->G_TABLE
            IT_EVENTS                  = ME->GT_EVNT
            IT_SORT                    = ME->GT_SORT
            I_TEXT                     = G_BLOCK_TEXT
          TABLES
            T_OUTTAB                   = <TABLE>
          EXCEPTIONS
            PROGRAM_ERROR              = 1
            MAXIMUM_OF_APPENDS_REACHED = 2
            OTHERS                     = 3.
        IF SY-SUBRC <> 0.
          RAISE DISPLAY_ERROR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "output_table
***
*   Output hierarchy
***
  METHOD OUTPUT_HIERARCHY.
    DATA: L_OBJECT TYPE REF TO LCL_TABLE_DISPLAY.

    DATA: LT_FCAT  TYPE SLIS_T_FIELDCAT_ALV,
          LT_SORT  TYPE SLIS_T_SORTINFO_ALV,
          LS_FCAT  TYPE SLIS_FIELDCAT_ALV,
          LS_VARI  TYPE DISVARIANT,
          LS_KEYI  TYPE SLIS_KEYINFO_ALV.

    DATA: L_INDEX  TYPE NUMC2,
          L_FIELD  TYPE FIELDNAME.

    FIELD-SYMBOLS: <HEAD> TYPE STANDARD TABLE,
    <DETA> TYPE STANDARD TABLE,
    <TARG> TYPE ANY.

    ASSIGN G_HEADER_TABLE->GT_DATA->* TO <HEAD>.
    ASSIGN G_DETAIL_TABLE->GT_DATA->* TO <DETA>.

*   Set key fields as common fields between header and detail
    LOOP AT G_HEADER_TABLE->GT_FCAT INTO LS_FCAT
    WHERE KEY = 'X'.
      L_INDEX = L_INDEX + 1.
*     Create link
      CONCATENATE 'HEADER' L_INDEX INTO L_FIELD.
      ASSIGN COMPONENT L_FIELD OF STRUCTURE LS_KEYI TO <TARG>.
      IF SY-SUBRC = 0.
        <TARG> = LS_FCAT-FIELDNAME.
        UNASSIGN <TARG>.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF G_HEADER_TABLE->GT_FCAT TO LT_FCAT.
    APPEND LINES OF G_DETAIL_TABLE->GT_FCAT TO LT_FCAT.

    APPEND LINES OF G_HEADER_TABLE->GT_SORT TO LT_SORT.
    APPEND LINES OF G_DETAIL_TABLE->GT_SORT TO LT_SORT.

    IF NOT G_BLOCK_MODE IS INITIAL.
      READ TABLE GT_TABLE_OBJ INTO L_OBJECT INDEX 1.
      IF SY-SUBRC = 0.
        IF L_OBJECT->G_TABLE = G_HEADER_TABLE->G_TABLE.
          CALL METHOD G_HEADER_TABLE->INIT_BLOCK_LIST.
        ENDIF.
      ENDIF.
    ENDIF.

*   Get default user variant
    CALL METHOD G_HEADER_TABLE->GET_DEFAULT_VARIANT
      CHANGING
        OUT_VARIANT = LS_VARI.

    IF G_BLOCK_MODE IS INITIAL.
      CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
        EXPORTING
          I_CALLBACK_PROGRAM = G_HEADER_TABLE->G_REPID
          IS_LAYOUT          = G_HEADER_TABLE->GS_LAYO
          IT_FIELDCAT        = LT_FCAT
          IT_SORT            = LT_SORT
          I_SAVE             = 'U'
          IS_VARIANT         = LS_VARI
          IT_EVENTS          = G_HEADER_TABLE->GT_EVNT
          I_TABNAME_HEADER   = G_HEADER_TABLE->G_TABLE
          I_TABNAME_ITEM     = G_DETAIL_TABLE->G_TABLE
          IS_KEYINFO         = LS_KEYI
          IS_PRINT           = G_HEADER_TABLE->GS_PRIN
        TABLES
          T_OUTTAB_HEADER    = <HEAD>
          T_OUTTAB_ITEM      = <DETA>
        EXCEPTIONS
          PROGRAM_ERROR      = 1
          OTHERS             = 2.
      IF SY-SUBRC <> 0.
        RAISE DISPLAY_ERROR.
      ENDIF.
      CALL METHOD REFRESH_OBJECTS.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_HS_APPEND'
        EXPORTING
          IS_LAYOUT                  = G_HEADER_TABLE->GS_LAYO
          IT_FIELDCAT                = LT_FCAT
          IS_KEYINFO                 = LS_KEYI
          I_HEADER_TABNAME           = G_HEADER_TABLE->G_TABLE
          I_ITEM_TABNAME             = G_DETAIL_TABLE->G_TABLE
          IT_EVENTS                  = G_HEADER_TABLE->GT_EVNT
          IT_SORT                    = LT_SORT
          I_TEXT                     = G_BLOCK_TEXT
        TABLES
          T_OUTTAB_HEADER            = <HEAD>
          T_OUTTAB_ITEM              = <DETA>
        EXCEPTIONS
          PROGRAM_ERROR              = 1
          MAXIMUM_OF_APPENDS_REACHED = 2
          OTHERS                     = 3.
      IF SY-SUBRC <> 0.
        RAISE DISPLAY_ERROR.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "output_hierarchy
***
*   Init block list
***
  METHOD INIT_BLOCK_LIST.
    DATA:  LS_EVNT1 TYPE SLIS_ALV_EVENT,
          LS_EVNT2 TYPE SLIS_ALV_EVENT.

*   Events for whole list display
    CONCATENATE 'F_' SLIS_EV_PF_STATUS_SET '_BLOCK'
    INTO LS_EVNT1-FORM.
    CONCATENATE 'F_' SLIS_EV_USER_COMMAND '_BLOCK'
    INTO LS_EVNT2-FORM.

*   Initialization of block list
    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
      EXPORTING
        I_CALLBACK_PROGRAM       = ME->G_REPID
        I_CALLBACK_PF_STATUS_SET = LS_EVNT1-FORM
        I_CALLBACK_USER_COMMAND  = LS_EVNT2-FORM.
  ENDMETHOD.                    "init_block_list
***
*   End of block list
***
  METHOD END_BLOCK_LIST.
    DATA: L_OBJECT TYPE REF TO LCL_TABLE_DISPLAY,
          LS_PRINT TYPE SLIS_PRINT_ALV.

    IF LINES( GT_TABLE_OBJ ) = 0.
      SUCCESS = ''.
      RETURN.
    ENDIF.
    CHECK NOT G_BLOCK_MODE IS INITIAL.
    IF IN_PRINT IS SUPPLIED.
      LS_PRINT = IN_PRINT.
    ELSE.
      READ TABLE GT_TABLE_OBJ INTO L_OBJECT INDEX 1.
      LS_PRINT = L_OBJECT->GS_PRIN.
    ENDIF.
    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
      EXPORTING
        IS_PRINT      = LS_PRINT
      EXCEPTIONS
        PROGRAM_ERROR = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
      RAISE DISPLAY_ERROR.
    ENDIF.
    CALL METHOD REFRESH_OBJECTS.
    SUCCESS = 'X'.
  ENDMETHOD.                    "end_block_list
***
*   Refresh table of objects
***
  METHOD REFRESH_OBJECTS.
    FREE: GT_TABLE_OBJ.
  ENDMETHOD.                    "refresh_objects


  METHOD FILL_FIELDCAT2.

    DATA:T_DFIES TYPE DDFIELDS,
          S_DFIES TYPE DFIES..

    T_DFIES = READ_STRUCTDESCR( TYPE_DESC ).

    DATA: LS_FCAT TYPE SLIS_FIELDCAT_ALV .
    LOOP AT T_DFIES INTO S_DFIES.

      MOVE-CORRESPONDING S_DFIES TO  LS_FCAT.
      LS_FCAT-COL_POS = SY-TABIX.
      LS_FCAT-TABNAME = G_TABLE.
      LS_FCAT-SELTEXT_S = S_DFIES-SCRTEXT_S.
      LS_FCAT-SELTEXT_M = S_DFIES-SCRTEXT_M.
      LS_FCAT-SELTEXT_L = S_DFIES-SCRTEXT_L.
      APPEND LS_FCAT TO FCAT.

    ENDLOOP.

*    MODIFY FCAT FROM LS_FCAT.
  ENDMETHOD.                    "FILL_FIELDCAT2

***
*   Fill field catalog
***
  METHOD FILL_FIELDCAT.
    DATA: LT_ABAP   TYPE RSFB_SOURCE.

    DATA: LS_DEFIN  TYPE TY_DEFIN.

    DATA: LT_DFIES  TYPE TABLE OF DFIES,
          LS_DFIES  TYPE DFIES,
          LS_DD04V  TYPE DD04V,
          LS_DD01V  TYPE DD01V,
          L_FLONG   TYPE DFIES-LFIELDNAME,
          L_DNAME   TYPE DFIES-DOMNAME.

    DATA: LS_FCAT   TYPE SLIS_FIELDCAT_ALV,
          LS_FCAT2  TYPE SLIS_FIELDCAT_ALV.

    DATA: L_INDEX   TYPE I,
          L_NBFLD   TYPE I.

    FREE: ME->GT_DEFIN.

*   Process data definition
    CALL METHOD GET_DEFINITION
      EXPORTING
        REPID = REPID
        STRUC = STRUC
      CHANGING
        ABAP  = LT_ABAP.

*   Process sub levels if required
    CALL METHOD RECURSIVE_DEFINITION
      EXPORTING
        REPID = REPID
      CHANGING
        ABAP  = LT_ABAP.

    IF ME->GT_DEFIN IS INITIAL.
      RAISE NO_DEFINITION.
    ENDIF.

    LOOP AT ME->GT_DEFIN INTO LS_DEFIN.
      CLEAR: LS_FCAT.
      MOVE-CORRESPONDING LS_DEFIN TO LS_FCAT.
*     Retrieve info about this field
      FREE: LS_DFIES, LS_DD04V, LS_DD01V, L_DNAME.
      L_FLONG = LS_FCAT-REF_FIELDNAME.
      SET LOCALE LANGUAGE 'E'.
      TRANSLATE: LS_FCAT-REF_TABNAME   TO UPPER CASE,
      LS_FCAT-REF_FIELDNAME TO UPPER CASE,
      L_FLONG               TO UPPER CASE.
      IF NOT LS_FCAT-REF_TABNAME IS INITIAL.
*       Try to get info about field in table
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            TABNAME        = LS_FCAT-REF_TABNAME
            FIELDNAME      = LS_FCAT-REF_FIELDNAME
            LFIELDNAME     = L_FLONG
          IMPORTING
            DFIES_WA       = LS_DFIES
          EXCEPTIONS
            NOT_FOUND      = 1
            INTERNAL_ERROR = 2
            OTHERS         = 3.
        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING LS_DFIES TO LS_FCAT.
          LS_FCAT-FIELDNAME = LS_DEFIN-FIELDNAME.
          MOVE: LS_DFIES-KEYFLAG   TO LS_FCAT-KEY,
          LS_DFIES-SCRTEXT_M TO LS_FCAT-SELTEXT_L,
          LS_DFIES-DOMNAME   TO L_DNAME.
        ENDIF.
      ELSE.
*       Try to get info about structure
        LS_DEFIN-REF_TABNAME = LS_DEFIN-REF_FIELDNAME.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            TABNAME   = LS_DEFIN-REF_TABNAME
          TABLES
            DFIES_TAB = LT_DFIES
          EXCEPTIONS
            OTHERS    = 0.
        IF NOT LT_DFIES IS INITIAL.
*         Process fields of this structure
          LOOP AT LT_DFIES INTO LS_DFIES.
            CLEAR: LS_FCAT.
            MOVE-CORRESPONDING LS_DFIES TO LS_FCAT.
            IF LS_DEFIN-FIELDNAME NE 'INCLUDE'.
              CONCATENATE LS_DEFIN-FIELDNAME LS_FCAT-FIELDNAME
              INTO LS_FCAT-FIELDNAME
              SEPARATED BY '-'.
            ENDIF.
            MOVE LS_DFIES-KEYFLAG TO LS_FCAT-KEY.
            MOVE LS_DFIES-SCRTEXT_M TO LS_FCAT-SELTEXT_L.
            LS_FCAT-TABNAME = ME->G_TABLE.
            CLEAR: LS_FCAT-COL_POS,
            LS_FCAT-OFFSET.
            IF LS_FCAT-REF_TABNAME IS INITIAL.
              LS_FCAT-DDICTXT = 'L'.
            ENDIF.
*           Display Yes/No fields as checkboxes
            IF LS_DFIES-DOMNAME = 'XFELD'.
              LS_FCAT-CHECKBOX = 'X'.
            ENDIF.

*           Add field to field catalog
            APPEND LS_FCAT TO FCAT.
          ENDLOOP.
          CONTINUE.
        ELSE.
*         Try to get info about data element
          CALL FUNCTION 'DDIF_DTEL_GET'
            EXPORTING
              NAME          = LS_FCAT-REF_FIELDNAME
              LANGU         = SY-LANGU
            IMPORTING
              DD04V_WA      = LS_DD04V
            EXCEPTIONS
              ILLEGAL_INPUT = 1
              OTHERS        = 2.
          IF SY-SUBRC = 0.
            MOVE-CORRESPONDING LS_DD04V TO LS_FCAT.
            MOVE: LS_DD04V-SCRTEXT_M TO LS_FCAT-SELTEXT_L,
            LS_DD04V-DOMNAME   TO L_DNAME.
          ELSE.
*           Finally try to get info about domain
            CALL FUNCTION 'DDIF_DOMA_GET'
              EXPORTING
                NAME          = LS_FCAT-REF_FIELDNAME
                LANGU         = SY-LANGU
              IMPORTING
                DD01V_WA      = LS_DD01V
              EXCEPTIONS
                ILLEGAL_INPUT = 1
                OTHERS        = 2.
            IF SY-SUBRC = 0.
              MOVE-CORRESPONDING LS_DD01V TO LS_FCAT.
              MOVE: LS_DD01V-DDTEXT  TO LS_FCAT-SELTEXT_L,
              LS_DD01V-DOMNAME TO L_DNAME.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*     Table name must be internal table containing data
      LS_FCAT-TABNAME = G_TABLE.
*     No offset
      CLEAR: LS_FCAT-OFFSET.
*     Default text is stored in long text
      IF LS_FCAT-REF_TABNAME IS INITIAL.
        LS_FCAT-DDICTXT = 'L'.
      ENDIF.
*     Display Yes/No fields as checkboxes
      IF L_DNAME = 'XFELD'.
        LS_FCAT-CHECKBOX = 'X'.
      ENDIF.

*     Add field to field catalog
      APPEND LS_FCAT TO FCAT.
    ENDLOOP.
*   Positions
    LOOP AT FCAT INTO LS_FCAT.
      LS_FCAT-ROW_POS = 1.
      LS_FCAT-COL_POS = SY-TABIX.
      MODIFY FCAT FROM LS_FCAT TRANSPORTING ROW_POS COL_POS.
    ENDLOOP.
*   Link between fields
    DESCRIBE TABLE FCAT LINES L_NBFLD.
    LOOP AT FCAT INTO LS_FCAT.
      IF SY-TABIX NE L_NBFLD.
        L_INDEX = SY-TABIX + 1.
        READ TABLE FCAT INTO LS_FCAT2 INDEX L_INDEX.
        IF SY-SUBRC = 0.
          IF LS_FCAT-DATATYPE = 'CURR'.
*           Currency unit
            IF LS_FCAT2-DATATYPE = 'CUKY'.
              LS_FCAT-CFIELDNAME = LS_FCAT2-FIELDNAME.
              LS_FCAT-CTABNAME   = LS_FCAT2-TABNAME.
              MODIFY FCAT FROM LS_FCAT.
            ELSE.
              LOOP AT FCAT INTO LS_FCAT2
              FROM L_INDEX
              WHERE DATATYPE = 'CUKY'.
*               First currency unit after field
                LS_FCAT-CFIELDNAME = LS_FCAT2-FIELDNAME.
                LS_FCAT-CTABNAME   = LS_FCAT2-TABNAME.
                MODIFY FCAT FROM LS_FCAT.
                EXIT.
              ENDLOOP.
              IF SY-SUBRC NE 0.
*               No currency unit after field, try before
                READ TABLE FCAT INTO LS_FCAT2
                WITH KEY DATATYPE = 'CUKY'.
                IF SY-SUBRC = 0.
                  LS_FCAT-CFIELDNAME = LS_FCAT2-FIELDNAME.
                  LS_FCAT-CTABNAME   = LS_FCAT2-TABNAME.
                  MODIFY FCAT FROM LS_FCAT.
                ELSE.
*                 Default is EURO
                  LS_FCAT-CURRENCY = 'EUR'.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          IF LS_FCAT-DATATYPE = 'QUAN'.
*           Quantity unit
            IF LS_FCAT2-DATATYPE = 'UNIT'.
              LS_FCAT-CFIELDNAME = LS_FCAT2-FIELDNAME.
              LS_FCAT-CTABNAME   = LS_FCAT2-TABNAME.
              MODIFY FCAT FROM LS_FCAT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "fill_fieldcat
***
*   Get definition of type from code source
***
  METHOD GET_DEFINITION.
    DATA: L_STRNG TYPE RSSOURCE,
          LS_ABAP TYPE RSSOURCE,
          L_FDPOS TYPE I,
          L_FIRST TYPE I,
          L_LASTR TYPE I.

    DATA: LT_INCL TYPE TABLE OF REPID,
          LS_INCL TYPE REPID.

*   Get program code
    READ REPORT REPID INTO ABAP.
    CHECK SY-SUBRC EQ 0.

*   Get first line of definition
    CONCATENATE 'BEGIN OF' STRUC INTO L_STRNG
    SEPARATED BY SPACE.
    LOOP AT ABAP INTO LS_ABAP.
      IF LS_ABAP CS L_STRNG.
        L_FDPOS = STRLEN( L_STRNG ) + SY-FDPOS.
        IF LS_ABAP(1) = '*' OR LS_ABAP(SY-FDPOS) CS '"'.
          CONTINUE.
        ENDIF.
        IF LS_ABAP+L_FDPOS(1) CA ',. "'.
          L_FIRST = SY-TABIX.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF L_FIRST IS INITIAL.
*     Table is defined in an include
      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          PROGRAM    = REPID
        TABLES
          INCLUDETAB = LT_INCL
        EXCEPTIONS
          OTHERS     = 1.
      IF SY-SUBRC = 0.
        LOOP AT LT_INCL INTO LS_INCL.
*         Try to find definition in this include
          READ REPORT LS_INCL INTO ABAP.
          LOOP AT ABAP INTO LS_ABAP.
            IF LS_ABAP CS L_STRNG.
              L_FDPOS = STRLEN( L_STRNG ) + SY-FDPOS.
              IF LS_ABAP(1) = '*' OR LS_ABAP(SY-FDPOS) CS '"'.
                CONTINUE.
              ENDIF.
              IF LS_ABAP+L_FDPOS(1) CA ',. "'.
                L_FIRST = SY-TABIX.
                EXIT.
              ENDIF.
            ENDIF.
          ENDLOOP.
          IF NOT L_FIRST IS INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*   Get last line of definition
    CONCATENATE 'END OF' STRUC INTO L_STRNG
    SEPARATED BY SPACE.
    LOOP AT ABAP INTO LS_ABAP.
      IF LS_ABAP CS L_STRNG.
        L_FDPOS = STRLEN( L_STRNG ) + SY-FDPOS.
        IF LS_ABAP(1) = '*' OR LS_ABAP(SY-FDPOS) CS '"'.
          CONTINUE.
        ENDIF.
        IF LS_ABAP+L_FDPOS(1) CA ',. "'.
          L_LASTR = SY-TABIX - L_FIRST.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

*   Keep only relevant code lines
    IF L_FIRST LE 0
    OR L_LASTR LE 0.
      REFRESH ABAP.
    ELSE.
      DELETE ABAP TO L_FIRST.
      DELETE ABAP FROM L_LASTR.
    ENDIF.
  ENDMETHOD.                    "get_definition
***
*   Get definition of type recursively
***
  METHOD RECURSIVE_DEFINITION.
    DATA: LT_TOKEN TYPE TABLE OF STOKEX,
          LS_TOKEN TYPE STOKEX,
          LT_STATE TYPE TABLE OF SSTMNT,
          LS_STATE TYPE SSTMNT.

    DATA: LS_DEFIN TYPE TY_DEFIN,
          L_REFFLD TYPE FIELDNAME.

    DATA: LT_RECU  TYPE RSFB_SOURCE.

*   Retrieve tokens
    SCAN ABAP-SOURCE ABAP
    TOKENS INTO LT_TOKEN
    STATEMENTS INTO LT_STATE
    WITH ANALYSIS.

    LOOP AT LT_STATE INTO LS_STATE.
      CLEAR: LS_DEFIN.
*     Field name
      READ TABLE LT_TOKEN INTO LS_TOKEN
      INDEX LS_STATE-FROM.
      LS_DEFIN-FIELDNAME = LS_TOKEN-STR.
*     Reference type
      READ TABLE LT_TOKEN INTO LS_TOKEN
      INDEX LS_STATE-TO.
      L_REFFLD = LS_TOKEN-STR.
*     Check if this type is defined in program
      FREE: LT_RECU.
      CALL METHOD GET_DEFINITION
        EXPORTING
          REPID = REPID
          STRUC = L_REFFLD
        CHANGING
          ABAP  = LT_RECU.
      IF LT_RECU IS INITIAL.
        IF NOT G_LEVEL IS INITIAL.
          CONCATENATE G_LEVEL LS_DEFIN-FIELDNAME
          INTO LS_DEFIN-FIELDNAME SEPARATED BY '-'.
          CONDENSE LS_DEFIN-FIELDNAME.
        ENDIF.
        IF L_REFFLD CS '-'.
          SPLIT L_REFFLD AT '-'
          INTO LS_DEFIN-REF_TABNAME
          LS_DEFIN-REF_FIELDNAME.
          IF LS_DEFIN-REF_TABNAME = 'SY'.
            LS_DEFIN-REF_TABNAME = 'SYST'.
          ENDIF.
        ELSE.
          LS_DEFIN-REF_FIELDNAME = LS_TOKEN-STR.
        ENDIF.
        APPEND LS_DEFIN TO ME->GT_DEFIN.
      ELSE.
*       Process sub levels
        IF ME->G_LEVEL IS INITIAL.
          ME->G_LEVEL = LS_DEFIN-FIELDNAME.
        ELSE.
          CONCATENATE ME->G_LEVEL LS_DEFIN-FIELDNAME INTO ME->G_LEVEL
          SEPARATED BY '-'.
        ENDIF.
        CALL METHOD RECURSIVE_DEFINITION
          EXPORTING
            REPID = REPID
          CHANGING
            ABAP  = LT_RECU.
        IF ME->G_LEVEL CS '-'.
          SHIFT ME->G_LEVEL RIGHT UP TO '-'.
          SHIFT ME->G_LEVEL RIGHT.
          SHIFT ME->G_LEVEL LEFT DELETING LEADING SPACE.
        ELSE.
          CLEAR: ME->G_LEVEL.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "recursive_definition

  METHOD GET_ALV_FIELDCAT_COUNT.
    FIELDCOUNT  = LINES( ME->GT_FCAT ).
  ENDMETHOD.                    "GET_ALV_FIELDCAT_COUNT
***
*   Get fieldcat
***
  METHOD GET_ALV_FIELDCAT.
    OUT_FIELDCAT = ME->GT_FCAT.
  ENDMETHOD.                    "get_alv_fieldcat
***
*   Set table name
***
  METHOD SET_TABLE_NAME.
    DATA: L_FCAT TYPE SLIS_FIELDCAT_ALV.

    LOOP AT ME->GT_FCAT INTO L_FCAT.
      L_FCAT-TABNAME = IN_TABNAME.
      MODIFY ME->GT_FCAT FROM L_FCAT.
    ENDLOOP.
    ME->G_TABLE = IN_TABNAME.
  ENDMETHOD.                    "set_table_name
***
*   Set title
***
  METHOD SET_ALV_TITLE.
    ME->G_TITLE = IN_TITLE.
  ENDMETHOD.                    "set_alv_title
***
*   Set fieldcat
***
  METHOD SET_ALV_FIELDCAT.
    ME->GT_FCAT = IN_FIELDCAT.
  ENDMETHOD.                    "set_alv_fieldcat
***
*   Set field invisible
***
  METHOD SET_ALV_FIELDNOOUT.
    DATA: L_FIELD TYPE FIELDNAME,
          L_NOOUT TYPE C,
          L_TECH  TYPE C,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    L_FIELD = IN_FIELD.
    IF IN_NOOUT IS SUPPLIED.
      L_NOOUT = IN_NOOUT.
    ELSE.
      L_NOOUT = 'X'.
    ENDIF.
    IF IN_TECH IS SUPPLIED.
      L_TECH = IN_TECH.
    ENDIF.

    LOOP AT ME->GT_FCAT INTO LS_FCAT
    WHERE FIELDNAME = L_FIELD.
      LS_FCAT-NO_OUT = L_NOOUT.
      LS_FCAT-TECH   = L_TECH.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING NO_OUT TECH.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_fieldnoout
***
*   Set field editable
***
  METHOD SET_ALV_FIELDEDIT.
    DATA: L_FIELD TYPE FIELDNAME,
          L_EDIT  TYPE C,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    L_FIELD = IN_FIELD.
    IF IN_EDIT IS SUPPLIED.
      L_EDIT = IN_EDIT.
    ELSE.
      L_EDIT = 'X'.
    ENDIF.

    LOOP AT ME->GT_FCAT INTO LS_FCAT
    WHERE FIELDNAME = L_FIELD.
      LS_FCAT-EDIT = L_EDIT.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING EDIT.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_fieldedit
***
*   Set field text
***
  METHOD SET_ALV_FIELDTEXT.
    DATA: L_FIELD TYPE FIELDNAME,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    L_FIELD = IN_FIELD.
    LOOP AT ME->GT_FCAT INTO LS_FCAT
    WHERE FIELDNAME = L_FIELD.
      LS_FCAT-SELTEXT_M = IN_FTEXT.
      LS_FCAT-DDICTXT = 'M'.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING SELTEXT_M DDICTXT.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_fieldtext
***
*   Set field sum
***
  METHOD SET_ALV_FIELDSUM.
    DATA: L_FIELD TYPE FIELDNAME,
          L_DOSUM TYPE C,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    L_FIELD = IN_FIELD.
    IF IN_DOSUM IS SUPPLIED.
      L_DOSUM = IN_DOSUM.
    ELSE.
      L_DOSUM = 'X'.
    ENDIF.

    LOOP AT ME->GT_FCAT INTO LS_FCAT
    WHERE FIELDNAME = L_FIELD.
      LS_FCAT-DO_SUM = L_DOSUM.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING DO_SUM.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_fieldsum
***
*   Set line break in field catalog
***
  METHOD SET_ALV_LINEBREAK.
    DATA: L_FIELD TYPE FIELDNAME,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV,
          L_TABIX TYPE I.

    L_FIELD = IN_FIELD.
    READ TABLE ME->GT_FCAT INTO LS_FCAT
    WITH KEY FIELDNAME = L_FIELD.
    IF SY-SUBRC = 0.
      L_TABIX = SY-TABIX.
    ELSE.
      EXIT.
    ENDIF.

    LOOP AT ME->GT_FCAT INTO LS_FCAT
    FROM L_TABIX.
      LS_FCAT-ROW_POS = LS_FCAT-ROW_POS + 1.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING ROW_POS.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_linebreak
***
*   Set settings
***
  METHOD SET_ALV_SETTINGS.
    CALL METHOD MAP_STRUCTURE
      EXPORTING
        SOURCE = IN_SETTINGS
      CHANGING
        DESTIN = ME->GS_SETT.
  ENDMETHOD.                    "set_alv_settings
***
*   Set layout
***
  METHOD SET_ALV_LAYOUT.
    CALL METHOD MAP_STRUCTURE
      EXPORTING
        SOURCE = IN_LAYOUT
      CHANGING
        DESTIN = ME->GS_LAYO.
  ENDMETHOD.                    "set_alv_layout
***
*   Set printing options
***
  METHOD SET_ALV_PRINT.
    CALL METHOD MAP_STRUCTURE
      EXPORTING
        SOURCE = IN_PRINT
      CHANGING
        DESTIN = ME->GS_PRIN.
  ENDMETHOD.                    "set_alv_print
***
*   Set sortings
***
  METHOD SET_ALV_SORTING.
    DATA: L_DESC   TYPE ALVDYNP-SORTDOWN,
          L_GROUP  TYPE ALVDYNP-GROUPLEVEL,
          L_SUBTOT TYPE ALVDYNP-SUBTOTALS.
    DATA: LS_SORT  TYPE SLIS_SORTINFO_ALV,
          L_INDEX  TYPE I.

    IF IN_DESC IS SUPPLIED.
      L_DESC = IN_DESC.
    ENDIF.
    IF IN_GROUP IS SUPPLIED.
      L_GROUP = IN_GROUP.
    ELSE.
      L_GROUP = '*'.
    ENDIF.
    IF IN_SUBTOT IS SUPPLIED.
      L_SUBTOT = IN_SUBTOT.
    ELSE.
      L_SUBTOT = 'X'.
    ENDIF.

    DESCRIBE TABLE ME->GT_SORT LINES L_INDEX.
    L_INDEX = L_INDEX + 1.

    LS_SORT-SPOS = L_INDEX.
    LS_SORT-FIELDNAME = IN_FIELD.
    LS_SORT-TABNAME = ME->G_TABLE.
    IF L_DESC IS INITIAL.
      LS_SORT-UP = 'X'.
    ELSE.
      LS_SORT-DOWN = 'X'.
    ENDIF.
    LS_SORT-GROUP = L_GROUP.
    LS_SORT-SUBTOT = L_SUBTOT.

    APPEND LS_SORT TO ME->GT_SORT.
  ENDMETHOD.                    "set_alv_sorting
***
*   Set key fields
***
  METHOD SET_ALV_KEYS.
    DATA: L_KEY   TYPE C,
          LS_FCAT TYPE SLIS_FIELDCAT_ALV.

    IF IN_KEY IS SUPPLIED.
      L_KEY = IN_KEY.
    ELSE.
      L_KEY = 'X'.
    ENDIF.
    LOOP AT ME->GT_FCAT INTO LS_FCAT FROM 1 TO IN_LEVEL.
      LS_FCAT-KEY = L_KEY.
      MODIFY GT_FCAT FROM LS_FCAT TRANSPORTING KEY.
    ENDLOOP.
  ENDMETHOD.                    "set_alv_keys
***
*   Add event
***
  METHOD SET_ALV_EVENT.
    DATA: LS_EVNT TYPE SLIS_ALV_EVENT.

    LOOP AT GT_EVNT INTO LS_EVNT
    WHERE NAME = IN_NAME.
      LS_EVNT-FORM = IN_FORM.
      MODIFY GT_EVNT FROM LS_EVNT TRANSPORTING FORM.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      LS_EVNT-NAME = IN_NAME.
      LS_EVNT-FORM = IN_FORM.
      APPEND LS_EVNT TO GT_EVNT.
    ENDIF.
  ENDMETHOD.                    "set_alv_event
***
*   Add event
***
  METHOD SET_ALL_EVENTS.
    DATA: LT_TRIG TYPE TABLE OF RTRIG,
          LS_EVNT TYPE SLIS_ALV_EVENT.

    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      EXPORTING
        I_LIST_TYPE     = G_TABLE_TYPE
      IMPORTING
        ET_EVENTS       = GT_EVNT
      EXCEPTIONS
        LIST_TYPE_WRONG = 1
        OTHERS          = 2.
    IF SY-SUBRC = 0.
*     Get program form routines
      LOAD REPORT G_REPID PART 'TRIG' INTO LT_TRIG.
*     List of valid form routines
      LOOP AT GT_EVNT INTO LS_EVNT.
        CONCATENATE 'F_' LS_EVNT-NAME INTO LS_EVNT-FORM.
        IF NOT G_BLOCK_MODE IS INITIAL.
          CONCATENATE LS_EVNT-FORM ME->G_TABLE INTO LS_EVNT-FORM
          SEPARATED BY '_'.
        ENDIF.
        READ TABLE LT_TRIG WITH KEY EXTO  = LS_EVNT-FORM
        FFORM = 'X'
        TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          MODIFY GT_EVNT FROM LS_EVNT TRANSPORTING FORM.
        ELSE.
          DELETE GT_EVNT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "set_all_events
***
*   Map fields from incoming structure into attribute
***
  METHOD MAP_STRUCTURE.
    DATA: OB_DESC  TYPE REF TO CL_ABAP_STRUCTDESCR,
          LS_COMPO TYPE ABAP_COMPDESCR.

    FIELD-SYMBOLS: <FIELD> TYPE ANY,
    <STRUC> TYPE ANY.

    OB_DESC ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( DESTIN ).

    LOOP AT OB_DESC->COMPONENTS INTO LS_COMPO.
      ASSIGN COMPONENT LS_COMPO-NAME OF STRUCTURE SOURCE TO <FIELD>.
      IF <FIELD> IS ASSIGNED.
        ASSIGN COMPONENT LS_COMPO-NAME OF STRUCTURE DESTIN TO <STRUC>.
        CATCH SYSTEM-EXCEPTIONS CONVERSION_ERRORS = 1.
          MOVE <FIELD> TO <STRUC>.
        ENDCATCH.
        UNASSIGN <FIELD>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "map_structure
***
*   Get default variant
***
  METHOD GET_DEFAULT_VARIANT.
    G_VARIANT_LEVEL = G_VARIANT_LEVEL + 1.

    OUT_VARIANT-REPORT   = ME->G_REPID.
    OUT_VARIANT-HANDLE   = ME->G_VARIANT_LEVEL.
    OUT_VARIANT-USERNAME = SY-UNAME.
    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        I_SAVE     = 'U'
      CHANGING
        CS_VARIANT = OUT_VARIANT
      EXCEPTIONS
        OTHERS     = 0.
  ENDMETHOD.                    "get_default_variant


  METHOD TRANSFORM.

** hack für DDIC elemente: prüfen lassen, ob das richtig ist
*  if s_dfies-fieldname is initial and not s_dfies-tabname is initial.
*    s_dfies-fieldname = s_dfies-tabname.
*    clear s_dfies-tabname.
*    exit.
*  endif.
** hack

*<<<Y7CK002776
    IF S_DFIES-TABNAME EQ S_DFIES-ROLLNAME OR
    S_DFIES-TABNAME EQ S_DFIES-DOMNAME.
      CLEAR S_DFIES-TABNAME.
    ENDIF.
*>>>Y7CK002776

    S_DFIES-REFTABLE = S_DFIES-TABNAME.
    S_DFIES-REFFIELD = S_DFIES-FIELDNAME.
    CLEAR S_DFIES-PRECFIELD.

    FIELD-SYMBOLS: <S_DFIES> TYPE DFIES.

    LOOP AT T_DFIES ASSIGNING <S_DFIES>.

      CLEAR <S_DFIES>-PRECFIELD.
      IF <S_DFIES>-DATATYPE = 'CURR' OR <S_DFIES>-DATATYPE = 'QUAN'.

        READ TABLE T_DFIES WITH KEY FIELDNAME = <S_DFIES>-REFFIELD
        TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          <S_DFIES>-PRECFIELD = <S_DFIES>-REFFIELD.
        ENDIF.

      ENDIF.
      <S_DFIES>-REFTABLE = <S_DFIES>-TABNAME.
      <S_DFIES>-REFFIELD = <S_DFIES>-FIELDNAME.

    ENDLOOP.

  ENDMETHOD.                    "TRANSFORM

  METHOD READ_STRUCTDESCR.

    DATA: T_COMPONENTS TYPE ABAP_COMPONENT_TAB,
          S_COMPONENT TYPE ABAP_COMPONENTDESCR,
          R_STRUCT TYPE REF TO CL_ABAP_STRUCTDESCR,
          R_ELEMENT TYPE REF TO CL_ABAP_ELEMDESCR,
          T_SUB_DFIES TYPE DDFIELDS,
          S_DFIES TYPE DFIES.

    CLEAR T_DFIES.

    IF R_STRUCTDESCR->IS_DDIC_TYPE( ) = ABAP_TRUE.
      T_DFIES = R_STRUCTDESCR->GET_DDIC_FIELD_LIST( ).
      TRANSFORM( CHANGING T_DFIES = T_DFIES ).
    ELSE.

      T_COMPONENTS = R_STRUCTDESCR->GET_COMPONENTS( ).

      LOOP AT T_COMPONENTS INTO S_COMPONENT.

        IF S_COMPONENT-AS_INCLUDE = ABAP_TRUE.

          R_STRUCT ?= S_COMPONENT-TYPE.
          T_SUB_DFIES = READ_STRUCTDESCR( R_STRUCT ).

          LOOP AT T_SUB_DFIES INTO S_DFIES.
            CONCATENATE S_DFIES-FIELDNAME S_COMPONENT-SUFFIX INTO S_DFIES-FIELDNAME.
            IF NOT S_DFIES-PRECFIELD IS INITIAL.
              CONCATENATE S_DFIES-PRECFIELD S_COMPONENT-SUFFIX INTO S_DFIES-PRECFIELD.
            ENDIF.
            APPEND S_DFIES TO T_DFIES.
          ENDLOOP.

        ELSE.

          CASE S_COMPONENT-TYPE->KIND.

            WHEN 'E'.
              R_ELEMENT ?= S_COMPONENT-TYPE.
              IF R_ELEMENT->IS_DDIC_TYPE( ) = ABAP_TRUE.
                S_DFIES = R_ELEMENT->GET_DDIC_FIELD( SY-LANGU ).
                TRANSFORM( CHANGING S_DFIES = S_DFIES ).
                S_DFIES-FIELDNAME = S_COMPONENT-NAME.
                APPEND S_DFIES TO T_DFIES.
              ELSE.
                CLEAR S_DFIES.
                S_DFIES-FIELDNAME = S_COMPONENT-NAME.
                S_DFIES-INTTYPE = R_ELEMENT->TYPE_KIND.
                S_DFIES-LENG = R_ELEMENT->LENGTH.
                S_DFIES-DECIMALS = R_ELEMENT->DECIMALS.
                S_DFIES-CONVEXIT = R_ELEMENT->EDIT_MASK.
                S_DFIES-OUTPUTLEN = R_ELEMENT->OUTPUT_LENGTH.
                S_DFIES-SIGN = 'X'.
                APPEND S_DFIES TO T_DFIES.
              ENDIF.

            WHEN 'S'.
              R_STRUCT ?= S_COMPONENT-TYPE.
              T_SUB_DFIES = READ_STRUCTDESCR( R_STRUCT ).

              LOOP AT T_SUB_DFIES INTO S_DFIES.
                CONCATENATE S_COMPONENT-NAME '-' S_DFIES-FIELDNAME INTO S_DFIES-FIELDNAME.
                CONCATENATE S_COMPONENT-NAME '-' S_DFIES-PRECFIELD INTO S_DFIES-PRECFIELD.
                IF NOT S_DFIES-PRECFIELD IS INITIAL.
                  CONCATENATE S_DFIES-PRECFIELD S_COMPONENT-SUFFIX INTO S_DFIES-PRECFIELD.
                ENDIF.
                APPEND S_DFIES TO T_DFIES.
              ENDLOOP.

          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "READ_STRUCTDESCR

ENDCLASS.                    "lcl_table_display IMPLEMENTATION 
