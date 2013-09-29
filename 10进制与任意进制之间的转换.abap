REPORT zvi001.

*使用ABAP语言进行进制之间的转换
*2013-09-27


*2进制与10进制之间的转换
*8进制与10进制之间的转换
*16进制与10进制之间的转换

*自定义进制与10进制之间的转换。


PARAMETERS p_number TYPE decfloat34.


CONSTANTS : c36 TYPE string VALUE  '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' .
CONSTANTS : c16 TYPE string VALUE  '0123456789ABCDEF' .
CONSTANTS : c8 TYPE string VALUE  '012345678' .

CONSTANTS : custom1 TYPE string VALUE  '01XYZ~!@#$%^&*()_+' .

TYPES: BEGIN OF ty_list,
      index TYPE int4,
      value TYPE decfloat34,
      END OF ty_list.


WRITE: '十进制的数据：' ,(20)p_number.
PERFORM demo_convert_scale USING p_number.


*&---------------------------------------------------------------------*
*&      Form  demo_convert_scale
*&---------------------------------------------------------------------*
*    演示进制转换之间的转换
*----------------------------------------------------------------------*
FORM demo_convert_scale USING num.
  DATA: list TYPE TABLE OF ty_list.
  DATA: wa TYPE ty_list.

  DATA: result TYPE string .
  DATA: presult TYPE string.

  PERFORM convert_scale_ten_to_any USING num 36 '' CHANGING list result .

  WRITE : / , / '三十六进制' ,result.
  PERFORM convert_scale_any_to_ten USING result 36 '' CHANGING presult.
  WRITE : '反解析：' ,presult.

*  LOOP AT  list INTO wa.
*    WRITE : / 'INDEX: ' , wa-index ,'VALUE: ' , (10) wa-value .
*  ENDLOOP.

  PERFORM convert_scale_ten_to_any USING num 16 '' CHANGING list result .
  WRITE : / , /  '十六进制' ,result.
  PERFORM convert_scale_any_to_ten USING result 16 '' CHANGING presult.
  WRITE : '反解析：' ,presult.

*  LOOP AT  list INTO wa.
*    WRITE : / 'INDEX: ' , wa-index ,'VALUE: ' , (10) wa-value .
*  ENDLOOP.



  PERFORM convert_scale_ten_to_any USING num 8 '' CHANGING list result .
  WRITE : / , / '八进制' ,result.
*  LOOP AT  list INTO wa.
*    WRITE : / 'INDEX: ' , wa-index ,'VALUE: ' , (10) wa-value .
*  ENDLOOP.

  PERFORM convert_scale_any_to_ten USING result 8 '' CHANGING presult.
  WRITE : '反解析：' ,presult.


  PERFORM convert_scale_ten_to_any USING num 2 '' CHANGING list result .
  WRITE : / , /  '二进制' ,result.
*  LOOP AT  list INTO wa.
*    WRITE : / 'INDEX: ' , wa-index ,'VALUE: ' , (10) wa-value .
*  ENDLOOP.


  PERFORM convert_scale_any_to_ten USING result 2 '' CHANGING presult.
  WRITE : '反解析：' ,presult.



  PERFORM convert_scale_ten_to_any USING num 0 custom1 CHANGING list result .
  WRITE : / , /  '自定义进制' ,result.
  PERFORM convert_scale_any_to_ten USING result 0 custom1 CHANGING presult.
  WRITE : '反解析：' ,presult.

*  LOOP AT  list INTO wa.
*    WRITE : / 'INDEX: ' , wa-index ,'VALUE: ' , (10) wa-value .
*  ENDLOOP.


ENDFORM.                    "demo_convert_scale



*&---------------------------------------------------------------------*
*&      Form  conver10to2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NUM        text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM to2 USING num CHANGING result.
  DATA: list TYPE TABLE OF ty_list.
  PERFORM convert_scale_ten_to_any USING num 2 '' CHANGING list result.

ENDFORM.                    "conver10to2

*&---------------------------------------------------------------------*
*&      Form  to8
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NUM        text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM to8 USING num CHANGING result.
  DATA: list TYPE TABLE OF ty_list.
  PERFORM convert_scale_ten_to_any USING num 8 '' CHANGING list result.

ENDFORM.                    "conver10to2

*&---------------------------------------------------------------------*
*&      Form  to16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NUM        text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM to16 USING num CHANGING result.
  DATA: list TYPE TABLE OF ty_list.
  PERFORM convert_scale_ten_to_any USING num 16 '' CHANGING list result.

ENDFORM.                    "conver10to2

*&---------------------------------------------------------------------*
*&      Form  from2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM from2 USING value CHANGING result.

  PERFORM convert_scale_any_to_ten USING value 2 '' CHANGING result.

ENDFORM.                    "from2

*&---------------------------------------------------------------------*
*&      Form  from8
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM from8 USING value CHANGING result.

  PERFORM convert_scale_any_to_ten USING value 8 '' CHANGING result.

ENDFORM.                    "from8


*&---------------------------------------------------------------------*
*&      Form  from16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->RESULT     text
*----------------------------------------------------------------------*
FORM from16 USING value CHANGING result.

  PERFORM convert_scale_any_to_ten USING value 16 '' CHANGING result.

ENDFORM.                    "from16




*&---------------------------------------------------------------------*
*&      Form  convert_scale_ten_to_any
*&---------------------------------------------------------------------*
*       转换10进制数到任意的进制，可以用于不重复编码。
*----------------------------------------------------------------------*
*      -->PVALUE     需要转换的值
*      -->PSTEP      进制
*      -->CUSTOM     用户自定义的字符串
*      -->PLIST      解析后的数值的顺序列表
*      -->PRESULT    返回的结果
*----------------------------------------------------------------------*
FORM convert_scale_ten_to_any USING pvalue pstep custom CHANGING plist presult.


  DATA num4 TYPE decfloat34."输入数据
  DATA step TYPE int4."进制

  DATA: list TYPE TABLE OF ty_list. "返回的结果值。
  DATA: wa TYPE ty_list.
  CLEAR :list ,wa.

  DATA: index TYPE sy-index.
  index = 0 .

  step = pstep."八进制
  num4 = pvalue."计算值

  IF custom IS NOT INITIAL.
    step = strlen( custom ).
    IF step = 0 .
      presult = pvalue.
      RETURN.
*      MESSAGE e000(00) WITH '无法处理'.
    ENDIF.
  ENDIF.
*  IF pstep IS INITIAL OR pstep = 0 .
*
*
*  ENDIF.
  DATA num5 TYPE decfloat34. "商
  DATA num6 TYPE decfloat34. "余数
  DATA tmpnum TYPE decfloat34."中间变量

  DATA tmp TYPE string.      "中间变量

  DATA result TYPE string.   "结果

  DATA exitflag TYPE c .
  DO .
    num5 = num4 / step.
    num6 = num4 MOD step.

    IF num4 > step ."
      tmpnum = round( val = num6 dec = 0 mode = 5 ).
    ELSE.
      tmpnum = round( val = num4 dec = 0 mode = 5 ).
      exitflag = 'X'.
    ENDIF.

    wa-index = index.
    wa-value = tmpnum.
    APPEND wa TO list.

    IF step = 8.
      tmp = c8+tmpnum(1).
    ELSEIF step = 16.
      tmp = c16+tmpnum(1).
    ELSEIF step = 36.
      tmp = c36+tmpnum(1).
    ELSEIF custom IS NOT INITIAL.
      tmp = custom+tmpnum(1).

    ELSE.
      tmp = tmpnum.
      CONDENSE tmp.
    ENDIF .
    CONCATENATE tmp result  INTO result.

    num4 = num5.
    index = index + 1.

    IF  exitflag = 'X'.
      EXIT.
    ENDIF.
  ENDDO.

  plist = list[].
  presult = result.
ENDFORM.                    "convert_scale_ten_to_any



*&---------------------------------------------------------------------*
*&      Form  convert_scale_any_to_ten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      需要转换的值
*      -->STEP       进制
*      -->CUSTOM     定制的字符串
*      -->RESULT     转换结果
*----------------------------------------------------------------------*
FORM convert_scale_any_to_ten USING value pstep custom CHANGING presult.

  IF value IS INITIAL.
    presult = value.
    RETURN.
  ENDIF.

  DATA raw TYPE string.
  raw = value.

  DATA leng TYPE int4. "计算的字符串的长度
  DATA pos TYPE int4. "当前计算的位置,同时也是计算的平方次数。
  leng = strlen( raw ).
  pos = leng - 1.

  DATA step TYPE int4.
  step = pstep.

  IF custom IS NOT INITIAL.
    step = strlen( custom ).
    IF step = 0 .
      presult = raw.
      RETURN.
*      MESSAGE e000(00) WITH '无法处理'.
    ENDIF.
  ENDIF.
*  IF pstep IS INITIAL OR pstep = 0 .
*
*  ENDIF.

  DATA tmpc TYPE c.
  DATA tmpv TYPE int4.
  DATA index TYPE int4.
  index = 0 .

  DATA sumv TYPE decfloat34.
  DATA total TYPE decfloat34.
  DO leng TIMES.
    tmpc = raw+index(1).
    IF step = 2.
      tmpv = tmpc.

    ELSEIF step = 8.
      PERFORM get_num USING tmpc c8 CHANGING tmpv.
    ELSEIF step = 16.
      PERFORM get_num USING tmpc c16 CHANGING tmpv.
    ELSEIF step = 36.
      PERFORM get_num USING tmpc c36 CHANGING tmpv.
    ELSEIF custom IS NOT INITIAL .
      PERFORM get_num USING tmpc custom CHANGING tmpv.
    ELSE.

      tmpv = tmpc.
    ENDIF.
    sumv  = ( step ** pos ) * tmpv.
    ADD sumv TO total.

    SUBTRACT 1 FROM pos.
    ADD 1 TO index.
  ENDDO.

  presult = total.

ENDFORM.                    "convert_scale_any_to_ten

*&---------------------------------------------------------------------*
*&      Form  get_num
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHAR     text
*      -->P_TARGET   text
*      -->P_NUM      text
*----------------------------------------------------------------------*
FORM get_num  USING    p_char p_target CHANGING p_num.
*  SEARCH '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' FOR p_char.
*  SEARCH numrange FOR p_char.
*  p_num = sy-fdpos.

  DATA result TYPE match_result .
  FIND p_char IN p_target IGNORING CASE RESULTS  result .
  IF sy-subrc <> 0.
*    MESSAGE e001(00) WITH '无法找到字符串' p_char.
    RAISE digit_not_valid.
  ENDIF.
  p_num = result-offset.

ENDFORM.                    " get_num
