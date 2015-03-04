class Z_LONGINT definition
  public
  final
  create public .

  PUBLIC SECTION.
    METHODS:
      constructor,
      clone EXPORTING value(out_long) TYPE REF TO Z_LONGINT,
      getlength EXPORTING value(len) TYPE i,
      output,
      set IMPORTING value(instr) TYPE string,

      add IMPORTING value(in_long) TYPE REF TO Z_LONGINT
          EXPORTING value(out_long) TYPE REF TO Z_LONGINT,
      sub IMPORTING value(in_long) TYPE REF TO Z_LONGINT
          EXPORTING value(out_long) TYPE REF TO Z_LONGINT,
      mul IMPORTING value(in_long) TYPE int2
          EXPORTING value(out_long) TYPE REF TO Z_LONGINT,
      div IMPORTING value(in_long) TYPE int2
          EXPORTING value(out_long) TYPE REF TO Z_LONGINT,

      sadd IMPORTING value(in_long) TYPE REF TO Z_LONGINT,
      ssub IMPORTING value(in_long) TYPE REF TO Z_LONGINT,
      smul IMPORTING value(in_long) TYPE int2,
      sdiv IMPORTING value(in_long) TYPE int2,

** ƒÎ-specific **
      setnum IMPORTING value(in_long) TYPE i,
      output_pi.
  protected section.
  private section.
    CONSTANTS: m_length TYPE i VALUE 1008.
    DATA: m_blocks TYPE i,
          m_value(m_length) TYPE n.
ENDCLASS.



CLASS Z_LONGINT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE REF TO Z_LONGINT
* | [<---] OUT_LONG                       TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add.
    CLEAR out_long.
    CREATE OBJECT out_long.

    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long->m_value+offset(4).
      i3 = i1 + i2 + borrow.
      IF i3 >= 10000.
        borrow = 1.
        i3 = i3 - 10000.
      ELSE.
        borrow = 0.
      ENDIF.
      out_long->m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "add


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->CLONE
* +-------------------------------------------------------------------------------------------------+
* | [<---] OUT_LONG                       TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clone.
    CLEAR out_long.
    CREATE OBJECT out_long.
    out_long->m_value = m_value.
  ENDMETHOD.                    "clone


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    m_blocks = m_length / 4.
  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->DIV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE        INT2
* | [<---] OUT_LONG                       TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD div.
    CLEAR out_long.
    CREATE OBJECT out_long.

    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( sy-index - 1 ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long.
      i3 = ( i1 + borrow * 10000 ) DIV i2.
      borrow = i1 MOD i2.
      out_long->m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "div


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->GETLENGTH
* +-------------------------------------------------------------------------------------------------+
* | [<---] LEN                            TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD getlength.
    len = m_length.
  ENDMETHOD.                    "getlength


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->MUL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE        INT2
* | [<---] OUT_LONG                       TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mul.
    CLEAR out_long.
    CREATE OBJECT out_long.

    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long.
      i3 = i1 * i2 + borrow.
      borrow = i3 DIV 10000.
      out_long->m_value+offset(4) = i3 MOD 10000.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "mul


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->OUTPUT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD output.
    DATA: blocks TYPE i,
          offset TYPE i,
          mod25 TYPE i.
    blocks = m_length / 4.
    DO blocks TIMES.
      offset = ( sy-index - 1 ) * 4.
      WRITE : m_value+offset(4).

      mod25 = sy-index MOD 25.
      IF mod25 EQ 0.
        NEW-LINE.
      ENDIF.
    ENDDO.
    NEW-LINE.
  ENDMETHOD.                    "Output


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->OUTPUT_PI
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD output_pi.
    DATA: blocks TYPE i,
          offset TYPE i,
          mod25 TYPE i.
    WRITE '3.'.
    NEW-LINE.
    DO 100 TIMES.
      offset = ( sy-index - 1 ) * 10 + 4.
      WRITE m_value+offset(10).
      mod25 = sy-index MOD 10.
      IF mod25 EQ 0.
        NEW-LINE.
      ENDIF.
    ENDDO.
    NEW-LINE.
  ENDMETHOD.                    "output_pi


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sadd.
    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long->m_value+offset(4).
      i3 = i1 + i2 + borrow.
      IF i3 >= 10000.
        borrow = 1.
        i3 = i3 - 10000.
      ELSE.
        borrow = 0.
      ENDIF.
      m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "add


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SDIV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE        INT2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sdiv.
    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( sy-index - 1 ) * 4.
      i1 = m_value+offset(4) + borrow * 10000.
      i2 = in_long.
      i3 = i1 DIV i2.
      borrow = i1 MOD i2.
      m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "div


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] INSTR                          TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set.
    m_value = instr.
  ENDMETHOD.                    "Set


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SETNUM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD setnum.
    m_value+0(4) = in_long.
  ENDMETHOD.                    "setnum


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SMUL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE        INT2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD smul.
    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long.
      i3 = i1 * i2 + borrow.
      borrow = i3 DIV 10000.
      m_value+offset(4) = i3 MOD 10000.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "mul


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SSUB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ssub.
    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long->m_value+offset(4).
      i3 = i1 - i2 - borrow.
      IF i3 < 0.
        borrow = 1.
        i3 = i3 + 10000.
      ELSE.
        borrow = 0.
      ENDIF.
      m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "sub


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_LONGINT->SUB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_LONG                        TYPE REF TO Z_LONGINT
* | [<---] OUT_LONG                       TYPE REF TO Z_LONGINT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sub.
    CLEAR out_long.
    CREATE OBJECT out_long.

    DATA: borrow TYPE i,
          i1 TYPE i,
          i2 TYPE i,
          i3 TYPE i,
          offset TYPE i.

    DO m_blocks TIMES.
      offset = ( m_blocks - sy-index ) * 4.
      i1 = m_value+offset(4).
      i2 = in_long->m_value+offset(4).
      i3 = i1 - i2 - borrow.
      IF i3 < 0.
        borrow = 1.
        i3 = i3 + 10000.
      ELSE.
        borrow = 0.
      ENDIF.
      out_long->m_value+offset(4) = i3.
    ENDDO.

    IF borrow <> 0.
      " overflow!
    ENDIF.
  ENDMETHOD.                    "sub
ENDCLASS.
