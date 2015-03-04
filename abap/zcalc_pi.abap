*** Calculation of ƒÎ with Machin's formula ****

REPORT  zcalc_pi.

START-OF-SELECTION.

  DATA: starttime TYPE timestampl,
        endtime TYPE timestampl.

  GET TIME STAMP FIELD starttime.

  DATA loopcounts TYPE i VALUE 716. " [1000/1.39794]+1

  DATA: ref_longint1 TYPE REF TO z_longint,
        ref_longint2 TYPE REF TO z_longint,
        ref_longint3 TYPE REF TO z_longint,
        ref_longint_pi TYPE REF TO z_longint.

  CREATE OBJECT: ref_longint1, ref_longint2, ref_longint3, ref_longint_pi.

  CALL METHOD: ref_longint1->setnum EXPORTING in_long = 80,
               ref_longint2->setnum EXPORTING in_long = 956. " 4*239

  DATA: mod2 TYPE i,
        hoge TYPE int2.

  DO loopcounts TIMES.
    " (1)=w
    CALL METHOD ref_longint1->sdiv
      EXPORTING
        in_long = 25.

    " (2)=v
    CALL METHOD ref_longint2->sdiv
      EXPORTING
        in_long = 239.
    CALL METHOD ref_longint2->sdiv
      EXPORTING
        in_long = 239.

    " (3)=w-v
    CALL METHOD ref_longint1->sub
      EXPORTING
        in_long  = ref_longint2
      IMPORTING
        out_long = ref_longint3.

    " (3)=(w-v)/(2n-1)
    hoge = 2 * sy-index - 1.
    CALL METHOD ref_longint3->sdiv
      EXPORTING
        in_long = hoge.

    mod2 = sy-index MOD 2.
    IF mod2 EQ 0.
      CALL METHOD ref_longint_pi->ssub
        EXPORTING
          in_long = ref_longint3.
    ELSE.
      CALL METHOD ref_longint_pi->sadd
        EXPORTING
          in_long = ref_longint3.
    ENDIF.

  ENDDO.

  GET TIME STAMP FIELD endtime.

  WRITE: 'START:', starttime,
       / 'END  :', endtime.
  NEW-LINE.
  ULINE.
  NEW-LINE.
  CALL METHOD ref_longint_pi->output_pi.
