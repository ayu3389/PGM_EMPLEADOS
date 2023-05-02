      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PGM01.
      * Author:AYELEN RIVERO.
      * Date:28/04/23.

      *---------------------------------------------------------------*
      *OBJETIVO: LEER E IMPRIMIR EL ARCHIVO DE EMPLEADOS DE LA        *
      *          EMPRESA Y DEVOLVER LA SUMA TOTAL DE SUS SUELDOS      *
      *          JUNTO CON LA CANTIDAD DE EMPLEADOS.                  *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.


       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO
           "C:\Users\ayele\OneDrive\Escritorio\PGM_EMP2\EMP_FILE.txt".

           SELECT REPORTE   ASSIGN TO
           "C:\Users\ayele\OneDrive\Documentos\Udemy\REPORTES.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS


           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-EMPLEADOS          PIC X(50).

       FD  REPORTE

           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-REPORTE            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-AREAS-A-USAR.
           05 WS-REG-EMPLEADOS.
              10 WS-NUMERO-EMP    PIC X(05).
              10 WS-NOMBRE-EMP    PIC X(30).
              10 WS-STATUS-EMP    PIC 9(01).
              10 WS-DEPTO-EMP     PIC 9(03).
              10 WS-PUESTO-EMP    PIC 9(02).
              10 WS-SALARIO-EMP   PIC 9(07)V99.
           05 WS-LEIDOS-EMP       PIC 9(05)    VALUE 0.
           05 WS-IMPRESOS         PIC 9(05)    VALUE 0.
           05 WS-TOT-SALARIOS     PIC 9(09)V99 VALUE 0.

       01  WS-CURRENT-DATE.
           03 WS-ACTUAL-DATE.
              05 WS-DATE-AAAA     PIC 9(04).
              05 WS-DATE-MM       PIC 9(02).
              05 WS-DATE-DD       PIC 9(02).

       01  WS-TITULO-1.
           05 FILLER              PIC X(27)    VALUE ' '.
           05 WS-TIT-1            PIC X(22)
                                  VALUE "CENTRO DE CAPACITACION".
           05 FILLER              PIC X(31)    VALUE ' '.

       01  WS-TITULO-2.
           03 FILLER              PIC X(08)    VALUE " FECHA: ".
           03 WS-TIT2-FECHA.
               05 TIT-2-DD         PIC 9(02).
               05 FILLER           PIC X(01)    VALUE "/".
               05 TIT-2-MM         PIC 9(02).
               05 FILLER           PIC X(01)    VALUE "/".
               05 TIT-2-AAAA       PIC 9(04).
               05 FILLER           PIC X(09)    VALUE ' '.
           03 WS-TIT-2.
               05 FILLER           PIC X(23)
                                   VALUE "EMPLEADOS DE LA EMPRESA".
               05 FILLER           PIC X(17)    VALUE ' '.
               05 FILLER           PIC X(08)    VALUE "PAGINA: ".
               05 TIT-2-PAGINA     PIC ZZ9.
               05 FILLER           PIC X(02)    VALUE ' '.

       01  WS-GUIONES.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(78)    VALUE ALL "-".
           05 FILLER              PIC X(01)    VALUE ' '.

       01  WS-SUB-TITULO-1.
           05 FILLER              PIC X(04)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "NUMERO".
           05 FILLER              PIC X(12)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "NOMBRE".
           05 FILLER              PIC X(15)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "STATUS".
           05 FILLER              PIC X(02)    VALUE ' '.
           05 FILLER              PIC X(05)    VALUE "DEPTO".
           05 FILLER              PIC X(01)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "PUESTO".
           05 FILLER              PIC X(04)    VALUE ' '.
           05 FILLER              PIC X(07)    VALUE "SALARIO".
           05 FILLER              PIC X(06)    VALUE ' '.
       01  WS-DETALLE.
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-NUMERO       PIC ZZZZ9.
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-NOMBRE       PIC X(30).
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-STATUS       PIC 9(01).
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-DEPTO        PIC 9(03).
           05 FILLER              PIC X(04)    VALUE ' '.
           05 WS-DET-PUESTO       PIC 9(02).
           05 FILLER              PIC X(03)    VALUE ' '.
           05 WS-DET-SALARIO      PIC Z,ZZZ,ZZ9.99.


      *TOTALES DE CONTROL.

       01  WS-DETALLE-LEIDOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "TOTAL DE EMPLEADOS LEIDOS  : ".
           05 WS-TOT-LEIDOS       PIC ZZ,ZZ9.
           05 FILLER              PIC X(44)    VALUE ' '.

       01  WS-DETALLE-IMPRESOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "TOTAL DE EMPLEADOS IMPRESOS: ".
           05 WS-TOT-IMPRESOS     PIC ZZ,ZZ9.
           05 FILLER              PIC X(44)    VALUE ' '.

       01  WS-DETALLE-SALARIOS.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(29)
                                  VALUE "SUMA TOTAL DE SALARIOS    : ".
           05 WS-DET-SALARIO2     PIC $$$,$$$,$$9.99.
           05 FILLER              PIC X(36)    VALUE ' '.

      * DEFINICION DE FECHA ACTUAL

       01  CURRENT-DATE.
           05 DATE-DD             PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 ATE-MM              PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 DATE-AAA            PIC 9(04).

       PROCEDURE DIVISION.
       010-INICIO.
           OPEN INPUT  EMPLEADOS
                OUTPUT REPORTE
           WRITE REG-REPORTE FROM WS-TITULO-1 AFTER ADVANCING PAGE
           MOVE FUNCTION CURRENT-DATE    TO WS-CURRENT-DATE


           MOVE WS-DATE-DD                    TO TIT-2-DD
           MOVE WS-DATE-MM                    TO TIT-2-MM
           MOVE WS-DATE-AAAA                  TO TIT-2-AAAA
           MOVE 1                             TO TIT-2-PAGINA
           WRITE REG-REPORTE FROM WS-TITULO-2 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES  AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-SUB-TITULO-1 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES AFTER ADVANCING 1.

       020-LEE.
           READ EMPLEADOS INTO WS-REG-EMPLEADOS AT END
                  PERFORM 100-FIN.

           DISPLAY WS-REG-EMPLEADOS.
           ADD 1                  TO WS-LEIDOS-EMP
           ADD WS-SALARIO-EMP     TO WS-TOT-SALARIOS

           MOVE WS-NUMERO-EMP     TO WS-DET-NUMERO
           MOVE WS-NOMBRE-EMP     TO WS-DET-NOMBRE
           MOVE WS-STATUS-EMP     TO WS-DET-STATUS
           MOVE WS-DEPTO-EMP      TO WS-DET-DEPTO
           MOVE WS-PUESTO-EMP     TO WS-DET-PUESTO
           MOVE WS-SALARIO-EMP    TO WS-DET-SALARIO
           WRITE REG-REPORTE FROM WS-DETALLE AFTER ADVANCING 1

           ADD 1 TO WS-IMPRESOS
                 PERFORM  020-LEE.


       100-FIN.
           MOVE WS-LEIDOS-EMP     TO WS-TOT-LEIDOS
           WRITE REG-REPORTE FROM WS-DETALLE-LEIDOS AFTER ADVANCING 2
           MOVE WS-IMPRESOS       TO WS-TOT-IMPRESOS
           WRITE REG-REPORTE FROM WS-DETALLE-IMPRESOS AFTER ADVANCING 1
           MOVE WS-TOT-SALARIOS   TO WS-DET-SALARIO2
           WRITE REG-REPORTE FROM WS-DETALLE-SALARIOS AFTER ADVANCING 1
           CLOSE EMPLEADOS
                 REPORTE
           STOP RUN.
