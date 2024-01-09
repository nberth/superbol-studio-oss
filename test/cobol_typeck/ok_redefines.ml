(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Prog_printer

let dotest = Typeck_testing.show_data

let%expect_test "redefines-occurs" =
  dotest @@ prog "redefines-occurs"
    ~working_storage:{|
       77 A             PIC X(5).
       77 B REDEFINES A PIC X OCCURS 5.
    |};
  [%expect {|
    prog.cob:4.7-5.39:
       1          PROGRAM-ID. redefines-occurs.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        77 A             PIC X(5).
    ----          ^^^^^^^^^^^^^^^^^^^^^^^^^^
       5 >        77 B REDEFINES A PIC X OCCURS 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6          PROCEDURE DIVISION.
       7
    Item definition: {
      qualname: A
      offset: 0
      size: 40
      layout: {
        elementary
        usage: {
          display
          category: ALPHANUMERIC(5)
        }
      }
      redefs: {
        table
        redefines: A
        offset: 0
        size: 40
        range: {
          span: fixed-length: 5
        }
        field: {
          qualname: B
          leading ranges: 1
          offset: 0
          size: 8
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(1)
            }
          }
        }
      }
    } |}];;

let%expect_test "qualified-redefines-occurs" =
  dotest @@ prog "qualified-redefines-occurs"
    ~working_storage:{|
       01 X.
         02 A             PIC X(5).
         02 B REDEFINES A PIC X OCCURS 5.
    |};
  [%expect {|
    prog.cob:4.7-6.41:
       1          PROGRAM-ID. qualified-redefines-occurs.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 X.
    ----          ^^^^^
       5 >          02 A             PIC X(5).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B REDEFINES A PIC X OCCURS 5.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8
    Item definition: {
      qualname: X
      offset: 0
      size: 40
      layout: {
        structure
        fields: {
          qualname: A IN X
          offset: 0
          size: 40
          layout: {
            elementary
            usage: {
              display
              category: ALPHANUMERIC(5)
            }
          }
          redefs: {
            table
            redefines: A IN X
            offset: 0
            size: 40
            range: {
              span: fixed-length: 5
            }
            field: {
              qualname: B IN X
              leading ranges: 1
              offset: 0
              size: 8
              layout: {
                elementary
                usage: {
                  display
                  category: ALPHANUMERIC(1)
                }
              }
            }
          }
        }
      }
    } |}];;

let%expect_test "occurs-n-redefines-1" =
  dotest @@ prog "occurs-n-redefines-1"
    ~working_storage:{|
       01 W.
         02 A OCCURS 5 TIMES PIC X VALUE "A".
         02 B REDEFINES A PIC X(5).
    |}
    ~procedure:{|
           MOVE "BCDEF" TO B
           DISPLAY A (1) "/" B.
    |};
  [%expect {|
    prog.cob:6.9-6.35:
       3          WORKING-STORAGE SECTION.
       4          01 W.
       5            02 A OCCURS 5 TIMES PIC X VALUE "A".
       6 >          02 B REDEFINES A PIC X(5).
    ----            ^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8          MOVE "BCDEF" TO B
    >> Warning: Redefinition of item with OCCURS clause A IN W

    prog.cob:4.7-6.35:
       1          PROGRAM-ID. occurs-n-redefines-1.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 W.
    ----          ^^^^^
       5 >          02 A OCCURS 5 TIMES PIC X VALUE "A".
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >          02 B REDEFINES A PIC X(5).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7          PROCEDURE DIVISION.
       8          MOVE "BCDEF" TO B
    Item definition: {
      qualname: W
      offset: 0
      size: 40
      layout: {
        structure
        fields: {
          table
          offset: 0
          size: 40
          range: {
            span: fixed-length: 5
          }
          field: {
            qualname: A IN W
            leading ranges: 1
            offset: 0
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(1)
              }
              value: "A"
            }
          }
          redefs: {
            qualname: B IN W
            redefines: A IN W
            offset: 0
            size: 40
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(5)
              }
            }
          }
        }
      }
    } |}];;

let%expect_test "occurs-n-redefines-2" =
  dotest @@ prog "occurs-n-redefines-2"
    ~working_storage:{|
       01 W.
         02 A PIC X OCCURS 2 TIMES.
       01 X REDEFINES W.
         02 B PIC X.
         02 C PIC X.
    |};
  [%expect {|
    prog.cob:4.7-8.20:
       1          PROGRAM-ID. occurs-n-redefines-2.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 W.
    ----          ^^^^^
       5 >          02 A PIC X OCCURS 2 TIMES.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       6 >        01 X REDEFINES W.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >          02 B PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       8 >          02 C PIC X.
    ----  ^^^^^^^^^^^^^^^^^^^^^
       9          PROCEDURE DIVISION.
      10
    Item definition: {
      qualname: W
      offset: 0
      size: 16
      layout: {
        structure
        fields: {
          table
          offset: 0
          size: 16
          range: {
            span: fixed-length: 2
          }
          field: {
            qualname: A IN W
            leading ranges: 1
            offset: 0
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(1)
              }
            }
          }
        }
      }
      redefs: {
        qualname: X
        redefines: W
        offset: 0
        size: 16
        layout: {
          structure
          fields: {
            qualname: B IN X
            offset: 0
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(1)
              }
            }
          }{
            qualname: C IN X
            offset: 8
            size: 8
            layout: {
              elementary
              usage: {
                display
                category: ALPHANUMERIC(1)
              }
            }
          }
        }
      }
    } |}];;


let%expect_test "redefines-index" =
  (* Note: GnuCOBOL accepts this.  But should it really?  *)
  dotest @@ prog "redefines-index"
    ~working_storage:{|
       01 XX USAGE IS INDEX.
         02 X.
         02 Y REDEFINES X.
       01 YY REDEFINES XX PIC 9(4).
    |}
    ~procedure:{|
       MAIN.
           DISPLAY X
           DISPLAY YY.
    |};
  [%expect {|
    prog.cob:4.7-7.35:
       1          PROGRAM-ID. redefines-index.
       2          DATA DIVISION.
       3          WORKING-STORAGE SECTION.
       4 >        01 XX USAGE IS INDEX.
    ----          ^^^^^^^^^^^^^^^^^^^^^
       5 >          02 X.
    ----  ^^^^^^^^^^^^^^^
       6 >          02 Y REDEFINES X.
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
       7 >        01 YY REDEFINES XX PIC 9(4).
    ----  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       8          PROCEDURE DIVISION.
       9          MAIN.
    Item definition: {
      qualname: XX
      offset: 0
      size: size-of-index
      layout: {
        structure
        fields: {
          qualname: X IN XX
          offset: 0
          size: size-of-index
          layout: {
            elementary
            usage: index
          }
          redefs: {
            qualname: Y IN XX
            redefines: X IN XX
            offset: 0
            size: size-of-index
            layout: {
              elementary
              usage: index
            }
          }
        }
      }
      redefs: {
        qualname: YY
        redefines: XX
        offset: 0
        size: 32
        layout: {
          elementary
          usage: {
            display
            category: NUMERIC(digits = 4, scale = 0, with_sign = false)
          }
        }
      }
    } |}];;