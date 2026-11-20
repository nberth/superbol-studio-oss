IDENTIFICATION DIVISION.
PROGRAM-ID. LIT.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC X(300) VALUE "This is a very long literal in a VALUE clause that will not fit within the right margin, and that has ""quotes"" in it, as well as trailing spaces     ".
PROCEDURE DIVISION.
    DISPLAY "[" FUNCTION TRIM (X TRAILING) "]"
    DISPLAY "This is a very long literal that will not fit within the right margin, and that has ""quotes"" in it" UPON CONSOLE
    DISPLAY 'Single-quoted literal that is long enough to need splitting, with '' a quote inside, and a few more words' *> comment
    DISPLAY "Short" " " "literals" " " "around" " " "a long one that will have to be split as well because it is so long, really"
    DISPLAY "    leading spaces, then text that is long enough to be split at least once at the margin of the target format"
>>D DISPLAY "A debugging line, with a literal that is long enough to be split when converted" UPON CONSOLE
    STOP RUN.
