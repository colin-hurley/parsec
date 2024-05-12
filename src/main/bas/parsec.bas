' Parsec 1.8.0 Source Code
' Originally written in QuickBasic 7.1.
' Ported to FreeBASIC 1.05.0.
'------------------------------------------------------------------------------

'----------------------- Constants --------------------------------------------

'16-color terminal palette constants
CONST BLACK = 0
CONST DARK_BLUE = 1
CONST DARK_GREEN = 2
CONST DARK_CYAN = 3
CONST DARK_RED = 4
CONST DARK_MAGENTA = 5
CONST DARK_YELLOW = 6
CONST LIGHT_GRAY = 7
CONST DARK_GRAY = 8
CONST LIGHT_BLUE = 9
CONST LIGHT_GREEN = 10
CONST LIGHT_CYAN = 11
CONST LIGHT_RED = 12
CONST LIGHT_MAGENTA = 13
CONST LIGHT_YELLOW = 14
CONST WHITE = 15

'The tab ('\t') character
CONST CHR_TAB = CHR$(9)

'----------------------- API Subs ---------------------------------------------
DECLARE SUB CreateAPI ()
DECLARE SUB MakeExit ()
DECLARE SUB PrintStatus (stat$)
DECLARE SUB PrintError (priority%, errorMsg$)
DECLARE SUB UpdateBar ()

'----------------------- Main Subs --------------------------------------------
DECLARE SUB SkipComments ()
DECLARE SUB CloseCog ()
DECLARE SUB OpenCog ()
DECLARE SUB ReadFileData ()
DECLARE SUB ParseFlags ()
DECLARE SUB ParseSymbols ()
DECLARE SUB ParseCode ()
DECLARE SUB ParseEnd ()
DECLARE SUB Analize ()

'----------------------- Code Subs --------------------------------------------
DECLARE SUB DoFor (codeLine$, x%)
DECLARE SUB DoForStatement (codeLine$, x%)
DECLARE SUB DoDo (codeLine$, x%)
DECLARE SUB DoWhile (codeLine$, x%)
DECLARE SUB DoStatement (codeLine$, x%, blockEnd%, MsgReturn%, which%)
DECLARE SUB DoIf (codeLine$, x%)
DECLARE SUB DoBlock (codeLine$, x%)
DECLARE SUB DoCall (codeLine$, x%)
DECLARE SUB DoAssignment (codeLine$, x%, del$)
DECLARE SUB DoMessage (codeLine$, x%, MsgName$)
DECLARE FUNCTION DoFunction% (codeLine$, x%)
DECLARE SUB DoString (codeLine$, x%)
DECLARE SUB DoSubExp (codeLine$, x%, del$, expType$, valDesc$)
DECLARE FUNCTION DoValue$ (codeLine$, x%, expType$)
DECLARE SUB DoVector (codeLine$, x%)
DECLARE SUB DoSemi (codeLine$, x%)
DECLARE SUB DoLabel (codeWord$, x%)

'----------------------- Parsing Subs -----------------------------------------
DECLARE SUB GetToNextDelimit (codeLine$, x%, del$)
DECLARE FUNCTION ReverseCutAtDelimit$ (codeLine$, x%, del$)
DECLARE SUB GetToNextChar (codeLine$, x%)
DECLARE FUNCTION NextCharInLine$ (codeLine$, x%)
DECLARE FUNCTION CutAtNextDelimit$ (codeLine$, x%, del$)
DECLARE SUB FindNextChar (codeLine$, x%, multiLine%)
DECLARE FUNCTION CheckForBlankLine% (codeLine$, x%)

'----------------------- Checking Subs ----------------------------------------
DECLARE SUB CheckSymType (theType$)
DECLARE SUB CheckSymName (theType$, theName$)
DECLARE SUB CheckSymVal (theType$, initVal$)
DECLARE SUB CheckSymExt (theType$, exten$, extVal$)
DECLARE SUB CheckForExtConflict (exten$(), extCount%)
DECLARE SUB CheckExpType (expect$, theType$, valDesc$)

'----------------------- Validation Functions ---------------------------------
DECLARE FUNCTION IsValidName% (parseString$)
DECLARE FUNCTION IsValidInt% (intString$)
DECLARE FUNCTION IsValidFlex% (flexString$)
DECLARE FUNCTION IsOperator% (char$)
DECLARE FUNCTION IsUnaryOp% (char$)
DECLARE FUNCTION IsValidVector% (vector$)

'----------------------- String Functions -------------------------------------
DECLARE SUB ChopString (str1$, str2$, Length%)
DECLARE FUNCTION Trim$ (tString$)

'----------------------- Misc Subs --------------------------------------------
DECLARE FUNCTION GetArrayPrefix$ (codeWord$)
DECLARE FUNCTION GetVarType$ (var$)
DECLARE FUNCTION SubType$ (theType$)
DECLARE FUNCTION CharNum% (codeLine$, char$)
DECLARE FUNCTION InStringArray% (sArray$(), sValue$)

'----------------------- Misc Startup -----------------------------------------

CreateAPI
DIM SHARED time1!
time1! = TIMER

'----------------------- Declare Shared Variables -----------------------------

REM $DYNAMIC

'Arrays
' Note: Unfortunately, we cannot be too generous with the memory space
' allocation; otherwise, code 14 (out of string space) will be thrown.
' This is the case in QuickBASIC at least. Need to confirm whether or not this
' happens with FreeBASIC as well.
' Update (May 2024): I increased the size of some of the arrays that
' hold symbol information and Parsec still works. Having more symbols
' in a cog than Parsec can fit in the array causes Parsec to crash.
DIM SHARED symType$(1 TO 30)
DIM SHARED symExt$(1 TO 10)
DIM SHARED message$(1 TO 100)
DIM SHARED verbName$(1 TO 601)
DIM SHARED verbAct%(1 TO 601)
DIM SHARED verbRet$(1 TO 601)
DIM SHARED verbParNum$(1 TO 601)
DIM SHARED verbParam$(1 TO 601, 1 TO 11)
DIM SHARED varName$(1 TO 512)
DIM SHARED varConst$(1 TO 512)
DIM SHARED varType$(1 TO 512)
DIM SHARED usedVar$(1 TO 512)
DIM SHARED assignVar$(1 TO 512)
DIM SHARED arrayVar$(1 TO 100)
DIM SHARED CallMessage$(1 TO 100)
DIM SHARED CodeMessage$(1 TO 100)
DIM SHARED errors$(1 TO 10)

'Array Pop Numbers
DIM SHARED varCount%
DIM SHARED numCalls%
DIM SHARED numCM%
DIM SHARED numLines%
DIM SHARED errNum%
DIM SHARED numUsedVars%
DIM SHARED numAssignVars%
DIM SHARED numArrayVars%

'Shared counters and vars
DIM SHARED lineNum%
DIM SHARED statExec%
DIM SHARED codeLine$

'Settings vars
DIM SHARED aisub$
DIM SHARED cogsub$
DIM SHARED flexsub$
DIM SHARED floatsub$
DIM SHARED intsub$
DIM SHARED keyframesub$
DIM SHARED materialsub$
DIM SHARED messagesub$
DIM SHARED modelsub$
DIM SHARED sectorsub$
DIM SHARED surfacesub$
DIM SHARED soundsub$
DIM SHARED templatesub$
DIM SHARED thingsub$
DIM SHARED vectorsub$
DIM SHARED allowBadFlex%
DIM SHARED strictParse%
DIM SHARED simArrayNames%

'Initialize Vars
numCalls% = 0
numCM% = 0
errNum% = 0
lineNum% = 0
numLines% = 0
numUsedVars% = 0
numArrayVars% = 0

'------------------------------------------------------------------------------

ReadFileData
OpenCog
SkipComments
ParseFlags
ParseSymbols
SkipComments
ParseCode
ParseEnd
Analize
CloseCog
UpdateBar
PrintError(0, "Parse of " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + " Complete. Time elapsed:" + LEFT$(STR$(TIMER - time1!), 5) + " seconds.")
PrintStatus ("Cog Parse Complete.")
MakeExit

'------------------------------------------------------------------------------
END

REM $STATIC

SUB CreateAPI

	'----------------------------------------------------------------------
	' This sub creates the text interface for Parsec. After the API is
	' created, UpdateBar, PrintError, and PrintStatus are used to change
	' the information displayed through the interface.
	'----------------------------------------------------------------------

	' FreeBASIC migration - CLEAR is not supported by FreeBASIC
	'CLEAR , , 8000

	DIM a%
	CLS

	'--------------- Resize Window ----------------------------------------

	WIDTH 80, 25

	'--------------- Print Title Bar --------------------------------------

	COLOR BLACK, LIGHT_GRAY
	PRINT SPACE$(33); "Parsec v1.8.0"; SPACE$(34)

	'--------------- Print main window ------------------------------------

	COLOR LIGHT_GRAY, DARK_BLUE
	LOCATE 2
	PRINT CHR$(201); STRING$(78, 205); CHR$(187)
	FOR a% = 1 TO 19
		LOCATE 2 + a%
		PRINT CHR$(186); SPACE$(78); CHR$(186)
	NEXT a%
	LOCATE 22
	PRINT CHR$(200); STRING$(78, 205); CHR$(188)

	'--------------- Print Status Bar -------------------------------------

	COLOR BLACK, DARK_CYAN
	LOCATE 23
	PRINT STRING$(80, 32)

	'--------------- Create Bar Graph Shadow ------------------------------

	COLOR DARK_GRAY, BLACK
	FOR a% = 5 TO 7
		LOCATE a%, 17
		PRINT SPACE$(52);
	NEXT a%

	'--------------- Create Error Box Shadow ------------------------------

	FOR a% = 11 TO 18
		LOCATE a%, 7
		PRINT SPACE$(70);
	NEXT a%

	'--------------- Create Bar Graph -------------------------------------

	COLOR BLACK, LIGHT_GRAY
	LOCATE 4, 15
	PRINT CHR$(201); STRING$(50, 205); CHR$(187)
	LOCATE 5, 15
	PRINT CHR$(186); SPACE$(1); STRING$(48, 176); SPACE$(1); CHR$(186)
	LOCATE 6, 15
	PRINT CHR$(200); STRING$(50, 205); CHR$(188)

	'--------------- Create Error Box -------------------------------------

	LOCATE 10, 5
	PRINT CHR$(201); STRING$(4, 205); " Error Box "; STRING$(53, 205); CHR$(187)
	FOR a% = 11 TO 16
		LOCATE a%, 5
		PRINT CHR$(186); SPACE$(68); CHR$(186)
	NEXT a%
	LOCATE 17, 5
	PRINT CHR$(200); STRING$(68, 205); CHR$(188)

END SUB

SUB PrintStatus (stat$)

	'----------------------------------------------------------------------
	' This sub prints strings to the status bar of the API.
	'----------------------------------------------------------------------

	COLOR BLACK, DARK_CYAN
	LOCATE 23, 2
	PRINT stat$ + SPACE$(70 - LEN(stat$))

END SUB

SUB PrintError (priority%, errorMsg$)

	'----------------------------------------------------------------------
	' PrintError is called to print an error in the API error box. If
	' "no errors" or "Warning" are not found in the string, then this sub
	' will call MakeExit to end the program.
	'----------------------------------------------------------------------

	DIM extra$
	DIM a%
	COLOR DARK_RED, LIGHT_GRAY

	errNum% = errNum% + 1
	IF errNum% <= 6 THEN
		IF LEN(errorMsg$) > 67 THEN
			ChopString(errorMsg$, extra$, 67)
			errors$(errNum%) = errorMsg$
			errNum% = errNum% + 1
			errors$(errNum%) = extra$
		ELSE
			errors$(errNum%) = errorMsg$
		END IF
	ELSE
		IF LEN(errorMsg$) > 67 THEN
			FOR a% = 1 TO 4
				errors$(a%) = errors$(a% + 1)
			NEXT a%
			ChopString(errorMsg$, extra$, 67)
			errors$(5) = errorMsg$
			errNum% = errNum% + 1
			errors$(6) = extra$
		ELSE
			FOR a% = 1 TO 5
				errors$(a%) = errors$(a% + 1)
			NEXT a%
			errors$(6) = errorMsg$
		END IF
	END IF

	FOR a% = 1 TO 6
		LOCATE 10 + a%, 7
		PRINT errors$(a%) + STRING$(67 - LEN(errors$(a%)), 32)
	NEXT a%

	IF errNum% >= 6 AND INSTR(errorMsg$, "Warning") < 1 AND INSTR(errorMsg$, "Parse of") < 1 AND priority% = 0 THEN
		PrintStatus ("Too many errors in " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + ". Aborting...")
		MakeExit
	END IF

	IF priority% > 0 THEN
		PrintStatus ("Fatal Error in " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + ". Unable to continue parse.")
		MakeExit
	END IF

END SUB

SUB ChopString (str1$, str2$, Length%)

	'----------------------------------------------------------------------
	' This sub is called only by PrintError to divide a string to long to
	' fit in the error box. ChopString finds the last space and divides the
	' string there.
	'----------------------------------------------------------------------

	DIM x%
	DIM char$

	FOR x% = Length% TO 1 STEP -1
		char$ = MID$(str1$, x%, 1)
		IF char$ = " " THEN EXIT FOR
	NEXT x%

	IF x% = 1 THEN
		COLOR BLACK, LIGHT_GRAY
		CLS
		PRINT "Internal Error: ChopString could not break up a solid string."
	END IF

	str2$ = MID$(str1$, x%, LEN(str1$) - x% + 1)
	str1$ = MID$(str1$, 1, LEN(str1$) - LEN(str2$))
	str2$ = "  " + str2$

END SUB

SUB UpdateBar

	'----------------------------------------------------------------------
	' This sub draws the green bar graph to show how far through the cog
	' Parsec has gone.
	'----------------------------------------------------------------------

	DIM percent%    'percent is not scaled 0 - 100, but 0 - 48
	DIM ratio!
	DIM a%

	ratio! = 48 / numLines%
	percent% = CINT(lineNum% * ratio!)

	COLOR DARK_GREEN, LIGHT_GRAY
	FOR a% = 1 TO percent%
		LOCATE 5, 16 + a%
		PRINT CHR$(219)
	NEXT a%

END SUB

SUB MakeExit

	'----------------------------------------------------------------------
	' MakeExit prints an "OK" box at the bottom of the API and waits for a
	' key to be pressed before ending the program.
	'----------------------------------------------------------------------

	'--------------- Create Button Shadow ---------------------------------

	COLOR DARK_GRAY, BLACK
	LOCATE 20, 37
	PRINT SPACE$(8);
	LOCATE 21, 37
	PRINT SPACE$(8);
	LOCATE 22, 37
	PRINT STRING$(8, 205)

	'--------------- Create Button ----------------------------------------

	COLOR BLACK, LIGHT_GRAY
	LOCATE 19, 36
	PRINT CHR$(201); STRING$(6, 205); CHR$(187)
	LOCATE 20, 36
	PRINT CHR$(186); SPACE$(2); "OK"; SPACE$(2); CHR$(186)
	LOCATE 21, 36
	PRINT CHR$(200); STRING$(6, 205); CHR$(188)

	'--------------- Wait for key to be pressed ---------------------------

	DO WHILE INKEY$ = ""
	LOOP

	'--------------- Restore original terminal colors ---------------------

	COLOR LIGHT_GRAY, BLACK
	CLS
	END

END SUB

SUB ReadFileData

	'----------------------------------------------------------------------
	' This sub opens the data.dat file and stores the contained data in
	' several global arrays.
	'----------------------------------------------------------------------

	DIM dataFile$
	DIM x%
	DIM a%
	DIM vParamNum%
	DIM codeWord$
	DIM numSymTypes%
	DIM numSymExts%
	DIM numMessages%
	DIM numVerbs%

	dataFile$ = __EXEPATH() + "\data.dat"

	OPEN dataFile$ FOR INPUT AS #1
	IF EOF(1) THEN
		CLOSE #1
		PrintError(1, "Failed to read file: " + dataFile$)
	END IF

	numSymExts% = 0
	numVerbs% = 0
	numSymTypes% = 0
	numMessages% = 0

	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)

		IF NextCharInLine$(codeLine$, 1) <> "#" THEN
			IF RIGHT$(codeLine$, 1) = ":" THEN
				section$ = codeLine$
			ELSE
				IF section$ = "extensions:" THEN
					numSymExts% = numSymExts% + 1
					symExt$(numSymExts%) = codeLine$

				ELSEIF section$ = "types:" THEN
					numSymTypes% = numSymTypes% + 1
					symType$(numSymTypes%) = codeLine$

				ELSEIF section$ = "messages:" THEN
					numMessages% = numMessages% + 1
					message$(numMessages%) = codeLine$

				ELSEIF section$ = "settings:" THEN      'Settings must be read before verbs.
					x% = 1
					codeWord$ = CutAtNextDelimit$(codeLine$, x%, "=")
					x% = x% + 1
					value$ = CutAtNextDelimit$(codeLine$, x%, "#")

					IF codeWord$ = "ai" THEN
						aisub$ = value$
					ELSEIF codeWord$ = "cog" THEN
						cogsub$ = value$
					ELSEIF codeWord$ = "flex" THEN
						flexsub$ = value$
					ELSEIF codeWord$ = "float" THEN
						floatsub$ = value$
					ELSEIF codeWord$ = "int" THEN
						intsub$ = value$
					ELSEIF codeWord$ = "keyframe" THEN
						keyframesub$ = value$
					ELSEIF codeWord$ = "material" THEN
						materialsub$ = value$
					ELSEIF codeWord$ = "message" THEN
						messagesub$ = value$
					ELSEIF codeWord$ = "model" THEN
						modelsub$ = value$
					ELSEIF codeWord$ = "sector" THEN
						sectorsub$ = value$
					ELSEIF codeWord$ = "sound" THEN
						soundsub$ = value$
					ELSEIF codeWord$ = "surface" THEN
						surfacesub$ = value$
					ELSEIF codeWord$ = "template" THEN
						templatesub$ = value$
					ELSEIF codeWord$ = "thing" THEN
						thingsub$ = value$
					ELSEIF codeWord$ = "vector" THEN
						vectorsub$ = value$
					ELSEIF codeWord$ = "allow_bad_flexes" THEN
						allowBadFlex% = VAL(value$)
					ELSEIF codeWord$ = "strict_parse" THEN
						strictParse% = VAL(value$)
					ELSEIF codeWord$ = "sim_array_names" THEN
						simArrayNames% = VAL(value$)
					END IF

				ELSEIF section$ = "verbs:" THEN
					numVerbs% = numVerbs% + 1
					x% = 1
					verbName$(numVerbs%) = CutAtNextDelimit$(codeLine$, x%, " ")
					verbAct%(numVerbs%) = VAL(CutAtNextDelimit$(codeLine$, x%, " "))
					verbRet$(numVerbs%) = SubType$(CutAtNextDelimit$(codeLine$, x%, " "))
					verbParNum$(numVerbs%) = CutAtNextDelimit$(codeLine$, x%, " #")
					vParamNum% = VAL(verbParNum$(numVerbs%))
					FOR a% = 1 TO vParamNum%
						verbParam$(numVerbs%, a%) = SubType$(CutAtNextDelimit$(codeLine$, x%, " #"))
					NEXT a%
				END IF
			END IF
		END IF
	LOOP
	CLOSE #1

END SUB

SUB OpenCog

	OPEN COMMAND$ FOR INPUT AS #1
	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		numLines% = numLines% + 1
	LOOP
	CLOSE #1

	OPEN COMMAND$ FOR INPUT AS #1
	IF EOF(1) THEN
		CLOSE #1
		PrintError(1, "Failed to read file: " + COMMAND$)
	END IF

END SUB

SUB CloseCog

	CLOSE #1

END SUB

SUB SkipComments

	DIM nextChar$

	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(Trim$(codeLine$))
		lineNum% = lineNum% + 1
		nextChar$ = NextCharInLine$(codeLine$, 1)
		IF nextChar$ = "#" OR LEN(nextChar$) < 1 THEN
			'trap
		ELSEIF nextChar$ = "/" THEN
			PrintError(0, "Garbage found on line" + STR$(lineNum%) + ". Double-slash comments are not allowed.")
		ELSE
			EXIT DO
		END IF
	LOOP

END SUB

SUB ParseFlags

	'----------------------------------------------------------------------
	' This sub is called to parse a cog's flags assingment. If the cog is
	' not using any, then ParseFlags will exit back to the calling sub.
	'----------------------------------------------------------------------

	IF MID$(Trim$(codeLine$), 1, 5) <> "flags" THEN EXIT SUB
	PrintStatus ("Parsing Cog Flags.")

	DIM x%
	DIM flagVal$
	DIM lastBreak%
	x% = 1

	GetToNextChar(codeLine$, x%)       'get to the "f" in flags.
	x% = x% + 5      'get past the "flags"

	lastBreak% = x%
	GetToNextChar(codeLine$, x%)
	IF x% > lastBreak% THEN PrintError(0, "An = was expected after the flags assignment on line" + STR$(lineNum%) + ". Spaces are not allowed.")
	IF NextCharInLine(codeLine$, x%) <> "=" THEN PrintError(1, "An = was expected after the flags assignment on line" + STR$(lineNum%) + ".")

	x% = x% + 1      'get past the "="
	IF x% > LEN(codeLine$) THEN PrintError(1, "Integer expected after the flags assignment on line" + STR$(lineNum%) + ".")

	lastBreak% = x%
	GetToNextChar(codeLine$, x%)
	IF x% > lastBreak% THEN PrintError(0, "Integer expected after the flags assignment on line" + STR$(lineNum%) + ". Spaces are not allowed.")

	flagVal$ = CutAtNextDelimit(codeLine$, x%, " #//")
	IF IsValidInt%(flagVal$) = 0 THEN PrintError(0, "The flags assignment on line" + STR$(lineNum%) + " has an invalid integer value.")
	IF CutAtNextDelimit(codeLine$, x%, "#//") <> "" THEN PrintError(0, "Garbage found after the flags assignment on line" + STR$(lineNum%))
	
	SkipComments
END SUB

SUB ParseSymbols

	'----------------------------------------------------------------------
	' ParseSymbols parses the symbol lines between the beginning and end of
	' the symbols section. Each variable is checked with several subs to
	' make sure that it's valid. ParseSymbols records each variable that it
	' finds. When a variable is found in the code section, it will be
	' checked to see if it was declared.
	'----------------------------------------------------------------------

	DIM codeWord$
	DIM x%
	x% = 1

	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "symbols" THEN PrintError(1, "Symbols beginning expected on line" + STR$(lineNum%) + ".")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN PrintError(0, "Garbage found after beginning of symbols section on line" + STR$(lineNum%) + ".")

	PrintStatus ("Parsing Defined Variables.")

	DIM a%
	DIM theType$
	DIM realType$
	DIM theName$
	DIM initVal$
	DIM extVal$(1 TO 5)
	DIM isBlank%
	DIM exten$(1 TO 5)
	DIM extCount%
	DIM EOL%
	varCount% = 0
	EOL% = 0

	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		IF CutAtNextDelimit(codeLine$, 1, " #//") = "end" OR CutAtNextDelimit(codeLine$, 1, " #//") = "code" THEN EXIT DO

		isBlank% = CheckForBlankLine%(codeLine$, 1)
		IF isBlank% = 0 THEN
			'--------- Reset Vars ---------------------------------
			theType$ = "null"
			theName$ = "null"
			initVal$ = "null"
			extCount% = 0
			EOL% = 0
			FOR a% = 1 TO 5
				extVal$(a%) = "null"
				exten$(a%) = "null"
			NEXT a%

			'--------- Main Symbol Parser -------------------------
			x% = 1
			theType$ = CutAtNextDelimit$(codeLine$, x%, " ")
			IF x% >= LEN(codeLine$) THEN
				PrintError(0, "Invalid symbol definition on line" + STR$(lineNum%) + ". Skipping symbol.")
				EOL% = 1
			END IF
			
			theName$ = CutAtNextDelimit$(codeLine$, x%, " =#//")

			IF EOL% = 0 THEN
				GetToNextChar(codeLine$, x%)
				IF MID$(codeLine$, x%, 1) = "=" THEN
					x% = x% + 1
					initVal$ = CutAtNextDelimit$(codeLine$, x%, " #//")
				END IF
			END IF

			DO WHILE x% <= LEN(codeLine$) AND CheckForBlankLine%(codeLine$, x%) = 0 AND EOL% = 0
				extCount% = extCount% + 1
				exten$(extCount%) = CutAtNextDelimit$(codeLine$, x%, " =,#//")
				GetToNextChar(codeLine$, x%)
				IF MID$(codeLine$, x%, 1) = "=" THEN
					x% = x% + 1
					extVal$(extCount%) = CutAtNextDelimit$(codeLine$, x%, " ,#//")
				END IF
				IF MID$(codeLine$, x%, 1) = "," THEN
					x% = x% + 1     'get past the comma
					IF CheckForBlankLine%(codeLine$, x%) = 1 THEN PrintError(0, "Another symbol extension expected on line" + STR$(lineNum%) + ", but none found.")
				END IF
			LOOP

			'--------- Check Symbol -------------------------------

			IF EOL% = 0 THEN

			CheckSymType(theType$)
			CheckSymName(theType$, theName$)
			CheckSymVal(theType$, initVal$)
			FOR a% = 1 TO extCount%
				CheckSymExt(theType$, exten$(a%), extVal$(a%))
			NEXT a%
			CheckForExtConflict(exten$(), extCount%)

			'--------- Substitute Type ----------------------------

			realType$ = theType$
			theType$ = SubType$(theType$)

			'--------- Record Variable ----------------------------

			varCount% = varCount% + 1
			varName$(varCount%) = theName$
			varType$(varCount%) = theType$

			IF InStringArray%(exten$(), "local") = 0 THEN
				IF realType$ = "sector" OR realType$ = "surface" OR realType$ = "thing" THEN varConst$(varCount%) = theName$
				numAssignVars% = numAssignVars% + 1
				assignVar$(numAssignVars%) = theName$
			ELSEIF initVal$ <> "null" THEN
				numAssignVars% = numAssignVars% + 1
				assignVar$(numAssignVars%) = theName$
			END IF

			END IF  'for EOL check
		END IF
		UpdateBar
	LOOP

	x% = 1
	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "end" THEN PrintError(1, "No end to the symbols section.")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN PrintError(0, "Garbage found after end of the symbols section on line" + STR$(lineNum%) + ".")
	
END SUB

SUB ParseCode

	'----------------------------------------------------------------------
	' ParseCode starts inside the code section and looks for messages. When
	' it finds one, it calls DoMessage to parse it.
	'----------------------------------------------------------------------

	DIM codeWord$
	DIM lastBreak%
	DIM x%
	DIM message$
	x% = 1

	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "code" THEN PrintError(1, "Code section beginning expected on line" + STR$(lineNum%) + ".")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN PrintError(0, "Garbage found after beginning of code section on line" + STR$(lineNum%) + ".")

	PrintStatus ("Parsing Code Section.")

	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		x% = 1
		IF CutAtNextDelimit(codeLine$, x%, " #//") = "end" THEN EXIT DO
		x% = 1
		DO WHILE x% <= LEN(codeLine$) AND CheckForBlankLine%(codeLine$, x%) = 0
			lastBreak% = x%
			message$ = CutAtNextDelimit$(codeLine$, x%, ":")
			IF IsValidName%(message$) = 1 AND MID$(codeLine$, x%, 1) = ":" THEN
				IF InStringArray%(CodeMessage$(), message$) > 0 THEN PrintError(0, "The message, " + message$ + ", on line" + STR$(lineNum%) + " has already been used in the code section.")
				PrintStatus ("Parsing message, " + message$ + ".")
				numCM% = numCM% + 1
				CodeMessage$(numCM%) = message$
				numUsedVars% = numUsedVars% + 1
				usedVar$(numUsedVars%) = message$
				x% = x% + 1     'get past the ":"
				DoMessage(codeLine$, x%, message$)
			ELSE
				x% = lastBreak%
				PrintError(1, "Message expected, but " + CutAtNextDelimit$(message$, 1, " :#//") + " found on line" + STR$(lineNum%) + ".")
			END IF
			
		LOOP
	LOOP

	'Note below that Parsec does not allow double-slash comments directly
	'after the 'end'. Since JK doesn't read beyond the end of the code,
	'it isn't aware of the comment. It's overkill, but it enforces the
	'rule: no double-slash comments outside of the code or symbols sections.
	
	x% = 1
	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "end" THEN PrintError(1, "No end to the code section.")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN PrintError(0, "Garbage found after end of code section on line" + STR$(lineNum%) + ".")

END SUB

SUB ParseEnd

	DIM nextChar$

	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(Trim$(codeLine$))
		lineNum% = lineNum% + 1
		nextChar$ = NextCharInLine$(codeLine$, 1)
		IF nextChar$ = "#" OR LEN(nextChar$) < 1 THEN
			'trap
		ELSEIF nextChar$ = "/" THEN
			PrintError(0, "Garbage found on line" + STR$(lineNum%) + ". Double-slash comments are not allowed.")
		ELSE
			PrintError(0, "Garbage found on line" + STR$(lineNum%) + ".")
		END IF
	LOOP

END SUB

SUB Analize

	'----------------------------------------------------------------------
	' Analisys checks how messages were used in the code section and
	' to see which variables were used.
	'----------------------------------------------------------------------

	PrintStatus ("Analizing Code.")

	DIM a%
	DIM mess$

	'--------------- Check messages used in the code section --------------

	FOR a% = 1 TO numCM%
		mess$ = CodeMessage$(a%)
		IF InStringArray%(message$(), mess$) > 0 THEN
			IF InStringArray%(varName$(), mess$) = 0 OR GetVarType$(mess$) <> SubType$("message") THEN PrintError(0, "The message, " + mess$ + ", is not defined in the symbols section.")
		ELSE
			IF InStringArray%(CallMessage$(), mess$) = 0 THEN PrintError(0, "The custom message, " + mess$ + ", is not called anywhere in the code section.")
		END IF
	NEXT a%

	'--------------- Check called messages --------------------------------

	FOR a% = 1 TO numCalls%
		mess$ = CallMessage$(a%)
		IF InStringArray%(CodeMessage$(), mess$) = 0 THEN PrintError(0, "The message, " + mess$ + ", does not exist, but it is called in the code section.")
	NEXT a%

	'--------------- Check Used Variables ---------------------------------
	
	FOR a% = 1 TO varCount%
		IF numArrayVars% = 0 THEN
			IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
				IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
					PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
				ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
					PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
				ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
					PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
				END IF
			END IF
		ELSEIF simArrayNames% = 1 THEN
			IF InStringArray(arrayVar$(), varName$(a%)) > 0 THEN
				'found the first var in an array.
				arrayPrefix$ = GetArrayPrefix$(varName$(a%))
				IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
					IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
					END IF
				END IF
				
			ELSEIF LEN(arrayPrefix$) > 0 AND GetArrayPrefix$(varName$(a%)) = arrayPrefix$ THEN
				'trap - found an array variable.
			ELSE
				'found a non-array variable.
				arrayPrefix$ = ""       'don't want non-array vars to come in the middle of an array.
				IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
					IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
						PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
					END IF
				END IF
			END IF
		END IF
	NEXT a%

END SUB

SUB GetToNextChar (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub parses through a string starting at a given pos. It returns
	' the pos of the next non-space character.
	'----------------------------------------------------------------------

	DIM char$
	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	FOR itr% = x% TO LEN(codeLine$)
		x% = itr%
		char$ = MID$(codeLine$, x%, 1)
		IF char$ <> " " AND char$ <> CHR_TAB THEN EXIT FOR
	NEXT itr%
	x% = itr%

END SUB

FUNCTION NextCharInLine$ (codeLine$, x%)

	GetToNextChar(codeLine$, x%)
	NextCharInLine$ = MID$(codeLine$, x%, 1)

END FUNCTION

SUB GetToNextDelimit (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This sub parses through a string looking for the next character that
	' the calling function identified as a delimiter.
	'----------------------------------------------------------------------

	DIM char$
	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	FOR itr% = x% TO LEN(codeLine$)
		x% = itr%
		char$ = MID$(codeLine$, x%, 1)
		IF INSTR(del$, char$) > 0 THEN EXIT FOR
		IF INSTR(del$, " ") > 0 AND char$ = CHR_TAB THEN EXIT FOR
	NEXT itr%
	x% = itr%

END SUB

FUNCTION CutAtNextDelimit$ (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This is a main parsing function designed to "cut" a codeWord out
	' of a string. First it gets to the next non-space character. Then, it
	' gets to the next character in the string that's in the del$ string.
	' The characters between these two points will be returned as the
	' codeWord. If a space character is in the del$ string, then both
	' spaces and tabs will be treated as delimiter characters.
	'----------------------------------------------------------------------

	DIM lastBreak%
	DIM char$

	GetToNextChar(codeLine$, x%)
	lastBreak% = x%

	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	FOR itr% = x% TO LEN(codeLine$)
		x% = itr%
		char$ = MID$(codeLine$, x%, 1)
		IF MID$(codeLine$, x%, 2) = "//" THEN
			IF CharNum%(del$, "/") > 1 THEN EXIT FOR
		ELSEIF char$ = "/" THEN
			IF CharNum%(del$, "/") = 1 OR CharNum%(del$, "/") > 2 THEN EXIT FOR
		ELSEIF INSTR(del$, char$) > 0 THEN
			EXIT FOR
		ELSEIF INSTR(del$, " ") > 0 AND char$ = CHR_TAB THEN
			EXIT FOR
		END IF
	NEXT itr%
	x% = itr%

	CutAtNextDelimit$ = Trim$(MID$(codeLine$, lastBreak%, x% - lastBreak%))

END FUNCTION

FUNCTION ReverseCutAtDelimit$ (codeLine$, x%, del$)

	'This function does not feature the
	'intuitive delimiter-guessing that
	'the original CAND has.

	DIM lastBreak%
	lastBreak% = x%
	
	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	FOR itr% = x% TO 1 STEP -1
		x% = itr%
		IF INSTR(del$, MID$(codeLine$, x%, 1)) > 0 THEN EXIT FOR
	NEXT itr%
	x% = itr%

	ReverseCutAtDelimit$ = LCASE$(Trim$(MID$(codeLine$, x% + 1, lastBreak%)))

END FUNCTION

SUB CheckExpType (expect$, theType$, valDesc$)

	'----------------------------------------------------------------------
	' This sub is called to make sure that the type that was found is
	' included in the list of expected types. We allow numerical types
	' to be interchangeable because this is often needed in cogscript.
	'----------------------------------------------------------------------

	DIM isErr%
	isErr% = 0

	IF INSTR(expect$, theType$) < 1 THEN
		IF expect$ = "int" AND (INSTR(theType$, "flex") > 0 OR INSTR(theType$, "float") > 0) THEN
			'trap
		ELSEIF expect$ = "flex" AND (INSTR(theType$, "int") > 0 OR INSTR(theType$, "float") > 0) THEN
			'trap
		ELSEIF expect$ = "float" AND (INSTR(theType$, "int") > 0 OR INSTR(theType$, "flex") > 0) THEN
			'trap
		ELSE
			isErr% = 1
		END IF
	END IF

	IF isErr% = 1 THEN PrintError(0, "Type mismatch. " + expect$ + " value expected, but " + theType$ + " found in " + valDesc$ + " on line" + STR$(lineNum%) + ".")

END SUB

FUNCTION CheckForBlankLine% (codeLine$, startPos%)

	'----------------------------------------------------------------------
	' This function starts at the given position in a string and looks for
	' non-space characters. It returns 1 for a blank line and 0 if it finds
	' a non-space character. startPos% is used so that the original pos var
	' is not changed.
	'----------------------------------------------------------------------

	DIM isBlank%
	DIM char$
	DIM x%
	isBlank% = 1

	FOR x% = startPos% TO LEN(codeLine$)
		char$ = MID$(codeLine$, x%, 1)
		IF char$ <> " " AND char$ <> CHR_TAB THEN
			IF char$ = "#" OR MID$(codeLine$, x%, 2) = "//" THEN
				EXIT FOR
			ELSE
				isBlank% = 0
				EXIT FOR
			END IF
		END IF
	NEXT x%

	CheckForBlankLine% = isBlank%

END FUNCTION

SUB CheckForExtConflict (exten$(), extCount%)

	'----------------------------------------------------------------------
	' This function checks to see if a symbol is using conflicting symbol
	' extensions. EG, you can't use local and mask together.
	'----------------------------------------------------------------------

	DIM hasLocal%
	DIM hasNolink%
	DIM hasLinkid%
	DIM hasMask%
	DIM x%
	DIM y%
	
	hasLocal% = 0
	hasNolink% = 0
	hasLinkid% = 0
	hasMask% = 0

	FOR x% = 1 TO extCount%
		IF exten$(x%) = "local" THEN
			hasLocal% = 1
		ELSEIF exten$(x%) = "nolink" THEN
			hasNolink% = 1
		ELSEIF exten$(x%) = "linkid" THEN
			hasLinkid% = 1
		ELSEIF exten$(x%) = "mask" THEN
			hasMask% = 1
		END IF
	NEXT x%

	IF hasLocal% = 1 THEN
		IF hasNolink% = 1 OR hasLinkid% = 1 OR hasMask% = 1 THEN PrintError(0, "Symbol extension conflict on line" + STR$(lineNum%) + ".")
	ELSEIF hasNolink% = 1 THEN
		IF hasMask% = 1 OR hasLinkid% = 1 THEN PrintError(0, "Symbol extension conflict on line" + STR$(lineNum%) + ".")
	END IF

	'In contrast to the neat code above, we have this code
	'below to check for duplicate symbol extensions:

	FOR x% = 1 TO extCount%
		FOR y% = 1 TO extCount%
			IF y% <> x% AND exten$(y%) = exten$(x%) AND exten$(x%) <> "null" THEN
				PrintError(0, "Duplicate symbol extension, " + exten$(x%) + ", found on line" + STR$(lineNum%) + ".")
				exten$(y%) = "null"
			END IF
		NEXT y%
	NEXT x%

END SUB

SUB CheckSymExt (theType$, exten$, extVal$)

	'----------------------------------------------------------------------
	' This function checks a symbol's extension to make sure that it's
	' valid for it's type.
	'----------------------------------------------------------------------

	'--------------- Make sure exten is known -----------------------------

	IF InStringArray%(symExt$(), exten$) = 0 THEN
		PrintError(0, "Invalid symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + ".")
	ELSE
		'------- Make sure exten fits type ----------------------------
		IF theType$ = "message" AND exten$ <> "desc" THEN
			PrintError(0, "The extension, " + exten$ + ", used on line" + STR$(lineNum%) + " is not valid for a symbol of the " + theType$ + " type.")
		ELSEIF theType$ <> "sector" AND theType$ <> "surface" AND theType$ <> "thing" AND exten$ <> "local" AND exten$ <> "desc" THEN
			PrintError(0, "The extension, " + exten$ + ", used on line" + STR$(lineNum%) + " is not valid for the " + theType$ + " symbol type.")
		END IF
	END IF

	'--------------- Check exten's value ----------------------------------

	IF (exten$ = "local" OR exten$ = "nolink") AND extVal$ <> "null" THEN
		PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " should not have a value.")

	ELSEIF exten$ = "desc" THEN
		IF extVal$ = "null" THEN PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " requires a value.")
		'The desc value may include delimiters and odd characters, so we can't really check it.
		'IF IsValidName%(extVal$) = 0 THEN PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " has an invalid value.")

	ELSEIF exten$ = "mask" OR exten$ = "linkid" THEN
		IF extVal$ = "null" THEN PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " requires a value.")
		IF IsValidInt%(extVal$) = 0 THEN PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " has an invalid value.")
	END IF

END SUB

SUB CheckSymName (theType$, theName$)

	'----------------------------------------------------------------------
	' This function checks the symbol name given to it to make sure that it
	' can be used.
	'----------------------------------------------------------------------

	DIM char%
	
	'--------------- Check First Char -------------------------------------

	char% = ASC(MID$(theName$, 1, 1))
	IF char% < 95 OR char% > 122 THEN PrintError(0, "The symbol, " + theName$ + ", begins with a numerical character.")

	IF theType$ = "message" THEN
		IF InStringArray%(message$(), theName$) = 0 THEN PrintError(0, "Unknown message, " + theName$ + ", declared on line" + STR$(lineNum%) + ".")
	ELSE
		IF IsValidName%(theName$) = 0 THEN
			PrintError(0, "Symbol name, " + theName$ + ", on line" + STR$(lineNum%) + " is invalid.")
		ELSEIF InStringArray%(message$(), theName$) > 0 THEN
			PrintError(0, "Warning: symbol name, " + theName$ + ", on line" + STR$(lineNum%) + " is a message name and using it will void the message.")
		ELSEIF InStringArray%(verbName$(), theName$) > 0 THEN
			PrintError(0, "Warning: symbol name, " + theName$ + ", on line" + STR$(lineNum%) + " is a verb name and using it will void the verb.")
		END IF
	END IF

	'--------------- Check for Duplicate Name -----------------------------

	IF InStringArray%(varName$(), theName$) > 0 THEN PrintError(0, "The symbol name, " + theName$ + ", on line" + STR$(lineNum%) + " has already been used.")

END SUB

SUB CheckSymType (theType$)

	'----------------------------------------------------------------------
	' This function checks the given symbol type to make sure that it is a
	' known type.
	'----------------------------------------------------------------------

	IF InStringArray%(symType$(), theType$) = 0 THEN PrintError(0, "Invalid symbol type, " + theType$ + ", found on line" + STR$(lineNum%) + ".")

END SUB

SUB CheckSymVal (theType$, initVal$)

	'----------------------------------------------------------------------
	' This function checks a symbol's initial value to make sure that it's
	' valid for its type.
	'----------------------------------------------------------------------

	DIM x%
	DIM ext$
	DIM fileName$

	IF initVal$ = "null" THEN EXIT SUB

	IF theType$ = "ai" OR theType$ = "keyframe" OR theType$ = "material" OR theType$ = "model" OR theType$ = "sound" THEN
		x% = 1
		fileName$ = CutAtNextDelimit$(initVal$, x%, ".")
		ext$ = MID$(initVal$, x% + 1, LEN(initVal$) - x% + 1)

		IF theType$ = "ai" AND ext$ <> "ai" AND ext$ <> "ai0" AND ext$ <> "ai2" THEN
			beenError% = 1
		ELSEIF theType$ = "keyframe" AND ext$ <> "key" THEN
			beenError% = 1
		ELSEIF theType$ = "material" AND ext$ <> "mat" THEN
			beenError% = 1
		ELSEIF theType$ = "model" AND ext$ <> "3do" THEN
			beenError% = 1
		ELSEIF theType$ = "sound" AND ext$ <> "wav" THEN
			beenError% = 1
		END IF

		IF beenError% = 1 THEN PrintError(0, "Invalid file extension, " + ext$ + ", found for the initial value of the symbol on line" + STR$(lineNum%) + ".")
		IF IsValidName%(fileName$) = 0 THEN PrintError(0, "A symbol on line" + STR$(lineNum%) + " is assigned to a file name that may be invalid.")

	ELSEIF theType$ = "vector" OR theType$ = "message" THEN
		PrintError(0, "The symbol of type, " + theType$ + ", used on line" + STR$(lineNum%) + " cannot have an initial value.")

	ELSEIF theType$ = "flex" OR theType$ = "float" THEN
		IF IsValidFlex%(initVal$) = 0 THEN PrintError(0, "Invalid initial value, " + initVal$ + ", found for the symbol on line" + STR$(lineNum%) + ".")

	ELSEIF theType$ = "cog" OR theType$ = "thing" OR theType$ = "sector" OR theType$ = "surface" OR theType$ = "int" THEN
		IF IsValidInt%(initVal$) = 0 THEN PrintError(0, "Invalid initial value, " + initVal$ + ", for the symbol on line" + STR$(lineNum%) + ".")

	ELSEIF theType$ = "template" THEN
		FOR x% = 1 TO LEN(initVal$)
			charint% = ASC(MID$(initVal$, x%, 1))
			'if charint is not a 0-9 number, a lowercase letter, an underscore, or a plus, then there's an error.
			IF (charint% < 48 OR charint% > 57) AND (charint% < 97 OR charint% > 122) AND charint% <> 43 AND charint% <> 95 THEN
				PrintError(0, "Invalid template name, " + initVal$ + ", found for the symbol on line" + STR$(lineNum%) + ".")
			END IF
		NEXT x%
	END IF

END SUB

SUB DoAssignment (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This sub parses through a variable assignment. It is called by
	' DoStatement. (no longer called by DoFor explicitly)
	'----------------------------------------------------------------------

	DIM var$

	var$ = CutAtNextDelimit$(codeLine$, x%, " ;()=[#//")
	IF MID$(codeLine$, x%, 1) = "[" THEN
		x% = x% + 1
		DoSubExp(codeLine$, x%, "]", "int", "the array access")     'process the expression.
		numArrayVars% = numArrayVars% + 1
		arrayVar$(numArrayVars%) = var$
	END IF

	IF InStringArray%(varName$(), var$) = 0 THEN PrintError(0, "The variable, " + var$ + ", in the assignment on line" + STR$(lineNum%) + " is undefined.")
	IF GetVarType(var$) = "message" THEN
		PrintError(0, "Warning: the symbol constant, " + var$ + ", is assigned on line" + STR$(lineNum%) + ", but its value should not be changed.")
	END IF

	IF InStringArray%(assignVar$(), var$) = 0 THEN
		numAssignVars% = numAssignVars% + 1
		assignVar$(numAssignVars%) = var$
	END IF
	
	FindNextChar(codeLine$, x%, 0)
	IF MID$(codeLine$, x%, 1) <> "=" THEN PrintError(1, "An = expected after the variable, " + var$ + ", on line" + STR$(lineNum%) + ".")
	x% = x% + 1

	DoSubExp(codeLine$, x%, del$, GetVarType$(var$), "the expression after the " + var$ + " assignment")              'note that the calling sub chooses the delimit that the assignment should end with.

END SUB

SUB DoBlock (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoBlock is called by DoStatement to parse compound statements (AKA
	' "code blocks"). DoBlock, in turn, calls on DoStatement to parse
	' individual statements.
	'----------------------------------------------------------------------

	DIM startLine%
	DIM blockEnd%
	DIM MsgReturn%
	DIM lastMR%
	DIM hasCode%

	startLine% = lineNum%
	blockEnd% = 0
	MsgReturn% = 0
	lastMR% = 0
	hasCode% = 0
	x% = x% + 1     'get past the "{"

	DO WHILE NOT EOF(1)
		DO WHILE x% <= LEN(codeLine$) AND CheckForBlankLine%(codeLine$, x%) = 0 AND blockEnd% = 0
			DoStatement(codeLine$, x%, blockEnd%, MsgReturn%, 1)
			IF blockEnd% = 0 THEN hasCode% = 1
			IF lastMR% = 1 AND blockEnd% = 0 THEN PrintError(0, "Extra code found after a direct return in the code block on line" + STR$(lineNum%) + ".")
			lastMR% = MsgReturn%
		LOOP
		IF blockEnd% = 1 THEN EXIT DO

		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		x% = 1
	LOOP

	IF hasCode% = 0 THEN PrintError(0, "No code found in the code block beginning on line" + STR$(startLine%) + ".")
	IF blockEnd% = 0 THEN PrintError(1, "No end to the code block begun on line" + STR$(startLine%) + ".")

END SUB

SUB DoCall (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoCall processes call statements. It is called after the "call"
	' keyword has been identified, so it only has to verify and log the
	' message.
	'----------------------------------------------------------------------

	DIM message$

	FindNextChar(codeLine$, x%, 0)       'message may be on the next line(s)
	message$ = CutAtNextDelimit$(codeLine$, x%, " ;#//")

	IF IsValidName%(message$) = 0 THEN PrintError(0, "Invalid message name, " + message$ + ", found after the call statement on line" + STR$(lineNum%) + ".")

	numCalls% = numCalls% + 1
	CallMessage$(numCalls%) = message$

END SUB

SUB DoDo (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub is called to process Do..while loops.
	'----------------------------------------------------------------------

	DIM key$
	
	'--------------- Check the statement or block after the "do" ----------

	FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		DoBlock(codeLine$, x%)
	ELSE
		DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'--------------- Check for the while statement ------------------------

	FindNextChar(codeLine$, x%, 1)
	key$ = CutAtNextDelimit$(codeLine$, x%, " (#//")

	IF key$ = "while" THEN
		FindNextChar(codeLine$, x%, 0)
		IF MID$(codeLine$, x%, 1) <> "(" THEN PrintError(1, "No expression found after the while keyword on line" + STR$(lineNum%) + ".")
		x% = x% + 1      'pass the "("
		DoSubExp(codeLine$, x%, ")", "int", "the while statement")
		DoSemi(codeLine$, x%)
	ELSE
		PrintError(1, "No while keyword found for the do..while loop on line" + STR$(lineNum%) + ".")
	END IF

END SUB

SUB DoFor (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub is called to process For loops.
	'----------------------------------------------------------------------

	DIM key$
	DIM char$
	
	'--------------- Check the variable init ------------------------------

	FindNextChar(codeLine$, x%, 0)      'get to the paren
	x% = x% + 1                              'get past open paren
	FindNextChar(codeLine$, x%, 0)     'semicolon may be on next line(s)
	IF NextCharInLine$(codeLine$, x%) <> ";" THEN
		DO
			FindNextChar(codeLine$, x%, 0)
			IF NextCharInLine(codeLine$, x%) = "" THEN
				PrintError(1, "Unable to parse for loop sub-statement on line" + STR$(lineNum%) + ". Unexpected line break.")
			END IF
			DoForStatement(codeLine$, x%)
			FindNextChar(codeLine$, x%, 0)
		LOOP WHILE NextCharInLine(codeLine$, x%) <> ";"
	END IF

	x% = x% + 1                             'get past it

	'--------------- Check the condition ----------------------------------

	FindNextChar(codeLine$, x%, 0)
	IF NextCharInLine$(codeLine$, x%) <> ";" THEN
		DoSubExp(codeLine$, x%, ";", "int", "the for statement's condition")
		DoSemi(codeLine$, x%)
	ELSE
		x% = x% + 1
	END IF

	'--------------- Parse the incrementation -----------------------------

	DO
		FindNextChar(codeLine$, x%, 0)
		IF NextCharInLine(codeLine$, x%) = "" THEN
			PrintError(1, "Unable to parse for loop sub-statement on line" + STR$(lineNum%) + ". Unexpected line break.")
		END IF
		DoForStatement(codeLine$, x%)
		FindNextChar(codeLine$, x%, 0)

	LOOP WHILE NextCharInLine$(codeLine$, x%) <> ")"

	x% = x% + 1     'pass the ')'

	'--------------- Check the statement or block after the expression ----

	FindNextChar(codeLine$, x%, 1)
	char$ = MID$(codeLine$, x%, 1)

	IF char$ = ";" THEN
		PrintError(0, "Warning: The for statement on line" + STR$(lineNum%) + " is ended with a semicolon.")
		x% = x% + 1     'get past the semicolon.
	ELSEIF char$ = "{" THEN
		DoBlock(codeLine$, x%)
	ELSE
		DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

END SUB

SUB DoForStatement (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoForStatement is called by DoFor to parse sub-statements within
	' the for loop's body. Only variable assignments and verbs
	' are allowed for these sub-statements.
	'----------------------------------------------------------------------

	DIM codeWord%
	DIM lastBreak%
	DIM verbNum%

	lastBreak% = x%
	

	codeWord$ = CutAtNextDelimit$(codeLine$, x%, " =,();[#//")
	nextChar$ = NextCharInLine$(codeLine$, x%)

	IF (strictParse% = 1 AND nextChar$ = "(") OR (strictParse% = 0 AND LEN(codeWord$) > 0 AND InStringArray(verbName$(), codeWord$)) THEN
		x% = lastBreak%
		verbNum% = DoFunction%(codeLine$, x%)
		IF verbNum% > 0 THEN IF verbAct%(verbNum%) = 0 THEN PrintError(0, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " as a seperate statement, but it does not perform an action.")
		
	ELSEIF IsValidName%(codeWord$) = 1 AND (strictParse% = 0 OR (strictParse% = 1 AND (nextChar$ = "=" OR nextChar$ = "["))) THEN
		x% = lastBreak%
		DoAssignment(codeLine$, x%, ",;)")
		IF NextCharInLine(codeLine$, x% - 1) = ")" THEN
			'DoSubExp will skip the ")" delimiter, so we'll go back one.
			x% = x% - 1
		ELSEIF NextCharInLine(codeLine$, x% - 1) = "," THEN
			FindNextChar(codeLine$, x%, 0)
			IF NextCharInLine$(codeLine$, x%) = ")" OR NextCharInLine$(codeLine$, x%) = ";" THEN
				PrintError(1, "For sub-statement expected, but " + NextCharInLine(codeLine$, x%) + " found on line" + STR$(lineNum%) + ".")
			END IF
		END IF
		
	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			PrintError(1, "For sub-statement expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			PrintError(1, "For sub-statement expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			PrintError(1, "For sub-statement expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		END IF
	END IF

END SUB

FUNCTION DoFunction% (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoFunction checks a function to make sure it's known and then parses
	' its arguments.
	'----------------------------------------------------------------------

	DIM funcName$
	DIM argNum%
	DIM paramNum%
	DIM verbNum%
	
	'--------------- Make sure the function is known ----------------------

	funcName$ = CutAtNextDelimit$(codeLine$, x%, "(#//")
	verbNum% = InStringArray%(verbName$(), funcName$)

	IF (verbNum% = 0) OR (LEN(funcName$) < 1) THEN PrintError(1, "Unknown verb, " + funcName$ + ", found on line" + STR$(lineNum%) + ".")
	paramNum% = VAL(verbParNum$(verbNum%))
	
	'--------------- Make sure this is not a bad verb ---------------------

	IF verbRet$(verbNum%) = "!" THEN PrintError(0, "The verb, " + funcName$ + ", found on line" + STR$(lineNum%) + " does not work properly.")
	
	'--------------- Parse function's arguments ---------------------------

	FindNextChar(codeLine$, x%, 0) 'find begin paren
	x% = x% + 1             'get past begin paren.
	FindNextChar(codeLine$, x%, 0)

	IF NextCharInLine$(codeLine$, x%) = ")" THEN
		argNum% = 0
		IF argNum% <> paramNum% THEN PrintError(0, "Expected parameters for the verb, " + funcName$ + ", on line" + STR$(lineNum%) + ".")
		x% = x% + 1
	ELSE
		FOR argNum% = 1 TO paramNum%
			IF argNum% = paramNum% THEN
				DoSubExp(codeLine$, x%, ")", verbParam$(verbNum%, argNum%), funcName$)
			ELSE
				DoSubExp(codeLine$, x%, ",", verbParam$(verbNum%, argNum%), funcName$)
			END IF
		NEXT argNum%
	END IF

	DoFunction% = verbNum%

END FUNCTION

SUB DoIf (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub is called to process if statements and the else statements
	' after them.
	'----------------------------------------------------------------------

	DIM key$
	DIM savex%

	'--------------- Check the Expression ---------------------------------

	FindNextChar(codeLine$, x%, 0)
	x% = x% + 1     'get past the "("
	DoSubExp(codeLine$, x%, ")", "int", "the if statement")
	
	'--------------- Check the statement or block after the expressionm ---

	FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		DoBlock(codeLine$, x%)
	ELSE
		DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'--------------- Check for an else statement --------------------------

	FindNextChar(codeLine$, x%, 1)
	savex% = x%
	key$ = CutAtNextDelimit$(codeLine$, x%, " #{//")
	
	IF key$ = "else" THEN
		FindNextChar(codeLine$, x%, 1)
		IF MID$(codeLine$, x%, 1) = "{" THEN
			DoBlock(codeLine$, x%)
		ELSE
			DoStatement(codeLine$, x%, 0, 0, 0)
			IF statExec% = 0 THEN PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
		END IF
	ELSE
		 'Since we did not find the else, we must set x% back to just before the key$.
		 x% = savex%
	END IF

END SUB

SUB DoLabel (codeWord$, x%)

	IF IsValidName%(codeWord$) = 0 THEN PrintError(0, "Invalid message, " + codeWord$ + ", found on line" + STR$(lineNum%) + ".")
	IF InStringArray%(CodeMessage$(), codeWord$) > 0 THEN PrintError(0, "The message, " + codeWord$ + ", on line" + STR$(lineNum%) + " has already been used in the code section.")
	IF InStringArray%(message$(), codeWord$) > 0 THEN PrintError(0, "Warning: No return before the message, " + codeWord$ + ", on line" + STR$(lineNum%) + ".")

	numCM% = numCM% + 1
	CodeMessage$(numCM%) = codeWord$
	numUsedVars% = numUsedVars% + 1
	usedVar$(numUsedVars%) = codeWord$

	x% = x% + 1     'get past the ":"

END SUB

SUB DoMessage (codeLine$, x%, MsgName$)

	DIM MsgReturn%
	MsgReturn% = 0

	DO WHILE NOT EOF(1)
		DO WHILE x% <= LEN(codeLine$) AND MsgReturn% = 0 AND CheckForBlankLine%(codeLine$, x%) = 0
			DoStatement(codeLine$, x%, 0, MsgReturn%, 0)
		LOOP
		IF MsgReturn% = 1 THEN EXIT DO
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		x% = 1
	LOOP

	UpdateBar

END SUB

SUB DoSemi (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoSemi checks for a semicolon at the given coordinates.
	'----------------------------------------------------------------------

	FindNextChar(codeLine$, x%, 0)       'get to the semicolon
	IF MID$(codeLine$, x%, 1) <> ";" THEN PrintError(1, "No semicolon after the statement on line" + STR$(lineNum%) + ".")
	x% = x% + 1                             'get past the semicolon

END SUB

SUB DoStatement (codeLine$, x%, blockEnd%, MsgReturn%, which%)

	'----------------------------------------------------------------------
	' DoStatement is called mainly by DoMessage and DoBlock to parse
	' seperate statements. This sub is the main statement parser for for
	' the code section. It calls various other subs depending on the first
	' few words and delimiters.
	'----------------------------------------------------------------------

	DIM codeWord%
	DIM lastBreak%
	DIM verbNum%

	lastBreak% = x%
	statExec% = 1   'statExec tells the caller whether or not the statement parsed is executable.

	codeWord$ = CutAtNextDelimit$(codeLine$, x%, " =();{}[:#//")
	nextChar$ = NextCharInLine$(codeLine$, x%)

	IF codeWord$ = "if" AND (strictParse% = 0 OR (strictParse% = 1 AND nextChar$ = "(")) THEN
		DoIf(codeLine$, x%)
	ELSEIF codeWord$ = "call" OR codeWord$ = "goto" THEN
		DoCall(codeLine$, x%)
		DoSemi(codeLine$, x%)
	ELSEIF codeWord$ = "do" THEN
		DoDo(codeLine$, x%)
	ELSEIF codeWord$ = "while" AND (strictParse% = 0 OR (strictParse% = 1 AND nextChar$ = "(")) THEN
		DoWhile(codeLine$, x%)
	ELSEIF codeWord$ = "for" AND (strictParse% = 0 OR (strictParse% = 1 AND nextChar$ = "(")) THEN
		DoFor(codeLine$, x%)
	ELSEIF codeWord$ = "return" OR codeWord$ = "stop" THEN
		DoSemi(codeLine$, x%)
		MsgReturn% = 1
	ELSEIF nextChar$ = "{" AND LEN(codeWord$) = 0 THEN
		PrintError(0, "Beginning of useless code block found on line" + STR$(lineNum%) + ".")
		DoBlock(codeLine$, x%)    'this code block is technically useless, though it is syntactically correct.
	ELSEIF nextChar$ = "}" AND LEN(codeWord$) = 0 THEN
		IF which% = 1 THEN
			x% = x% + 1     'get past the bracket
			blockEnd% = 1
		ELSE
			PrintError(1, "Extra end bracket found on line" + STR$(lineNum%) + ".")
		END IF
	ELSEIF (strictParse% = 1 AND nextChar$ = "(") OR (strictParse% = 0 AND LEN(codeWord$) > 1 AND InStringArray(verbName$(), codeWord$)) THEN
		x% = lastBreak%
		verbNum% = DoFunction%(codeLine$, x%)
		IF verbNum% > 0 THEN IF verbAct%(verbNum%) = 0 THEN PrintError(0, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " as a seperate statement, but it does not perform an action.")
		DoSemi(codeLine$, x%)
		
	ELSEIF nextChar$ = ":" THEN
		DoLabel(codeWord$, x%)
		statExec% = 0

	ELSEIF IsValidName%(codeWord$) = 1 AND (strictParse% = 0 OR (strictParse% = 1 AND (nextChar$ = "=" OR nextChar$ = "["))) THEN
		x% = lastBreak%
		DoAssignment(codeLine$, x%, ",;)")
		DoSemi(codeLine$, x%)

	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			PrintError(1, "Statement expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			PrintError(1, "Statement expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			PrintError(1, "Statement expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		END IF
	END IF

END SUB

SUB DoString (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoString is called by DoValue when it encounters a double quote.
	'----------------------------------------------------------------------

	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	'+1 to get past opening quotes
	FOR itr% = x% + 1 TO LEN(codeLine$)
		x% = itr%
		IF ASC(MID$(codeLine$, x%, 1)) = 34 THEN EXIT FOR
	NEXT itr%
	x% = itr%

	IF x% > LEN(codeLine$) THEN PrintError(1, "No end to the string value on line" + STR$(lineNum%) + ".")
	x% = x% + 1     'get past end quote

END SUB

SUB DoSubExp (codeLine$, x%, del$, expType$, valDesc$)

	'----------------------------------------------------------------------
	' DoSubExp is called to evaluate expressions. Any expression found or
	' expected in the code section will be sent to this sub.
	'----------------------------------------------------------------------

	DIM char$
	DIM hasExp%
	DIM expect$
	DIM altChar$

	expect$ = "value"

	DO WHILE NOT EOF(1)
		FindNextChar(codeLine$, x%, 0)
		char$ = MID$(codeLine$, x%, 1)
		altChar$ = MID$(codeLine$, x%, 2)

		IF IsUnaryOp%(char$) = 1 AND expect$ = "value" THEN
			IF LEN(expType$) > 1 THEN CheckExpType(expType$, "int flex float", valDesc$)
			x% = x% + 1

		ELSEIF expect$ = "value" AND char$ = "(" THEN
			x% = x% + 1
			DoSubExp(codeLine$, x%, ")", expType$, "a sub-expression")
			expect$ = "op/end"

		ELSEIF expect$ = "value" THEN
			theType$ = DoValue$(codeLine$, x%, expType$)
			'PrintError(0, "Warning:" + theType$)
			IF LEN(expType$) > 1 THEN CheckExpType(expType$, theType$, valDesc$)
			expect$ = "op/end"

		ELSEIF IsOperator%(altChar$) = 1 AND expect$ = "op/end" THEN    'note that we check for two-char ops first.
			expect$ = "value"
			IF LEN(expType$) > 1 THEN CheckExpType(expType$, "int, flex, or float operator", valDesc$)    'make sure the last value's type can be used with operators.
			x% = x% + 2     'get past second char of operator.

		ELSEIF IsOperator%(char$) = 1 AND expect$ = "op/end" THEN
			expect$ = "value"
			IF LEN(expType$) > 1 THEN CheckExpType(expType$, "int, flex, or float operator", valDesc$)
			x% = x% + 1

		ELSEIF INSTR(del$, char$) > 0 AND LEN(char$) > 0 THEN
			'in most cases, the semicolon is checked by seperate subs, so we won't pass it here.
			IF char$ <> ";" THEN x% = x% + 1   'get past the delimiter.
			EXIT DO
		ELSE
			IF LEN(char$) < 1 THEN PrintError(1, "Unexpected end of line in " + valDesc$ + " on line" + STR$(lineNum%) + ".")
			PrintError(1, "Unexpected " + char$ + " in " + valDesc$ + " on line" + STR$(lineNum%) + ".")
		END IF
	LOOP

	IF expect$ = "value" THEN PrintError(1, "Value expected in " + valDesc$ + " on line" + STR$(lineNum%) + ", but none found.")

END SUB

FUNCTION DoValue$ (codeLine$, x%, expType$)

	'----------------------------------------------------------------------
	' DoValue will be called when DoSubExp needs to process a value. The
	' value can be a function, number, variable, string, vector, etc.
	'----------------------------------------------------------------------

	DIM theType$
	DIM lastBreak%
	DIM verbNum%

	lastBreak% = x%
	codeWord$ = CutAtNextDelimit$(codeLine$, x%, " ;()[],==!=>=<=&&||%*/-+'#//" + CHR$(34))
	nextChar$ = NextCharInLine$(codeLine$, x%)

	IF nextChar$ = CHR$(34) THEN    'Note that we check for a string before we check the length of codeWord$
		DoString(codeLine$, x%)   'Strings can contain other delimiters so we can't use an IsValid function.
		theType$ = "string"

	ELSEIF nextChar$ = "'" THEN     'Same as above.
		DoVector(codeLine$, x%)
		theType$ = "vector"

	ELSEIF LEN(codeWord$) < 1 THEN
		PrintError(1, "Value expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")

	ELSEIF nextChar$ = "(" THEN
		x% = lastBreak%
		verbNum% = DoFunction%(codeLine$, x%)
		theType$ = verbRet$(verbNum%)
		IF theType$ = "void" THEN PrintError(1, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " in an expression.")

	ELSEIF IsValidInt%(codeWord$) = 1 THEN
		theType$ = "int"

	ELSEIF IsValidFlex%(codeWord$) = 1 THEN
		theType$ = "flex"

	ELSEIF InStringArray%(message$(), codeWord$) > 0 AND InStringArray%(varName$(), codeWord$) = 0 THEN
		theType$ = "message"

	ELSEIF IsValidName%(codeWord$) = 1 THEN
		IF nextChar$ = "[" THEN
			x% = x% + 1                         'get past the "["
			DoSubExp(codeLine$, x%, "]", "int", "the array access")  'evaluate the expression
			numArrayVars% = numArrayVars% + 1
			arrayVar$(numArrayVars%) = codeWord$
		END IF

		IF InStringArray%(usedVar$(), codeWord$) = 0 THEN
			numUsedVars% = numUsedVars% + 1
			usedVar$(numUsedVars%) = codeWord$
		END IF

		IF InStringArray%(varName$(), codeWord$) = 0 THEN PrintError(0, "The variable, " + codeWord$ + ", found in the expression on line" + STR$(lineNum%) + " is undefined.")
		theType$ = GetVarType$(codeWord$)
	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			PrintError(1, "Value expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			PrintError(1, "Value expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			PrintError(1, "Value expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		END IF
	END IF

	DoValue$ = SubType(theType$)

END FUNCTION

SUB DoVector (codeLine$, x%)

	DIM lastBreak%
	lastBreak% = x%

	' FreeBASIC migration - Use local iterator variable to avoid "error 51: Expected scalar counter..."
	DIM itr%
	'+1 to get past opening quote
	FOR itr% = x% + 1 TO LEN(codeLine$)
		x% = itr%
		IF MID$(codeLine$, x%, 1) = "'" THEN EXIT FOR
	NEXT itr%
	x% = itr%

	codeWord$ = MID$(codeLine$, lastBreak%, x% - lastBreak% + 1)
	IF IsValidVector%(codeWord$) = 0 THEN PrintError(1, "Invalid vector, " + codeWord$ + ", found in an expression on line" + STR$(lineNum%) + ".")
	x% = x% + 1     'get past end quote

END SUB

SUB DoWhile (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoWhile is called by DoStatement to process while loops.
	'----------------------------------------------------------------------

	'--------------- Check the Expression ---------------------------------

	FindNextChar(codeLine$, x%, 0)
	x% = x% + 1     'get past the "("
	DoSubExp(codeLine$, x%, ")", "int", "the while statement")
	
	'--------------- Check the statement or block after the expression ----

	FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		DoBlock(codeLine$, x%)
	ELSE
		DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'----------------------------------------------------------------------

END SUB

SUB FindNextChar (codeLine$, x%, multiLine%)

	'----------------------------------------------------------------------
	' FindNextChar is called when a sub needs the position of the
	' next character in the code section. This sub will go through multiple
	' lines looking for the next char unlike other one-line-only parsing
	' subs.
	'----------------------------------------------------------------------

	IF strictParse% = 1 AND multiLine% = 0 THEN
		GetToNextChar(codeLine$, x%)
		EXIT SUB
	END IF

	DO WHILE NOT EOF(1)
		IF CheckForBlankLine%(codeLine$, x%) = 0 THEN
			GetToNextChar(codeLine$, x%)
			EXIT DO
		END IF
		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		x% = 1
	LOOP

END SUB

FUNCTION GetArrayPrefix$ (codeWord$)

	DIM x%
	DIM charint%

	FOR x% = 1 TO LEN(codeWord$)
		charint% = ASC(MID$(codeWord$, x%, 1))
		IF charint% >= 48 AND charint% <= 57 THEN
			GetArrayPrefix$ = MID$(codeWord$, 1, x% - 1)
			EXIT FOR
		END IF
	NEXT x%

END FUNCTION

FUNCTION GetVarType$ (var$)

	'----------------------------------------------------------------------
	' GetVarType returns the type that the variable was given in the
	' symbols section. Note that this function assumes that the variable
	' has been defined.
	'----------------------------------------------------------------------

	DIM a%
	FOR a% = 1 TO varCount%
		IF var$ = varName$(a%) THEN
			GetVarType$ = varType$(a%)
			EXIT FOR
		END IF
	NEXT a%

END FUNCTION

FUNCTION IsOperator% (char$)

	'----------------------------------------------------------------------
	' This function checks to see if the given string is a known, non-unary
	' operator. Note that char$ is not strictly one character in the case
	' of some operators. A Pascal set would be nice here... *sigh*
	'----------------------------------------------------------------------

	DIM valid%
	valid% = 0
	IF char$ = "+" OR char$ = "-" OR char$ = "*" OR char$ = "/" THEN
		valid% = 1
	ELSEIF char$ = "%" OR char$ = "==" OR char$ = ">" OR char$ = "<" THEN
		valid% = 1
	ELSEIF char$ = ">=" OR char$ = "<=" OR char$ = "&" OR char$ = "|" THEN
		valid% = 1
	ELSEIF char$ = "&&" OR char$ = "||" OR char$ = "!=" THEN
		valid% = 1
	END IF
	IsOperator% = valid%

END FUNCTION

FUNCTION IsUnaryOp% (char$)

	IF char$ = "-" OR char$ = "!" THEN
		IsUnaryOp% = 1
	ELSE
		IsUnaryOp% = 0
	END IF

END FUNCTION

FUNCTION IsValidFlex% (flexString$)

	'----------------------------------------------------------------------
	' This function checks a flex string for validity.
	'----------------------------------------------------------------------

	DIM x%
	DIM valid%
	DIM charint%
	DIM hasDec%
	hasDec% = 0
	valid% = 1

	IF LEFT$(flexString$, 1) = "-" THEN flexString$ = RIGHT$(flexString$, LEN(flexString$) - 1)

	FOR x% = 1 TO LEN(flexString$)
		charint% = ASC(MID$(flexString$, x%, 1))
		IF charint% = 46 THEN           'allow only one decimal.
			IF hasDec% = 1 THEN
				valid% = 0
				EXIT FOR
			END IF
			hasDec% = 1
		END IF
		' if charint is not 0-9, a decimal, or a minus sign, there's an error.
		IF (charint% < 48 OR charint% > 57) AND charint% <> 46 AND charint% <> 45 THEN
			valid% = 0
			EXIT FOR
		END IF
	NEXT x%

	IF (LEFT$(flexString$, 1) = "." OR RIGHT$(flexString$, 1) = ".") AND (allowBadFlex% = 0 OR LEN(flexString) = 1) THEN valid% = 0
	IF CheckForBlankLine(flexString$, 1) = 1 THEN valid% = 0
	IsValidFlex% = valid%

END FUNCTION

FUNCTION IsValidInt% (inputString$)

	'----------------------------------------------------------------------
	' This function reads a string to make sure that it's a valid integer.
	' This includes flags with the 0x prefix. This function allows a minus
	' for non-hex integers.
	'----------------------------------------------------------------------

	DIM isHex%
	DIM valid%
	DIM charint%
	DIM x%
	DIM intString$
	valid% = 1
	isHex% = 0
	intString$ = inputString$

	IF LEFT$(intString$, 1) = "-" THEN intString$ = RIGHT$(intString$, LEN(intString$) - 1)

	IF LEFT$(intString$, 2) = "0x" THEN
		isHex% = 1
		intString$ = RIGHT$(intString$, LEN(intString$) - 2)
	END IF

	FOR x% = 1 TO LEN(intString$)
		charint% = ASC(MID$(intString$, x%, 1))
		IF isHex% = 1 THEN
			' if char is not 0-9 or a-f
			IF (charint% < 48 OR charint% > 57) AND (charint% < 97 OR charint% > 102) THEN
				valid% = 0
				EXIT FOR
			END IF
		ELSE
			' if char is not 0-9
			IF (charint% < 48 OR charint% > 57) THEN
				valid% = 0
				EXIT FOR
			END IF
		END IF
	NEXT x%

	IF CheckForBlankLine(intString$, 1) = 1 THEN valid% = 0
	IsValidInt% = valid%

END FUNCTION

FUNCTION IsValidName% (theName$)

	'----------------------------------------------------------------------
	' The name of this function is vague, but it will check a string to
	' make sure it contains only characters allowed for variable names.
	'----------------------------------------------------------------------

	DIM x%
	DIM valid%
	DIM charint%
	DIM hasAlpha%
	valid% = 1

	FOR x% = 1 TO LEN(theName$)
		charint% = ASC(MID$(theName$, x%, 1))
		' if charint is not 0-9 or a lowercase letter, or an underscore, or a dash, then there's an error.
		IF (charint% < 48 OR charint% > 57) AND (charint% < 97 OR charint% > 122) AND charint% <> 95 AND charint% <> 45 THEN
			IsValidName% = 0
			EXIT FUNCTION
		END IF
		IF charint% >= 97 AND charint% <= 122 THEN hasAlpha% = 1
	NEXT x%

	IF hasAlpha% = 0 THEN valid% = 0

	'Quick hack because Parsec does not have a list of reserved words...
	IF (theName$ = "end" OR theName$ = "code" OR theName$ = "if" OR theName$ = "do" OR theName$ = "while") THEN
		valid% = 0
	ELSEIF (theName$ = "for" OR theName$ = "else" OR theName$ = "call" OR theName$ = "return" OR theName$ = "stop") THEN
		valid% = 0
	END IF

	IsValidName% = valid%

END FUNCTION

FUNCTION IsValidVector% (vector$)

	'----------------------------------------------------------------------
	' This function checks a vector to make sure that it's valid.
	'----------------------------------------------------------------------

	DIM x%
	DIM valid%
	DIM xvec$
	DIM yvec$
	DIM zvec$

	xvec$ = "null"
	yvec$ = "null"
	zvec$ = "null"
	valid% = 1

	IF RIGHT$(vector$, 1) <> "'" OR LEFT$(vector$, 1) <> "'" THEN
		valid% = 0
	ELSE
		x% = 2
		xvec$ = CutAtNextDelimit$(vector$, x%, " ")
		yvec$ = CutAtNextDelimit$(vector$, x%, " ")
		zvec$ = CutAtNextDelimit$(vector$, x%, " '")
		IF NextCharInLine$(vector$, x%) <> "'" THEN valid% = 0
		IF IsValidFlex%(xvec$) = 0 OR IsValidFlex%(yvec$) = 0 OR IsValidFlex%(zvec$) = 0 THEN valid% = 0
	END IF

	IsValidVector% = valid%

END FUNCTION

FUNCTION SubType$ (theType$)

	IF theType$ = "ai" THEN
		SubType$ = aisub$
	ELSEIF theType$ = "cog" THEN
		SubType$ = cogsub$
	ELSEIF theType$ = "flex" THEN
		SubType$ = flexsub$
	ELSEIF theType$ = "float" THEN
		SubType$ = floatsub$
	ELSEIF theType$ = "int" THEN
		SubType$ = intsub$
	ELSEIF theType$ = "keyframe" THEN
		SubType$ = keyframesub$
	ELSEIF theType$ = "material" THEN
		SubType$ = materialsub$
	ELSEIF theType$ = "message" THEN
		SubType$ = messagesub$
	ELSEIF theType$ = "model" THEN
		SubType$ = modelsub$
	ELSEIF theType$ = "surface" THEN
		SubType$ = surfacesub$
	ELSEIF theType$ = "sound" THEN
		SubType$ = soundsub$
	ELSEIF theType$ = "sector" THEN
		SubType$ = sectorsub$
	ELSEIF theType$ = "template" THEN
		SubType$ = templatesub$
	ELSEIF theType$ = "thing" THEN
		SubType$ = thingsub$
	ELSEIF theType$ = "vector" THEN
		SubType$ = vectorsub$
	ELSE
		SubType$ = theType$
	END IF

END FUNCTION

FUNCTION Trim$ (tString$)

	'----------------------------------------------------------------------
	' This function was created to remove spaces and tabs from either side
	' of a string. The RTRIM and LTRIM library functions do not trim tabs.
	'----------------------------------------------------------------------

	DIM char$

	char$ = LEFT$(tString$, 1)
	DO WHILE char$ = " " OR char$ = CHR_TAB
		tString$ = RIGHT$(tString$, LEN(tString$) - 1)
		char$ = LEFT$(tString$, 1)
	LOOP

	char$ = RIGHT$(tString$, 1)
	DO WHILE char$ = " " OR char$ = CHR_TAB
		tString$ = LEFT$(tString$, LEN(tString$) - 1)
		char$ = RIGHT$(tString$, 1)
	LOOP

	Trim$ = tString$

END FUNCTION

FUNCTION CharNum% (codeLine$, char$)

	'----------------------------------------------------------------------
	' Count the number of occurrences of char$ found in codeLine$
	'----------------------------------------------------------------------

	DIM x%
	DIM count%
	count% = 0

	FOR x% = 1 TO LEN(codeLine$)
		IF MID$(codeLine$, x%, 1) = char$ THEN count% = count% + 1
	NEXT x%

	CharNum% = count%

END FUNCTION

FUNCTION InStringArray% (sArray$(), sValue$)

	'----------------------------------------------------------------------
	' This function was created to search a string array for a certain
	' value. This avoids having too much array-searching code.
	'----------------------------------------------------------------------

	DIM a%
	FOR a% = 1 TO UBOUND(sArray$, 1)
		IF sValue$ = sArray$(a%) THEN
			InStringArray% = a%
			EXIT FUNCTION
		END IF
	NEXT a%

	InStringArray% = 0

END FUNCTION

