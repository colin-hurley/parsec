' Parsec 1.8.0 Source Code
' written in QuickBasic 7.1.
'------------------------------------------------------------------------------

'----------------------- Main Subs --------------------------------------------
DECLARE SUB SkipComments ()
DECLARE SUB CloseCog ()
DECLARE SUB OpenCog ()
DECLARE SUB ReadFileData ()
DECLARE SUB ParseFlags ()
DECLARE SUB ParseSymbols ()
DECLARE SUB ParseCode ()
DECLARE SUB Analize ()
DECLARE SUB ParseEnd ()

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

'----------------------- String Functions -------------------------------------
DECLARE SUB ChopString (str1$, str2$, Length%)
DECLARE FUNCTION Trim$ (tString$)

'----------------------- Misc Subs --------------------------------------------
DECLARE FUNCTION GetArrayPrefix$ (codeWord$)
DECLARE FUNCTION GetVarType$ (var$)
DECLARE FUNCTION SubType$ (type$)
DECLARE FUNCTION CharNum% (codeLine$, char$)
DECLARE FUNCTION InStringArray% (sArray$(), sValue$)

'----------------------- API Subs ---------------------------------------------
DECLARE SUB CreateAPI ()
DECLARE SUB MakeExit ()
DECLARE SUB PrintStatus (stat$)
DECLARE SUB PrintError (priority%, errorMsg$)
DECLARE SUB UpdateBar ()

'----------------------- Checking Subs ----------------------------------------
DECLARE SUB CheckSymType (type$)
DECLARE SUB CheckSymName (type$, name$)
DECLARE SUB CheckSymVal (type$, initVal$)
DECLARE SUB CheckSymExt (type$, exten$, extVal$)
DECLARE SUB CheckForExtConflict (exten$(), extCount%)
DECLARE SUB CheckExpType (expect$, type$, valDesc$)

'----------------------- Validation Functions ---------------------------------
DECLARE FUNCTION IsValidName% (parseString$)
DECLARE FUNCTION IsValidInt% (intString$)
DECLARE FUNCTION IsValidFlex% (flexString$)
DECLARE FUNCTION IsOperator% (char$)
DECLARE FUNCTION IsUnaryOp% (char$)
DECLARE FUNCTION IsValidVector% (vector$)

'----------------------- Misc Startup -----------------------------------------

CLEAR , , 8000
ON ERROR GOTO HandleError
CreateAPI
DIM SHARED time1!
time1! = TIMER

'----------------------- Declare Shared Variables -----------------------------

REM $DYNAMIC

'Arrays
DIM SHARED symType$(1 TO 30)
DIM SHARED symExt$(1 TO 10)
DIM SHARED message$(1 TO 100)
DIM SHARED verbName$(1 TO 451)
DIM SHARED verbAct%(1 TO 451)
DIM SHARED verbRet$(1 TO 451)
DIM SHARED verbParNum$(1 TO 451)
DIM SHARED verbParam$(1 TO 451, 1 TO 10)
DIM SHARED varName$(1 TO 200)
DIM SHARED varConst$(1 TO 200)
DIM SHARED varType$(1 TO 200)
DIM SHARED usedVar$(1 TO 200)
DIM SHARED assignVar$(1 TO 200)
DIM SHARED arrayVar$(1 TO 100)
DIM SHARED CallMessage$(1 TO 100)
DIM SHARED CodeMessage$(1 TO 100)
DIM SHARED errors$(1 TO 10)

'Array Pop Numbers
DIM SHARED varCount%, numCalls%, numCM%, numLines%, errNum%
DIM SHARED numUsedVars%, numAssignVars%, numArrayVars%

'Shared counters and vars
DIM SHARED lineNum%, statExec%, codeLine$

'Settings vars
DIM SHARED aisub$, cogsub$, flexsub$, floatsub$, intsub$, keyframesub$
DIM SHARED materialsub$, messagesub$, modelsub$, sectorsub$, surfacesub$
DIM SHARED soundsub$, templatesub$, thingsub$, vectorsub$, allowBadFlex%
DIM SHARED strictParse%, simArrayNames%

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
CALL PrintError(0, "Parse of " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + " Complete. Time elapsed:" + LEFT$(STR$(TIMER - time1!), 5) + " seconds.")
PrintStatus ("Cog Parse Complete.")
MakeExit

'------------------------------------------------------------------------------
END

HandleError:
	IF ERR = 52 THEN
		CALL PrintError(1, "Invalid file name or number.")
	ELSEIF ERR = 53 THEN
		CALL PrintError(1, "Cog file or data.dat not found.")
	ELSEIF ERR = 64 THEN
		CALL PrintError(1, "Invalid DOS file name.")
	ELSE
		CALL PrintError(1, "Error code" + STR$(ERR) + ". Not handled.")
	END IF
END

REM $STATIC
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
			IF InStringArray%(varName$(), mess$) = 0 OR GetVarType$(mess$) <> SubType$("message") THEN CALL PrintError(0, "The message, " + mess$ + ", is not defined in the symbols section.")
		ELSE
			IF InStringArray%(CallMessage$(), mess$) = 0 THEN CALL PrintError(0, "The custom message, " + mess$ + ", is not called anywhere in the code section.")
		END IF
	NEXT a%

	'--------------- Check called messages --------------------------------

	FOR a% = 1 TO numCalls%
		mess$ = CallMessage$(a%)
		IF InStringArray%(CodeMessage$(), mess$) = 0 THEN CALL PrintError(0, "The message, " + mess$ + ", does not exist, but it is called in the code section.")
	NEXT a%

	'--------------- Check Used Variables ---------------------------------
	
	FOR a% = 1 TO varCount%
		IF numArrayVars% = 0 THEN
			IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
				IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
					CALL PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
				ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
					CALL PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
				ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
					CALL PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
				END IF
			END IF
		ELSEIF simArrayNames% = 1 THEN
			IF InStringArray(arrayVar$(), varName$(a%)) > 0 THEN
				'found the first var in an array.
				arrayPrefix$ = GetArrayPrefix$(varName$(a%))
				IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
					IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
					END IF
				END IF
				
			ELSEIF LEN(arrayPrefix$) > 0 AND GetArrayPrefix$(varName$(a%)) = arrayPrefix$ THEN
				'trap - found an array variable.
			ELSE
				'found a non-array variable.
				arrayPrefix$ = ""       'don't want non-array vars to come in the middle of an array.
				IF InStringArray%(varConst$(), varName$(a%)) = 0 THEN
					IF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was not used in the code section.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) > 0 AND InStringArray(assignVar$(), varName$(a%)) = 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was used in the code section, but never assigned.")
					ELSEIF InStringArray(usedVar$(), varName$(a%)) = 0 AND InStringArray(assignVar$(), varName$(a%)) > 0 THEN
						CALL PrintError(0, "The symbol, " + varName$(a%) + ", was assigned, but never used.")
					END IF
				END IF
			END IF
		END IF
	NEXT a%

END SUB

FUNCTION CharNum% (codeLine$, char$)

	DIM x%
	DIM count%
	count% = 0

	FOR x% = 1 TO LEN(codeLine$)
		IF MID$(codeLine$, x%, 1) = char$ THEN count% = count% + 1
	NEXT x%

	CharNum% = count%

END FUNCTION

SUB CheckExpType (expect$, type$, valDesc$)

	'----------------------------------------------------------------------
	' This sub is called to make sure that the type that was found is
	' included in the list of expected types. We allow numerical types
	' to be interchangeable because this is often needed in cogscript.
	'----------------------------------------------------------------------

	DIM isErr%
	isErr% = 0

	IF INSTR(expect$, type$) < 1 THEN
		IF expect$ = "int" AND (INSTR(type$, "flex") > 0 OR INSTR(type$, "float") > 0) THEN
			'trap
		ELSEIF expect$ = "flex" AND (INSTR(type$, "int") > 0 OR INSTR(type$, "float") > 0) THEN
			'trap
		ELSEIF expect$ = "float" AND (INSTR(type$, "int") > 0 OR INSTR(type$, "flex") > 0) THEN
			'trap
		ELSE
			isErr% = 1
		END IF
	END IF

	IF isErr% = 1 THEN CALL PrintError(0, "Type mismatch. " + expect$ + " value expected, but " + type$ + " found in " + valDesc$ + " on line" + STR$(lineNum%) + ".")

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
		IF char$ <> " " AND char$ <> CHR$(9) THEN
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
		IF hasNolink% = 1 OR hasLinkid% = 1 OR hasMask% = 1 THEN CALL PrintError(0, "Symbol extension conflict on line" + STR$(lineNum%) + ".")
	ELSEIF hasNolink% = 1 THEN
		IF hasMask% = 1 OR hasLinkid% = 1 THEN CALL PrintError(0, "Symbol extension conflict on line" + STR$(lineNum%) + ".")
	END IF

	'In contrast to the neat code above, we have this code
	'below to check for duplicate symbol extensions:

	FOR x% = 1 TO extCount%
		FOR y% = 1 TO extCount%
			IF y% <> x% AND exten$(y%) = exten$(x%) AND exten$(x%) <> "null" THEN
				CALL PrintError(0, "Duplicate symbol extension, " + exten$(x%) + ", found on line" + STR$(lineNum%) + ".")
				exten$(y%) = "null"
			END IF
		NEXT y%
	NEXT x%

END SUB

SUB CheckSymExt (type$, exten$, extVal$)

	'----------------------------------------------------------------------
	' This function checks a symbol's extension to make sure that it's
	' valid for it's type.
	'----------------------------------------------------------------------

	'--------------- Make sure exten is known -----------------------------

	IF InStringArray%(symExt$(), exten$) = 0 THEN
		CALL PrintError(0, "Invalid symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + ".")
	ELSE
		'------- Make sure exten fits type ----------------------------
		IF type$ = "message" AND exten$ <> "desc" THEN
			CALL PrintError(0, "The extension, " + exten$ + ", used on line" + STR$(lineNum%) + " is not valid for a symbol of the " + type$ + " type.")
		ELSEIF type$ <> "sector" AND type$ <> "surface" AND type$ <> "thing" AND exten$ <> "local" AND exten$ <> "desc" THEN
			CALL PrintError(0, "The extension, " + exten$ + ", used on line" + STR$(lineNum%) + " is not valid for the " + type$ + " symbol type.")
		END IF
	END IF

	'--------------- Check exten's value ----------------------------------

	IF (exten$ = "local" OR exten$ = "nolink") AND extVal$ <> "null" THEN
		CALL PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " should not have a value.")

	ELSEIF exten$ = "desc" THEN
		IF extVal$ = "null" THEN CALL PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " requires a value.")
		'The desc value may include delimiters and odd characters, so we can't really check it.
		'IF IsValidName%(extVal$) = 0 THEN CALL PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " has an invalid value.")

	ELSEIF exten$ = "mask" OR exten$ = "linkid" THEN
		IF extVal$ = "null" THEN CALL PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " requires a value.")
		IF IsValidInt%(extVal$) = 0 THEN CALL PrintError(0, "Symbol extension, " + exten$ + ", on line" + STR$(lineNum%) + " has an invalid value.")
	END IF

END SUB

SUB CheckSymName (type$, name$)

	'----------------------------------------------------------------------
	' This function checks the symbol name given to it to make sure that it
	' can be used.
	'----------------------------------------------------------------------

	DIM char%
	
	'--------------- Check First Char -------------------------------------

	char% = ASC(MID$(name$, 1, 1))
	IF char% < 95 OR char% > 122 THEN CALL PrintError(0, "The symbol, " + name$ + ", begins with a numerical character.")

	IF type$ = "message" THEN
		IF InStringArray%(message$(), name$) = 0 THEN CALL PrintError(0, "Unknown message, " + name$ + ", declared on line" + STR$(lineNum%) + ".")
	ELSE
		IF IsValidName%(name$) = 0 THEN
			CALL PrintError(0, "Symbol name, " + name$ + ", on line" + STR$(lineNum%) + " is invalid.")
		ELSEIF InStringArray%(message$(), name$) > 0 THEN
			CALL PrintError(0, "Warning: symbol name, " + name$ + ", on line" + STR$(lineNum%) + " is a message name and using it will void the message.")
		ELSEIF InStringArray%(verbName$(), name$) > 0 THEN
			CALL PrintError(0, "Warning: symbol name, " + name$ + ", on line" + STR$(lineNum%) + " is a verb name and using it will void the verb.")
		END IF
	END IF

	'--------------- Check for Duplicate Name -----------------------------

	IF InStringArray%(varName$(), name$) > 0 THEN CALL PrintError(0, "The symbol name, " + name$ + ", on line" + STR$(lineNum%) + " has already been used.")

END SUB

SUB CheckSymType (type$)

	'----------------------------------------------------------------------
	' This function checks the given symbol type to make sure that it is a
	' known type.
	'----------------------------------------------------------------------

	IF InStringArray%(symType$(), type$) = 0 THEN CALL PrintError(0, "Invalid symbol type, " + type$ + ", found on line" + STR$(lineNum%) + ".")

END SUB

SUB CheckSymVal (type$, initVal$)

	'----------------------------------------------------------------------
	' This function checks a symbol's initial value to make sure that it's
	' valid for its type.
	'----------------------------------------------------------------------

	DIM x%
	DIM ext$
	DIM fileName$

	IF initVal$ = "null" THEN EXIT SUB

	IF type$ = "ai" OR type$ = "keyframe" OR type$ = "material" OR type$ = "model" OR type$ = "sound" THEN
		x% = 1
		fileName$ = CutAtNextDelimit$(initVal$, x%, ".")
		ext$ = MID$(initVal$, x% + 1, LEN(initVal$) - x% + 1)

		IF type$ = "ai" AND ext$ <> "ai" AND ext$ <> "ai0" AND ext$ <> "ai2" THEN
			beenError% = 1
		ELSEIF type$ = "keyframe" AND ext$ <> "key" THEN
			beenError% = 1
		ELSEIF type$ = "material" AND ext$ <> "mat" THEN
			beenError% = 1
		ELSEIF type$ = "model" AND ext$ <> "3do" THEN
			beenError% = 1
		ELSEIF type$ = "sound" AND ext$ <> "wav" THEN
			beenError% = 1
		END IF

		IF beenError% = 1 THEN CALL PrintError(0, "Invalid file extension, " + ext$ + ", found for the initial value of the symbol on line" + STR$(lineNum%) + ".")
		IF IsValidName%(fileName$) = 0 THEN CALL PrintError(0, "A symbol on line" + STR$(lineNum%) + " is assigned to a file name that may be invalid.")

	ELSEIF type$ = "cog" OR type$ = "vector" OR type$ = "message" THEN
		CALL PrintError(0, "The symbol of type, " + type$ + ", used on line" + STR$(lineNum%) + " cannot have an initial value.")

	ELSEIF type$ = "flex" OR type$ = "float" THEN
		IF IsValidFlex%(initVal$) = 0 THEN CALL PrintError(0, "Invalid initial value, " + initVal$ + ", found for the symbol on line" + STR$(lineNum%) + ".")

	ELSEIF type$ = "thing" OR type$ = "sector" OR type$ = "surface" OR type$ = "int" THEN
		IF IsValidInt%(initVal$) = 0 THEN CALL PrintError(0, "Invalid initial value, " + initVal$ + ", for the symbol on line" + STR$(lineNum%) + ".")

	ELSEIF type$ = "template" THEN
		FOR x% = 1 TO LEN(initVal$)
			charint% = ASC(MID$(initVal$, x%, 1))
			'if charint is not a 0-9 number, a lowercase letter, an underscore, or a plus, then there's an error.
			IF (charint% < 48 OR charint% > 57) AND (charint% < 97 OR charint% > 122) AND charint% <> 43 AND charint% <> 95 THEN
				CALL PrintError(0, "Invalid template name, " + initVal$ + ", found for the symbol on line" + STR$(lineNum%) + ".")
			END IF
		NEXT x%
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
		COLOR 0, 7
		CLS
		PRINT "Internal Error: ChopString could not break up a solid string."
	END IF

	str2$ = MID$(str1$, x%, LEN(str1$) - x% + 1)
	str1$ = MID$(str1$, 1, LEN(str1$) - LEN(str2$))
	str2$ = "  " + str2$

END SUB

SUB CloseCog

	CLOSE #1

END SUB

SUB CreateAPI

	'----------------------------------------------------------------------
	' This sub creates the text interface for Parsec. After the API is
	' created, UpdateBar, PrintError, and PrintStatus are used to change
	' the information displayed through the interface.
	'----------------------------------------------------------------------

	DIM a%
	CLS

	'--------------- Print Title Bar --------------------------------------

	COLOR 0, 7
	PRINT SPACE$(33); "Parsec v1.8.0"; SPACE$(34);

	'--------------- Print main window ------------------------------------

	COLOR 7, 1
	PRINT CHR$(201); STRING$(78, 205); CHR$(187)
	FOR a% = 1 TO 19
		PRINT CHR$(186); SPACE$(78); CHR$(186)
	NEXT a%
	PRINT CHR$(200); STRING$(78, 205); CHR$(188)

	'--------------- Print Status Bar -------------------------------------

	COLOR 0, 3
	PRINT STRING$(80, 32)

	'--------------- Create Bar Graph Shadow ------------------------------

	COLOR 0, 8
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

	COLOR 0, 7
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

FUNCTION CutAtNextDelimit$ (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This is a main parsing function designed to "cut" a codeWord out out
	' of a string. First it gets to the next non-space character. Then, it
	' gets to the next character in the string that's in the del$ string.
	' The characters between these two points will be returned as the
	' codeWord.
	'----------------------------------------------------------------------

	DIM lastBreak%
	DIM char$

	CALL GetToNextChar(codeLine$, x%)
	lastBreak% = x%

	FOR x% = x% TO LEN(codeLine$)
		char$ = MID$(codeLine$, x%, 1)
		IF MID$(codeLine$, x%, 2) = "//" THEN
			IF CharNum%(del$, "/") > 1 THEN EXIT FOR
		ELSEIF char$ = "/" THEN
			IF CharNum%(del$, "/") = 1 OR CharNum%(del$, "/") > 2 THEN EXIT FOR
		ELSEIF INSTR(del$, char$) > 0 THEN
			EXIT FOR
		ELSEIF INSTR(del$, " ") > 0 AND char$ = CHR$(9) THEN
			EXIT FOR
		END IF
	NEXT x%

	CutAtNextDelimit$ = Trim$(MID$(codeLine$, lastBreak%, x% - lastBreak%))

END FUNCTION

SUB DoAssignment (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This sub parses through a variable assignment. It is called by
	' DoStatement. (no longer called by DoFor explicitly)
	'----------------------------------------------------------------------

	DIM var$

	var$ = CutAtNextDelimit$(codeLine$, x%, " ;()=[#//")
	IF MID$(codeLine$, x%, 1) = "[" THEN
		x% = x% + 1
		CALL DoSubExp(codeLine$, x%, "]", "int", "the array access")     'process the expression.
		numArrayVars% = numArrayVars% + 1
		arrayVar$(numArrayVars%) = var$
	END IF

	IF InStringArray%(varName$(), var$) = 0 THEN CALL PrintError(0, "The variable, " + var$ + ", in the assignment on line" + STR$(lineNum%) + " is undefined.")
	IF GetVarType(var$) = "message" THEN
		CALL PrintError(0, "Warning: the symbol constant, " + var$ + ", is assigned on line" + STR$(lineNum%) + ", but its value should not be changed.")
	END IF

	IF InStringArray%(assignVar$(), var$) = 0 THEN
		numAssignVars% = numAssignVars% + 1
		assignVar$(numAssignVars%) = var$
	END IF
	
	CALL FindNextChar(codeLine$, x%, 0)
	IF MID$(codeLine$, x%, 1) <> "=" THEN CALL PrintError(1, "An = expected after the variable, " + var$ + ", on line" + STR$(lineNum%) + ".")
	x% = x% + 1

	CALL DoSubExp(codeLine$, x%, del$, GetVarType$(var$), "the expression after the " + var$ + " assignment")              'note that the calling sub chooses the delimit that the assignment should end with.

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
			CALL DoStatement(codeLine$, x%, blockEnd%, MsgReturn%, 1)
			IF blockEnd% = 0 THEN hasCode% = 1
			IF lastMR% = 1 AND blockEnd% = 0 THEN CALL PrintError(0, "Extra code found after a direct return in the code block on line" + STR$(lineNum%) + ".")
			lastMR% = MsgReturn%
		LOOP
		IF blockEnd% = 1 THEN EXIT DO

		LINE INPUT #1, codeLine$
		codeLine$ = LCASE$(codeLine$)
		lineNum% = lineNum% + 1
		x% = 1
	LOOP

	IF hasCode% = 0 THEN CALL PrintError(0, "No code found in the code block beginning on line" + STR$(startLine%) + ".")
	IF blockEnd% = 0 THEN CALL PrintError(1, "No end to the code block begun on line" + STR$(startLine%) + ".")

END SUB

SUB DoCall (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoCall processes call statements. It is called after the "call"
	' keyword has been identified, so it only has to verify and log the
	' message.
	'----------------------------------------------------------------------

	DIM message$

	CALL FindNextChar(codeLine$, x%, 0)       'message may be on the next line(s)
	message$ = CutAtNextDelimit$(codeLine$, x%, " ;#//")

	IF IsValidName%(message$) = 0 THEN CALL PrintError(0, "Invalid message name, " + message$ + ", found after the call statement on line" + STR$(lineNum%) + ".")

	numCalls% = numCalls% + 1
	CallMessage$(numCalls%) = message$

END SUB

SUB DoDo (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub is called to process Do..while loops.
	'----------------------------------------------------------------------

	DIM key$
	
	'--------------- Check the statement or block after the "do" ----------

	CALL FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		CALL DoBlock(codeLine$, x%)
	ELSE
		CALL DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN CALL PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'--------------- Check for the while statement ------------------------

	CALL FindNextChar(codeLine$, x%, 1)
	key$ = CutAtNextDelimit$(codeLine$, x%, " (#//")

	IF key$ = "while" THEN
		CALL FindNextChar(codeLine$, x%, 0)
		IF MID$(codeLine$, x%, 1) <> "(" THEN CALL PrintError(1, "No expression found after the while keyword on line" + STR$(lineNum%) + ".")
		x% = x% + 1      'pass the "("
		CALL DoSubExp(codeLine$, x%, ")", "int", "the while statement")
		CALL DoSemi(codeLine$, x%)
	ELSE
		CALL PrintError(1, "No while keyword found for the do..while loop on line" + STR$(lineNum%) + ".")
	END IF

END SUB

SUB DoFor (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub is called to process For loops.
	'----------------------------------------------------------------------

	DIM key$
	DIM char$
	
	'--------------- Check the variable init ------------------------------

	CALL FindNextChar(codeLine$, x%, 0)      'get to the paren
	x% = x% + 1                              'get past open paren
	CALL FindNextChar(codeLine$, x%, 0)     'semicolon may be on next line(s)
	IF NextCharInLine$(codeLine$, x%) <> ";" THEN
		DO
			CALL FindNextChar(codeLine$, x%, 0)
			IF NextCharInLine(codeLine$, x%) = "" THEN
				CALL PrintError(1, "Unable to parse for loop sub-statement on line" + STR$(lineNum%) + ". Unexpected line break.")
			END IF
			CALL DoForStatement(codeLine$, x%)
			CALL FindNextChar(codeLine$, x%, 0)
		LOOP WHILE NextCharInLine(codeLine$, x%) <> ";"
	END IF

	x% = x% + 1                             'get past it

	'--------------- Check the condition ----------------------------------

	CALL FindNextChar(codeLine$, x%, 0)
	IF NextCharInLine$(codeLine$, x%) <> ";" THEN
		CALL DoSubExp(codeLine$, x%, ";", "int", "the for statement's condition")
		CALL DoSemi(codeLine$, x%)
	ELSE
		x% = x% + 1
	END IF

	'--------------- Parse the incrementation -----------------------------

	DO
		CALL FindNextChar(codeLine$, x%, 0)
		IF NextCharInLine(codeLine$, x%) = "" THEN
			CALL PrintError(1, "Unable to parse for loop sub-statement on line" + STR$(lineNum%) + ". Unexpected line break.")
		END IF
		CALL DoForStatement(codeLine$, x%)
		CALL FindNextChar(codeLine$, x%, 0)

	LOOP WHILE NextCharInLine$(codeLine$, x%) <> ")"

	x% = x% + 1     'pass the ')'

	'--------------- Check the statement or block after the expression ----

	CALL FindNextChar(codeLine$, x%, 1)
	char$ = MID$(codeLine$, x%, 1)

	IF char$ = ";" THEN
		CALL PrintError(0, "Warning: The for statement on line" + STR$(lineNum%) + " is ended with a semicolon.")
		x% = x% + 1     'get past the semicolon.
	ELSEIF char$ = "{" THEN
		CALL DoBlock(codeLine$, x%)
	ELSE
		CALL DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN CALL PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
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
		IF verbNum% > 0 THEN IF verbAct%(verbNum%) = 0 THEN CALL PrintError(0, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " as a seperate statement, but it does not perform an action.")
		
	ELSEIF IsValidName%(codeWord$) = 1 AND (strictParse% = 0 OR (strictParse% = 1 AND (nextChar$ = "=" OR nextChar$ = "["))) THEN
		x% = lastBreak%
		CALL DoAssignment(codeLine$, x%, ",;)")
		IF NextCharInLine(codeLine$, x% - 1) = ")" THEN
			'DoSubExp will skip the ")" delimiter, so we'll go back one.
			x% = x% - 1
		ELSEIF NextCharInLine(codeLine$, x% - 1) = "," THEN
			CALL FindNextChar(codeLine$, x%, 0)
			IF NextCharInLine$(codeLine$, x%) = ")" OR NextCharInLine$(codeLine$, x%) = ";" THEN
				CALL PrintError(1, "For sub-statement expected, but " + NextCharInLine(codeLine$, x%) + " found on line" + STR$(lineNum%) + ".")
			END IF
		END IF
		
	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "For sub-statement expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			CALL PrintError(1, "For sub-statement expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "For sub-statement expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
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

	IF (verbNum% = 0) OR (LEN(funcName$) < 1) THEN CALL PrintError(1, "Unknown verb, " + funcName$ + ", found on line" + STR$(lineNum%) + ".")
	paramNum% = VAL(verbParNum$(verbNum%))
	
	'--------------- Make sure this is not a bad verb ---------------------

	IF verbRet$(verbNum%) = "!" THEN CALL PrintError(0, "The verb, " + funcName$ + ", found on line" + STR$(lineNum%) + " does not work properly.")
	
	'--------------- Parse function's arguments ---------------------------

	CALL FindNextChar(codeLine$, x%, 0) 'find begin paren
	x% = x% + 1             'get past begin paren.
	CALL FindNextChar(codeLine$, x%, 0)

	IF NextCharInLine$(codeLine$, x%) = ")" THEN
		argNum% = 0
		IF argNum% <> paramNum% THEN CALL PrintError(0, "Expected parameters for the verb, " + funcName$ + ", on line" + STR$(lineNum%) + ".")
		x% = x% + 1
	ELSE
		FOR argNum% = 1 TO paramNum%
			IF argNum% = paramNum% THEN
				CALL DoSubExp(codeLine$, x%, ")", verbParam$(verbNum%, argNum%), funcName$)
			ELSE
				CALL DoSubExp(codeLine$, x%, ",", verbParam$(verbNum%, argNum%), funcName$)
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

	CALL FindNextChar(codeLine$, x%, 0)
	x% = x% + 1     'get past the "("
	CALL DoSubExp(codeLine$, x%, ")", "int", "the if statement")
	
	'--------------- Check the statement or block after the expressionm ---

	CALL FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		CALL DoBlock(codeLine$, x%)
	ELSE
		CALL DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN CALL PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'--------------- Check for an else statement --------------------------

	CALL FindNextChar(codeLine$, x%, 1)
	savex% = x%
	key$ = CutAtNextDelimit$(codeLine$, x%, " #{//")
	
	IF key$ = "else" THEN
		CALL FindNextChar(codeLine$, x%, 1)
		IF MID$(codeLine$, x%, 1) = "{" THEN
			CALL DoBlock(codeLine$, x%)
		ELSE
			CALL DoStatement(codeLine$, x%, 0, 0, 0)
			IF statExec% = 0 THEN CALL PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
		END IF
	ELSE
		 'Since we did not find the else, we must set x% back to just before the key$.
		 x% = savex%
	END IF

END SUB

SUB DoLabel (codeWord$, x%)

	IF IsValidName%(codeWord$) = 0 THEN CALL PrintError(0, "Invalid message, " + codeWord$ + ", found on line" + STR$(lineNum%) + ".")
	IF InStringArray%(CodeMessage$(), codeWord$) > 0 THEN CALL PrintError(0, "The message, " + codeWord$ + ", on line" + STR$(lineNum%) + " has already been used in the code section.")
	IF InStringArray%(message$(), codeWord$) > 0 THEN CALL PrintError(0, "Warning: No return before the message, " + codeWord$ + ", on line" + STR$(lineNum%) + ".")

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
			CALL DoStatement(codeLine$, x%, 0, MsgReturn%, 0)
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

	CALL FindNextChar(codeLine$, x%, 0)       'get to the semicolon
	IF MID$(codeLine$, x%, 1) <> ";" THEN CALL PrintError(1, "No semicolon after the statement on line" + STR$(lineNum%) + ".")
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
		CALL DoIf(codeLine$, x%)
	ELSEIF codeWord$ = "call" THEN
		CALL DoCall(codeLine$, x%)
		CALL DoSemi(codeLine$, x%)
	ELSEIF codeWord$ = "do" THEN
		CALL DoDo(codeLine$, x%)
	ELSEIF codeWord$ = "while" AND (strictParse% = 0 OR (strictParse% = 1 AND nextChar$ = "(")) THEN
		CALL DoWhile(codeLine$, x%)
	ELSEIF codeWord$ = "for" AND (strictParse% = 0 OR (strictParse% = 1 AND nextChar$ = "(")) THEN
		CALL DoFor(codeLine$, x%)
	ELSEIF codeWord$ = "return" OR codeWord$ = "stop" THEN
		CALL DoSemi(codeLine$, x%)
		MsgReturn% = 1
	ELSEIF nextChar$ = "{" AND LEN(codeWord$) = 0 THEN
		CALL PrintError(0, "Beginning of useless code block found on line" + STR$(lineNum%) + ".")
		CALL DoBlock(codeLine$, x%)    'this code block is technically useless, though it is syntactically correct.
	ELSEIF nextChar$ = "}" AND LEN(codeWord$) = 0 THEN
		IF which% = 1 THEN
			x% = x% + 1     'get past the bracket
			blockEnd% = 1
		ELSE
			CALL PrintError(1, "Extra end bracket found on line" + STR$(lineNum%) + ".")
		END IF
	ELSEIF (strictParse% = 1 AND nextChar$ = "(") OR (strictParse% = 0 AND LEN(codeWord$) > 1 AND InStringArray(verbName$(), codeWord$)) THEN
		x% = lastBreak%
		verbNum% = DoFunction%(codeLine$, x%)
		IF verbNum% > 0 THEN IF verbAct%(verbNum%) = 0 THEN CALL PrintError(0, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " as a seperate statement, but it does not perform an action.")
		CALL DoSemi(codeLine$, x%)
		
	ELSEIF nextChar$ = ":" THEN
		CALL DoLabel(codeWord$, x%)
		statExec% = 0

	ELSEIF IsValidName%(codeWord$) = 1 AND (strictParse% = 0 OR (strictParse% = 1 AND (nextChar$ = "=" OR nextChar$ = "["))) THEN
		x% = lastBreak%
		CALL DoAssignment(codeLine$, x%, ",;)")
		CALL DoSemi(codeLine$, x%)

	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "Statement expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			CALL PrintError(1, "Statement expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "Statement expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		END IF
	END IF

END SUB

SUB DoString (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoString is called by DoValue when it encounters a double quote.
	'----------------------------------------------------------------------

	'+1 to get past opening quotes
	FOR x% = x% + 1 TO LEN(codeLine$)
		IF ASC(MID$(codeLine$, x%, 1)) = 34 THEN EXIT FOR
	NEXT x%

	IF x% > LEN(codeLine$) THEN CALL PrintError(1, "No end to the string value on line" + STR$(lineNum%) + ".")
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
		CALL FindNextChar(codeLine$, x%, 0)
		char$ = MID$(codeLine$, x%, 1)
		altChar$ = MID$(codeLine$, x%, 2)

		IF IsUnaryOp%(char$) = 1 AND expect$ = "value" THEN
			IF LEN(expType$) > 1 THEN CALL CheckExpType(expType$, "int flex float", valDesc$)
			x% = x% + 1

		ELSEIF expect$ = "value" AND char$ = "(" THEN
			x% = x% + 1
			CALL DoSubExp(codeLine$, x%, ")", expType$, "a sub-expression")
			expect$ = "op/end"

		ELSEIF expect$ = "value" THEN
			type$ = DoValue$(codeLine$, x%, expType$)
			'CALL PrintError(0, "Warning:" + type$)
			IF LEN(expType$) > 1 THEN CALL CheckExpType(expType$, type$, valDesc$)
			expect$ = "op/end"

		ELSEIF IsOperator%(altChar$) = 1 AND expect$ = "op/end" THEN    'note that we check for two-char ops first.
			expect$ = "value"
			IF LEN(expType$) > 1 THEN CALL CheckExpType(expType$, "int, flex, or float operator", valDesc$)    'make sure the last value's type can be used with operators.
			x% = x% + 2     'get past second char of operator.

		ELSEIF IsOperator%(char$) = 1 AND expect$ = "op/end" THEN
			expect$ = "value"
			IF LEN(expType$) > 1 THEN CALL CheckExpType(expType$, "int, flex, or float operator", valDesc$)
			x% = x% + 1

		ELSEIF INSTR(del$, char$) > 0 AND LEN(char$) > 0 THEN
			'in most cases, the semicolon is checked by seperate subs, so we won't pass it here.
			IF char$ <> ";" THEN x% = x% + 1   'get past the delimiter.
			EXIT DO
		ELSE
			IF LEN(char$) < 1 THEN CALL PrintError(1, "Unexpected end of line in " + valDesc$ + " on line" + STR$(lineNum%) + ".")
			CALL PrintError(1, "Unexpected " + char$ + " in " + valDesc$ + " on line" + STR$(lineNum%) + ".")
		END IF
	LOOP

	IF expect$ = "value" THEN CALL PrintError(1, "Value expected in " + valDesc$ + " on line" + STR$(lineNum%) + ", but none found.")

END SUB

FUNCTION DoValue$ (codeLine$, x%, expType$)

	'----------------------------------------------------------------------
	' DoValue will be called when DoSubExp needs to process a value. The
	' value can be a function, number, variable, string, vector, etc.
	'----------------------------------------------------------------------

	DIM type$
	DIM lastBreak%
	DIM verbNum%

	lastBreak% = x%
	codeWord$ = CutAtNextDelimit$(codeLine$, x%, " ;()[],==!=>=<=&&||%*/-+'#//" + CHR$(34))
	nextChar$ = NextCharInLine$(codeLine$, x%)

	IF nextChar$ = CHR$(34) THEN    'Note that we check for a string before we check the length of codeWord$
		CALL DoString(codeLine$, x%)   'Strings can contain other delimiters so we can't use an IsValid function.
		type$ = "string"

	ELSEIF nextChar$ = "'" THEN     'Same as above.
		CALL DoVector(codeLine$, x%)
		type$ = "vector"

	ELSEIF LEN(codeWord$) < 1 THEN
		CALL PrintError(1, "Value expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")

	ELSEIF nextChar$ = "(" THEN
		x% = lastBreak%
		verbNum% = DoFunction%(codeLine$, x%)
		type$ = verbRet$(verbNum%)
		IF type$ = "void" THEN CALL PrintError(1, "The verb, " + verbName$(verbNum%) + ", is used on line" + STR$(lineNum%) + " in an expression.")

	ELSEIF IsValidInt%(codeWord$) = 1 THEN
		type$ = "int"

	ELSEIF IsValidFlex%(codeWord$) = 1 THEN
		type$ = "flex"

	ELSEIF InStringArray%(message$(), codeWord$) > 0 AND InStringArray%(varName$(), codeWord$) = 0 THEN
		type$ = "message"

	ELSEIF IsValidName%(codeWord$) = 1 THEN
		IF nextChar$ = "[" THEN
			x% = x% + 1                         'get past the "["
			CALL DoSubExp(codeLine$, x%, "]", "int", "the array access")  'evaluate the expression
			numArrayVars% = numArrayVars% + 1
			arrayVar$(numArrayVars%) = codeWord$
		END IF

		IF InStringArray%(usedVar$(), codeWord$) = 0 THEN
			numUsedVars% = numUsedVars% + 1
			usedVar$(numUsedVars%) = codeWord$
		END IF

		IF InStringArray%(varName$(), codeWord$) = 0 THEN CALL PrintError(0, "The variable, " + codeWord$ + ", found in the expression on line" + STR$(lineNum%) + " is undefined.")
		type$ = GetVarType$(codeWord$)
	ELSE
		IF LEN(codeWord$) > 0 AND LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "Value expected, but " + codeWord$ + " and " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(codeWord$) > 0 THEN
			CALL PrintError(1, "Value expected, but " + codeWord$ + " found on line" + STR$(lineNum%) + ".")
		ELSEIF LEN(nextChar$) > 0 THEN
			CALL PrintError(1, "Value expected, but " + nextChar$ + " found on line" + STR$(lineNum%) + ".")
		END IF
	END IF

	DoValue$ = SubType(type$)

END FUNCTION

SUB DoVector (codeLine$, x%)

	DIM lastBreak%
	lastBreak% = x%

	'+1 to get past opening quote
	FOR x% = x% + 1 TO LEN(codeLine$)
		IF MID$(codeLine$, x%, 1) = "'" THEN EXIT FOR
	NEXT x%

	codeWord$ = MID$(codeLine$, lastBreak%, x% - lastBreak% + 1)
	IF IsValidVector%(codeWord$) = 0 THEN CALL PrintError(1, "Invalid vector, " + codeWord$ + ", found in an expression on line" + STR$(lineNum%) + ".")
	x% = x% + 1     'get past end quote

END SUB

SUB DoWhile (codeLine$, x%)

	'----------------------------------------------------------------------
	' DoWhile is called by DoStatement to process while loops.
	'----------------------------------------------------------------------

	'--------------- Check the Expression ---------------------------------

	CALL FindNextChar(codeLine$, x%, 0)
	x% = x% + 1     'get past the "("
	CALL DoSubExp(codeLine$, x%, ")", "int", "the while statement")
	
	'--------------- Check the statement or block after the expression ----

	CALL FindNextChar(codeLine$, x%, 1)
	IF MID$(codeLine$, x%, 1) = "{" THEN
		CALL DoBlock(codeLine$, x%)
	ELSE
		CALL DoStatement(codeLine$, x%, 0, 0, 0)
		IF statExec% = 0 THEN CALL PrintError(0, "Non-executable statement found after a conditional statement on line" + STR$(lineNum%) + ".")
	END IF

	'----------------------------------------------------------------------

END SUB

SUB FindNextChar (codeLine$, x%, multiLine%)

	'----------------------------------------------------------------------
	' FindNextChar is called when the next a sub needs the position of the
	' next character in the code section. This sub will go through multiple
	' lines looking for the next char unlike other one-line-only parsing
	' subs.
	'----------------------------------------------------------------------

	IF strictParse% = 1 AND multiLine% = 0 THEN
		CALL GetToNextChar(codeLine$, x%)
		EXIT SUB
	END IF

	DO WHILE NOT EOF(1)
		IF CheckForBlankLine%(codeLine$, x%) = 0 THEN
			CALL GetToNextChar(codeLine$, x%)
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

SUB GetToNextChar (codeLine$, x%)

	'----------------------------------------------------------------------
	' This sub parses through a string starting at a given pos. It returns
	' the pos of the next non-space character.
	'----------------------------------------------------------------------

	DIM char$
	FOR x% = x% TO LEN(codeLine$)
		char$ = MID$(codeLine$, x%, 1)
		IF char$ <> " " AND char$ <> CHR$(9) THEN EXIT FOR
	NEXT x%

END SUB

SUB GetToNextDelimit (codeLine$, x%, del$)

	'----------------------------------------------------------------------
	' This sub parses through a string looking for the next character that
	' the calling function identified as a delimiter.
	'----------------------------------------------------------------------

	DIM char$
	FOR x% = x% TO LEN(codeLine$)
		char$ = MID$(codeLine$, x%, 1)
		IF INSTR(del$, char$) > 0 THEN EXIT FOR
		IF INSTR(del$, " ") > 0 AND char$ = CHR$(9) THEN EXIT FOR
	NEXT x%

END SUB

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

FUNCTION IsValidName% (name$)

	'----------------------------------------------------------------------
	' The name of this function is vague, but it will check a string to
	' make sure it contains only characters allowed for variable names.
	'----------------------------------------------------------------------

	DIM x%
	DIM valid%
	DIM charint%
	DIM hasAlpha%
	valid% = 1

	FOR x% = 1 TO LEN(name$)
		charint% = ASC(MID$(name$, x%, 1))
		' if charint is not 0-9 or a lowercase letter, or an underscore, or a dash, then there's an error.
		IF (charint% < 48 OR charint% > 57) AND (charint% < 97 OR charint% > 122) AND charint% <> 95 AND charint% <> 45 THEN
			IsValidName% = 0
			EXIT FUNCTION
		END IF
		IF charint% >= 97 AND charint% <= 122 THEN hasAlpha% = 1
	NEXT x%

	IF hasAlpha% = 0 THEN valid% = 0

	'Quick hack because Parsec does not have a list of reserved words...
	IF (name$ = "end" OR name$ = "code" OR name$ = "if" OR name$ = "do" OR name$ = "while") THEN
		valid% = 0
	ELSEIF (name$ = "for" OR name$ = "else" OR name$ = "call" OR name$ = "return" OR name$ = "stop") THEN
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

SUB MakeExit

	'----------------------------------------------------------------------
	' MakeExit prints an "OK" box at the bottom of the API and waits for a
	' key to be pressed before ending the program.
	'----------------------------------------------------------------------

	COLOR 0, 8
	LOCATE 20, 37
	PRINT SPACE$(8);
	LOCATE 21, 37
	PRINT SPACE$(8);
	LOCATE 22, 37
	COLOR 8, 8
	PRINT STRING$(8, 205)

	COLOR 0, 7
	LOCATE 19, 36
	PRINT CHR$(201); STRING$(6, 205); CHR$(187)
	LOCATE 20, 36
	PRINT CHR$(186); SPACE$(2); "OK"; SPACE$(2); CHR$(186)
	LOCATE 21, 36
	PRINT CHR$(200); STRING$(6, 205); CHR$(188)

	DO WHILE INKEY$ = ""
	LOOP

	COLOR 7, 0
	CLS
	END

END SUB

FUNCTION NextCharInLine$ (codeLine$, x%)

	CALL GetToNextChar(codeLine$, x%)
	NextCharInLine$ = MID$(codeLine$, x%, 1)

END FUNCTION

SUB OpenCog

	OPEN COMMAND$ FOR INPUT AS #1
	DO WHILE NOT EOF(1)
		LINE INPUT #1, codeLine$
		numLines% = numLines% + 1
	LOOP
	CLOSE #1

	OPEN COMMAND$ FOR INPUT AS #1

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
	IF codeWord$ <> "code" THEN CALL PrintError(1, "Code section beginning expected on line" + STR$(lineNum%) + ".")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN CALL PrintError(0, "Garbage found after beginning of code section on line" + STR$(lineNum%) + ".")

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
				IF InStringArray%(CodeMessage$(), message$) > 0 THEN CALL PrintError(0, "The message, " + message$ + ", on line" + STR$(lineNum%) + " has already been used in the code section.")
				PrintStatus ("Parsing message, " + message$ + ".")
				numCM% = numCM% + 1
				CodeMessage$(numCM%) = message$
				numUsedVars% = numUsedVars% + 1
				usedVar$(numUsedVars%) = message$
				x% = x% + 1     'get past the ":"
				CALL DoMessage(codeLine$, x%, message$)
			ELSE
				x% = lastBreak%
				CALL PrintError(1, "Message expected, but " + CutAtNextDelimit$(message$, 1, " :#//") + " found on line" + STR$(lineNum%) + ".")
			END IF
			
		LOOP
	LOOP

	'Note below that Parsec does not allow double-slash comments directly
	'after the 'end'. Since JK doesn't read beyond the end of the code,
	'it isn't aware of the comment. It's overkill, but it enforces the
	'rule: no double-slash comments outside of the code or symbols sections.
	
	x% = 1
	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "end" THEN CALL PrintError(1, "No end to the code section.")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN CALL PrintError(0, "Garbage found after end of code section on line" + STR$(lineNum%) + ".")

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
			CALL PrintError(0, "Garbage found on line" + STR$(lineNum%) + ". Double-slash comments are not allowed.")
		ELSE
			CALL PrintError(0, "Garbage found on line" + STR$(lineNum%) + ".")
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

	CALL GetToNextChar(codeLine$, x%)       'get to the "f" in flags.
	x% = x% + 5      'get past the "flags"

	lastBreak% = x%
	CALL GetToNextChar(codeLine$, x%)
	IF x% > lastBreak% THEN CALL PrintError(0, "An = was expected after the flags assignment on line" + STR$(lineNum%) + ". Spaces are not allowed.")
	IF NextCharInLine(codeLine$, x%) <> "=" THEN CALL PrintError(1, "An = was expected after the flags assignment on line" + STR$(lineNum%) + ".")

	x% = x% + 1      'get past the "="
	IF x% > LEN(codeLine$) THEN CALL PrintError(1, "Integer expected after the flags assignment on line" + STR$(lineNum%) + ".")

	lastBreak% = x%
	CALL GetToNextChar(codeLine$, x%)
	IF x% > lastBreak% THEN CALL PrintError(0, "Integer expected after the flags assignment on line" + STR$(lineNum%) + ". Spaces are not allowed.")

	flagVal$ = CutAtNextDelimit(codeLine$, x%, " #//")
	IF IsValidInt%(flagVal$) = 0 THEN CALL PrintError(0, "The flags assignment on line" + STR$(lineNum%) + " has an invalid integer value.")
	IF CutAtNextDelimit(codeLine$, x%, "#//") <> "" THEN CALL PrintError(0, "Garbage found after the flags assignment on line" + STR$(lineNum%))
	
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
	IF codeWord$ <> "symbols" THEN CALL PrintError(1, "Symbols beginning expected on line" + STR$(lineNum%) + ".")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN CALL PrintError(0, "Garbage found after beginning of symbols section on line" + STR$(lineNum%) + ".")

	PrintStatus ("Parsing Defined Variables.")

	DIM a%
	DIM type$
	DIM realType$
	DIM name$
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
			type$ = "null"
			name$ = "null"
			initVal$ = "null"
			extCount% = 0
			EOL% = 0
			FOR a% = 1 TO 5
				extVal$(a%) = "null"
				exten$(a%) = "null"
			NEXT a%

			'--------- Main Symbol Parser -------------------------
			x% = 1
			type$ = CutAtNextDelimit$(codeLine$, x%, " ")
			IF x% >= LEN(codeLine$) THEN
				CALL PrintError(0, "Invalid symbol definition on line" + STR$(lineNum%) + ". Skipping symbol.")
				EOL% = 1
			END IF
			
			name$ = CutAtNextDelimit$(codeLine$, x%, " =#//")

			IF EOL% = 0 THEN
				CALL GetToNextChar(codeLine$, x%)
				IF MID$(codeLine$, x%, 1) = "=" THEN
					x% = x% + 1
					initVal$ = CutAtNextDelimit$(codeLine$, x%, " #//")
				END IF
			END IF

			DO WHILE x% <= LEN(codeLine$) AND CheckForBlankLine%(codeLine$, x%) = 0 AND EOL% = 0
				extCount% = extCount% + 1
				exten$(extCount%) = CutAtNextDelimit$(codeLine$, x%, " =,#//")
				CALL GetToNextChar(codeLine$, x%)
				IF MID$(codeLine$, x%, 1) = "=" THEN
					x% = x% + 1
					extVal$(extCount%) = CutAtNextDelimit$(codeLine$, x%, " ,#//")
				END IF
				IF MID$(codeLine$, x%, 1) = "," THEN
					x% = x% + 1     'get past the comma
					IF CheckForBlankLine%(codeLine$, x%) = 1 THEN CALL PrintError(0, "Another symbol extension expected on line" + STR$(lineNum%) + ", but none found.")
				END IF
			LOOP

			'--------- Check Symbol -------------------------------

			IF EOL% = 0 THEN

			CALL CheckSymType(type$)
			CALL CheckSymName(type$, name$)
			CALL CheckSymVal(type$, initVal$)
			FOR a% = 1 TO extCount%
				CALL CheckSymExt(type$, exten$(a%), extVal$(a%))
			NEXT a%
			CALL CheckForExtConflict(exten$(), extCount%)

			'--------- Substitute Type ----------------------------

			realType$ = type$
			type$ = SubType$(type$)

			'--------- Record Variable ----------------------------

			varCount% = varCount% + 1
			varName$(varCount%) = name$
			varType$(varCount%) = type$

			IF InStringArray%(exten$(), "local") = 0 THEN
				IF realType$ = "sector" OR realType$ = "surface" OR realType$ = "thing" THEN varConst$(varCount%) = name$
				numAssignVars% = numAssignVars% + 1
				assignVar$(numAssignVars%) = name$
			ELSEIF initVal$ <> "null" THEN
				numAssignVars% = numAssignVars% + 1
				assignVar$(numAssignVars%) = name$
			END IF

			END IF  'for EOL check
		END IF
		UpdateBar
	LOOP

	x% = 1
	codeWord$ = CutAtNextDelimit(codeLine$, x%, " #")
	IF codeWord$ <> "end" THEN CALL PrintError(1, "No end to the symbols section.")
	IF CheckForBlankLine(codeLine$, x%) = 0 THEN CALL PrintError(0, "Garbage found after end of the symbols section on line" + STR$(lineNum%) + ".")
	
END SUB

SUB PrintError (priority%, err$)

	'----------------------------------------------------------------------
	' PrintError is called to print an error in the API error box. If
	' "no errors" or "Warning" are not found in the string, then this sub
	' will call MakeExit to end the program.
	'----------------------------------------------------------------------

	DIM extra$
	DIM a%
	COLOR 4, 7

	errNum% = errNum% + 1
	IF errNum% <= 6 THEN
		IF LEN(err$) > 67 THEN
			CALL ChopString(err$, extra$, 67)
			errors$(errNum%) = err$
			errNum% = errNum% + 1
			errors$(errNum%) = extra$
		ELSE
			errors$(errNum%) = err$
		END IF
	ELSE
		IF LEN(err$) > 67 THEN
			FOR a% = 1 TO 4
				errors$(a%) = errors$(a% + 1)
			NEXT a%
			CALL ChopString(err$, extra$, 67)
			errors$(5) = err$
			errNum% = errNum% + 1
			errors$(6) = extra$
		ELSE
			FOR a% = 1 TO 5
				errors$(a%) = errors$(a% + 1)
			NEXT a%
			errors$(6) = err$
		END IF
	END IF

	FOR a% = 1 TO 6
		LOCATE 10 + a%, 7
		PRINT errors$(a%) + STRING$(67 - LEN(errors$(a%)), 32)
	NEXT a%

	IF errNum% >= 6 AND INSTR(err$, "Warning") < 1 AND INSTR(err$, "Parse of") < 1 AND priority% = 0 THEN
		PrintStatus ("Too many errors in " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + ". Aborting...")
		MakeExit
	END IF

	IF priority% > 0 THEN
		PrintStatus ("Fatal Error in " + ReverseCutAtDelimit$(COMMAND$, LEN(COMMAND$), "\") + ". Unable to continue parse.")
		MakeExit
	END IF

END SUB

SUB PrintStatus (stat$)

	'----------------------------------------------------------------------
	' This sub prints strings to the status bar of the API.
	'----------------------------------------------------------------------

	COLOR 0, 3
	LOCATE 23, 2
	PRINT stat$ + SPACE$(70 - LEN(stat$))

END SUB

SUB ReadFileData

	'----------------------------------------------------------------------
	' This sub opens the data.dat file and stores the contained data in
	' several global arrays.
	'----------------------------------------------------------------------

	DIM x%
	DIM a%
	DIM vParamNum%
	DIM codeWord$
	DIM numSymTypes%
	DIM numSymExts%
	DIM numMessages%
	DIM numVerbs%

	OPEN "data.dat" FOR INPUT AS #1

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

FUNCTION ReverseCutAtDelimit$ (codeLine$, x%, del$)

	'This function does not feature the
	'intuitive delimiter-guessing that
	'the original CAND has.

	DIM lastBreak%
	lastBreak% = x%
	
	FOR x% = x% TO 1 STEP -1
		IF INSTR(del$, MID$(codeLine$, x%, 1)) > 0 THEN EXIT FOR
	NEXT x%

	ReverseCutAtDelimit$ = LCASE$(Trim$(MID$(codeLine$, x% + 1, lastBreak%)))

END FUNCTION

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
			CALL PrintError(0, "Garbage found on line" + STR$(lineNum%) + ". Double-slash comments are not allowed.")
		ELSE
			EXIT DO
		END IF
	LOOP

END SUB

FUNCTION SubType$ (type$)

	IF type$ = "ai" THEN
		SubType$ = aisub$
	ELSEIF type$ = "cog" THEN
		SubType$ = cogsub$
	ELSEIF type$ = "flex" THEN
		SubType$ = flexsub$
	ELSEIF type$ = "float" THEN
		SubType$ = floatsub$
	ELSEIF type$ = "int" THEN
		SubType$ = intsub$
	ELSEIF type$ = "keyframe" THEN
		SubType$ = keyframesub$
	ELSEIF type$ = "material" THEN
		SubType$ = materialsub$
	ELSEIF type$ = "message" THEN
		SubType$ = messagesub$
	ELSEIF type$ = "model" THEN
		SubType$ = modelsub$
	ELSEIF type$ = "surface" THEN
		SubType$ = surfacesub$
	ELSEIF type$ = "sound" THEN
		SubType$ = soundsub$
	ELSEIF type$ = "sector" THEN
		SubType$ = sectorsub$
	ELSEIF type$ = "template" THEN
		SubType$ = templatesub$
	ELSEIF type$ = "thing" THEN
		SubType$ = thingsub$
	ELSEIF type$ = "vector" THEN
		SubType$ = vectorsub$
	ELSE
		SubType$ = type$
	END IF

END FUNCTION

FUNCTION Trim$ (tString$)

	'----------------------------------------------------------------------
	' This function was created to remove spaces and tabs from either side
	' of a string. The RTRIM and LTRIM library functions do not trim tabs.
	'----------------------------------------------------------------------

	DIM char$

	char$ = LEFT$(tString$, 1)
	DO WHILE char$ = " " OR char$ = CHR$(9)
		tString$ = RIGHT$(tString$, LEN(tString$) - 1)
		char$ = LEFT$(tString$, 1)
	LOOP

	char$ = RIGHT$(tString$, 1)
	DO WHILE char$ = " " OR char$ = CHR$(9)
		tString$ = LEFT$(tString$, LEN(tString$) - 1)
		char$ = RIGHT$(tString$, 1)
	LOOP

	Trim$ = tString$

END FUNCTION

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

	COLOR 2, 7
	FOR a% = 1 TO percent%
		LOCATE 5, 16 + a%
		PRINT CHR$(219)
	NEXT a%

END SUB

