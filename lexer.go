package main

import (
	"fmt"
	"io"
	"strconv"
	"strings"
	"text/scanner"
	"unicode"
)

var keywordMap map[string]int

var deferredStmtNo int16
var parsingDef bool

func NewLexer() *Lexer {

	var eof bool
	var err error

	lexer := &Lexer{}

	if g.programFile != nil {
		lexer.line, err = g.programFile.reader.ReadString('\n')
		if err == io.EOF {
			if len(lexer.line) == 0 {
				closeProgramFile()
				if g.endStmtNo == 0 {
					missingEnd()
				}
			}
		} else if err != nil {
			closeProgramFile()
			exitToPrompt(fmt.Sprintf("ReadString error %q\n", err))
		} else {
			lexer.line = strings.TrimSuffix(lexer.line, "\n")
		}
	} else {
		lexer.line, eof = readLine(g.parserLiner, myPrompt, true)
		if eof {
			return (lexer)
		}
	}

	//
	// Ugly: the low-level scanner will get confused if it sees
	// something like '100end', as the '100e' will look like the
	// beginning of a floating point number in exponential format,
	// and the 'nd' will trigger an invalid floating point constant
	// error.  If we see a line starting with a digit, insert a single
	// space after the last digit in the sequence.  The subsequent call
	// to trimWhitespace will make sure the line is pretty-printed
	//

	if len(lexer.line) > 0 && unicode.IsDigit(rune(lexer.line[0])) {
		for i := 1; i < len(lexer.line); i++ {
			if !unicode.IsDigit(rune(lexer.line[i])) {
				lexer.line = replaceSubstring(lexer.line, uint16(i),
					uint16(i), " ")
				break
			}
		}
	}

	lexer.line = trimWhitespace(lexer.line)

	myScanner(lexer)

	return (lexer)
}

//
// If a runtime error occurs on a statement where the line contains
// multiple statements, it's not good enough to print the line number
// or the source line, since we can't tell *which* statement was the
// offender.  To address this, the lexer provides a way for the parser
// to fetch a list of the token structures comprising the statement in
// question, allowing us to underline the culprit
//

func (yylex *Lexer) Lex(lval *yySymType) int {

	//
	// This should not be possible, as we should already have
	// send up an EOL token, causing the parser to exit
	//

	if len(yylex.tokens) == 0 {
		return EOL
	}

	t := yylex.tokens[0]

	yylex.tokens = yylex.tokens[1:]

	*lval = t.lval

	return (t.token)
}

func (yylex *Lexer) ErrorLoc(e string, errLoc *yySymLoc) {
	errorLoc(e, errLoc)
}

//
// Only called if the parser doesn't implement token location tracking
//

func (yylex *Lexer) Error(e string) {
	errorLoc(e)
}

//
// This function is called either by the Error() method, or action
// rules in the parser, and is passed token location information ala
// BISON.  At least one caller has no useful token location information,
// so it passes nothing.  Cope with that by not printing the identifying
// text in red.  Finally, we panic with a crawlout, to cause us to crawl
// out to the interpreter command loop if requested.  This will work for
// the explicit callers, as well as the Error() method called by the parser.
// This is helpful, as goyacc (unlike BISON) has no YYERROR macro to force
// a parse error
//
// We accept 0, 1 or 2 location tokens.  Functionality:
// 0. Print no location info (e.g. no colorized text)
// 1. Print location information for the token in question
// 2. Generate a temporary location structure that spans both
//    tokens
//

func errorLoc(e string, yylloc ...*yySymLoc) {

	errorLocFull(g.yylex.line, e, true, yylloc...)
}

func errorLocFull(l string, e string, doPanic bool, yylloc ...*yySymLoc) {

	var tLoc yySymLoc

	if e != "" {
		fmt.Println()
	}

	switch len(yylloc) {
	default:
		fatalError("Too many location tokens (%d)", len(yylloc))

	case 0:
		// NOP

	case 1:
		tLoc.pos.column = yylloc[0].pos.column
		tLoc.end.column = yylloc[0].end.column

	case 2:
		tLoc.pos.column = yylloc[0].pos.column
		tLoc.end.column = yylloc[1].end.column
	}

	if l != "" {
		if tLoc.pos.column != 0 {
			var colorSeq string

			//
			// If the token column is less than the length of the
			// output line, the parser is basically complaining
			// about a premature end of input, so change the error
			// string to something more informative
			//

			if e != "" {
				if tLoc.pos.column <= len(l) {
					colorSeq = colorRedSeq
				} else {
					e = "Unexpected end of input"
					l = l + " "
				}
			} else {
				colorSeq = colorInverseVideoSeq
			}

			l = colorizeString(l, &tLoc, colorSeq)
		}

		fmt.Println(l)
	}

	if e != "" {
		fmt.Println(e)
	}

	if doPanic {
		panic(&crawloutException{false})
	}
}

//
// Another ugly hack.  BASIC-PLUS allows '!' (COMMENT) as well as
// the REM(ark) verb.  The ugliness arises because '!' can be a verb
// itself (e.g. '100 ! this is a comment'), as well as a directive to
// ignore the rest of the line (ala the "C" '//' syntax).  We can't
// just ignore everything up to the NL, since '100 ! blah blah blah\n'
// would be equivalent to '100\n', which is a directive to delete statement
// 100 (if it exists), and at best, would not result in a statement with
// statement number 100 being added to the program. If the comment character
// follows an initial statement number (which must be the first token on
// the current line), return a COMMENT token
//

func saveToken(yylex *Lexer, tok Lval) {
	yylex.tokens = append(yylex.tokens, tok)
}

//
// This routine will scan lexemes from the current input string,
// sending them to Lex() via a channel until EOF, at which point
// we close the channel and return.  Lex() will receive a zero
// lexeme reading from the closed channel, and terminate
//

func myScanner(yylex *Lexer) {
	var s scanner.Scanner
	var deferredStmt bool
	var done bool
	var lexingFilename bool
	var prevToken Lval

	if len(yylex.line) > maxLineLen {
		t := Lval{token: LONGLINE}
		saveToken(yylex, t)
		return
	}

	sinput := strings.NewReader(yylex.line)

	s.Init(sinput)
	s.Mode = scanner.ScanIdents | scanner.ScanInts | scanner.ScanFloats
	s.IsIdentRune = basicPlusIdent
	s.Error = dummyScannerError

	//
	// Read lexemes from getLexeme until we get an EOF, at which point
	// we close the channel and return (which will terminate this goroutine)
	//

	//
	// If we see EOF, return.
	// If we see a REM token, send it back, and terminate the loop,
	// If we see a '!' character, it has to be handled in one of two ways:
	// if it's the first token after a statement number, send back a
	// COMMENT token, and return, otherwise, ignore the '!' and terminate
	// the loop
	//

	for i := 0; !done; i++ {

		//
		// If lexing a filename, consume the rest of the input line,
		// and return a filename
		//

		if lexingFilename {
			t := scanFilename(&s)
			saveToken(yylex, t)
			if t.token != EOL {
				saveToken(yylex, Lval{token: EOL})
			}

			return
		}

		t, eof := getLexeme(&s)

		if eof {

			//
			// If we got EOL, we want to have the token location
			// point to just past the last real character on the
			// input line.  We need to do this to diagnose the
			// unexpected end of input.  On EOL, it just so happens
			// that the end column is less than the starting column,
			// in which case getTokenLoc sets the end to the start.
			//
			// Like this:
			// % x = 1 /
			// x = 1 /
			//        ^
			// syntax error
			//

			if prevToken.token != 0 {
				prevToken.token = prevTokenFixup(prevToken.token)
				saveToken(yylex, prevToken)
			}

			t = Lval{token: EOL}
			t.lval.symLoc = getTokenLoc(&s)

			saveToken(yylex, t)

			return
		}

		//
		// If we see a BADFILENAME token, close the channel
		// and return so as not to confuse the parser
		//

		if t.token == BADFILENAME {
			saveToken(yylex, t)
			return
		}

		//
		// If this is COMMA or SEMI, save the trailing version, so that
		// if the next token is EOL, COLON or ELSE, we can send it up
		// first, otherwise nuke any prevToken
		//

		if t.token == COMMA || t.token == SEMI {
			prevToken = Lval{token: t.token}
			prevToken.lval.symLoc = getTokenLoc(&s)
			continue
		} else if t.token == ELSE || t.token == COLON {
			if prevToken.token != 0 {
				prevToken.token = prevTokenFixup(prevToken.token)
			}
		}

		if prevToken.token != 0 {
			saveToken(yylex, prevToken)
			prevToken.token = 0
		}

		switch t.token {

		//
		// If the first non-statement number token is DEF, set a flag
		// to facilitate distinguishing function names from regular
		// variable names
		//

		case DEF:
			if i == 1 && deferredStmt {
				parsingDef = true
			}

		//
		// If we see REM anywhere after the statement number,
		// terminate the loop.  NB: the parser will reject a
		// REM token anywhere other than the 1st token
		//

		case REM:
			if i >= 1 && deferredStmt {
				done = true
			}

		case NEW, OLD, SAVE:
			if i == 0 {
				lexingFilename = true
			}

		//
		// If we see an integer, and it's the first token on the line,
		// flag that for later in the loop.  Otherwise, peek at the
		// next character, and if it's a '%', return the numeric token
		// with token type EINTEGER (explicit integer, like '100%'),
		// else return the numeric token with token type INTEGER.
		// We do this to make life easier on the parser.
		// Note that deferredStmtNo is int16, so if the user types
		// an integer that is out of range, deferredStmtNo will end
		// up with garbage, but that's harmless, since the parser
		// is being handed an int64 to examine, and will diagnose
		// the illegal line number anyway
		//

		case INTEGER:
			if i == 0 {
				deferredStmt = true
				deferredStmtNo = int16(t.lval.int64Val)
			} else {

				//
				// Note that the hack for EINTEGER results in the ending
				// token location column being off by one, so we correct
				// that here.  Unfortunately, we can't do this post
				// processing in getLexeme(), where SVAR and IVAR are
				// handled, because we need to be aware of the
				// 'first token on the line'
				//

				if s.Peek() == '%' {
					t.token = EINTEGER
					t.lval.symLoc.end.column++
					_ = s.Next()
				}
			}

		case '!':

			//
			// '!' is dodgy.  If it's the first token after the
			// statement number, return a COMMENT token (this is
			// equivalent to a REM statement).  If it occurs
			// anywhere after that, ignore the rest of the line
			// (akin to the "C" '//' directive.  We also return
			// COMMENT if the '!' is the first token.  This is
			// illegal syntax, but the parser will catch this
			//

			if (i == 1 && deferredStmt) || i == 0 {
				t.token = COMMENT
				done = true
			} else {
				t.lval.symLoc = getTokenLoc(&s)

				saveToken(yylex, t)

				return
			}
		}

		//
		// Transmit the completed token to the upper level of
		// the lexer code
		//

		saveToken(yylex, t)
	}
}

//
// This function is passed in a token value of COMMA or SEMI and
// returns TRAILING_COMMA or TRAILING_SEMI
//

func prevTokenFixup(tok int) int {

	if tok == COMMA {
		return TRAILING_COMMA
	} else if tok == SEMI {
		return TRAILING_SEMI
	} else {
		msg := fmt.Sprintf("tok == %v", tok)
		panic(msg)
	}

}

func getLexeme(s *scanner.Scanner) (Lval, bool) {

	var t Lval

	tok := s.Scan()
	txt := s.TokenText()

	if tok == scanner.EOF {
		return Lval{}, true
	}

	switch tok {
	case scanner.Ident:

		//
		// Lower-case all letters.
		//

		txt = strings.ToLower(txt)

		//
		// We *could* have basicPlusIdent() check the passed-in
		// position against maxVariableLen, but that can lead to an
		// ugly 'syntax error' message from the parser.  Consider what
		// would happen if we try to lex an otherwise syntactically
		// legal identifier > 29 characters?  The low-level scanner
		// will stop at the 29th character and return what it has so far.
		// The next call to the lexer will return (at least) one more
		// identifier, which the parser will reject with 'syntax error',
		// as there are no rules that allow consecutive identifiers.
		// Our solution: allow the scanner to lex as long of an identifier
		// as it can match.  Once it hands it back to us, if the lexeme is
		// longer than maxVariableLen, we return a BADVARIABLE token
		//

		if len(txt) > maxVariableLen {
			t.token = BADVARIABLE
			break
		}

		//
		// Look the identifier up in the keyword lexeme map,
		// and return the keyword if found
		//

		keyword := keywordMap[txt]
		if keyword != 0 {
			t = Lval{token: keyword}
			break
		}

		//
		// At this point, we know it's some kind of variable,
		// so save the name
		//

		t = Lval{}
		t.lval.stringVal = txt

		numDol := strings.Count(txt, "$")
		numPer := strings.Count(txt, "%")
		isFunctionName := false

		//
		// Special case user defined function names here to make life
		// MUCH easier on the parser
		//

		if len(txt) > 2 && strings.HasPrefix(txt, "fn") {
			if unicode.IsLetter(rune(txt[2])) {
				isFunctionName = true
			} else {
				t.token = BADFUNCNAME
				break
			}
		}

		//
		// parsingDef exists for the sole purpose of detecting a non
		// function name (e.g. not starting with 'fn' as the function
		// name component in a DEF statement.  Sadly, we can't check for
		// this in the parser itself, lest we get a bazillion conflicts.
		// The flag is set by the lexer immediately after seeing a DEF
		// token, and reset here.  We don't NEED to do this, but if we
		// don't, something like '100 def foo=0' will get the canonical
		// (e.g. useless) 'syntax error' message
		//

		if parsingDef {
			parsingDef = false
			if !isFunctionName {
				t.token = BADFUNCNAME
				break
			}
		}

		if numDol == 0 && numPer == 0 {
			if isFunctionName {
				t.token = FNFVAR
			} else {
				t.token = FVAR
			}

			break
		}

		if numDol == 1 && numPer == 0 && strings.HasSuffix(txt, "$") {
			if isFunctionName {
				t.token = FNSVAR
			} else {
				t.token = SVAR
			}

			break
		}

		if numDol == 0 && numPer == 1 && strings.HasSuffix(txt, "%") {
			if isFunctionName {
				t.token = FNIVAR
			} else {
				t.token = IVAR
			}

			break
		}

		//
		// If we get here, the purported identfier is malformed
		//

		t.token = BADVARIABLE

	case '\'', '"':
		t = lexString(s, tok)

	case scanner.Int:

		//
		// If we scanned an integer, but it's out of range for a
		// 64-bit integer, re-parse it as a float
		//

		i, e := strconv.ParseInt(txt, 10, 64)
		if e != nil {
			if e.(*strconv.NumError).Err == strconv.ErrRange {
				f, e := strconv.ParseFloat(txt, 64)
				if e != nil {
					fmt.Println(EILLEGALNUMBER)
					f = 0.0
				}
				t = Lval{token: FLOAT}
				t.lval.float64Val = f
			} else {
				fmt.Println(EILLEGALNUMBER)
				t = Lval{token: INTEGER}
				t.lval.int64Val = 0
			}
		} else {
			t = Lval{token: INTEGER}
			t.lval.int64Val = i
		}

	case scanner.Float:
		f, e := strconv.ParseFloat(txt, 64)
		if e != nil {
			fmt.Println(EILLEGALNUMBER)
			f = 0.0
		}

		t = Lval{token: FLOAT}
		t.lval.float64Val = f

		//
		// Complication: BASIC-PLUS implements the '==' operator, which is
		// defined as 'the two values look equivalent when printed'.  We
		// need to peek at the next character to see if this is the case
		//

	case '=':
		if s.Peek() == '=' {
			_ = s.Next()
			t = Lval{token: APPROX}
		} else {
			t = Lval{token: EQ}
		}

	case '<':
		if s.Peek() == '>' {
			_ = s.Next()
			t = Lval{token: NE}
		} else if s.Peek() == '=' {
			_ = s.Next()
			t = Lval{token: LE}
		} else {
			t = Lval{token: LT}
		}

	case '>':
		if s.Peek() == '=' {
			_ = s.Next()
			t = Lval{token: GE}
		} else {
			t = Lval{token: GT}
		}

	case '+':
		t = Lval{token: PLUS}

	case '-':
		t = Lval{token: MINUS}

	case '*':
		// BASIC-PLUS allows '**' as a synonym of '^'

		if s.Peek() == '*' {
			t = Lval{token: POW}
			_ = s.Next()
		} else {
			t = Lval{token: STAR}
		}

	case '/':
		t = Lval{token: SLASH}

	case '(':
		t = Lval{token: LPAR}

	case ')':
		t = Lval{token: RPAR}

		//
		// Ugly hack: BASIC PLUS allows two consecutive commas,
		// which causes the print cursor to advance one field.
		// It's awful trying to get yacc to accept that, so since
		// we already peeked at the next token anyway, if it's also
		// a comma, discard both commas, and return a fake token DCOMMA
		//

	case ',':
		if s.Peek() == ',' {
			t = Lval{token: DCOMMA}
			_ = s.Next()
		} else {
			t = Lval{token: COMMA}
		}

	case ';':
		t = Lval{token: SEMI}

	case ':':
		t = Lval{token: COLON}

	case '^':
		t = Lval{token: POW}

	case '#':
		t = Lval{token: POUND}

	default:
		t = Lval{token: int(tok)}
	}

	//
	// The boolean operators AND, EQV, IMP, NOT, OR and XOR
	// all look like floating variable names to the lexer,
	// so now that we're almost done, check for any of those
	// and if found, change the token type
	//

	if t.token == FVAR {
		switch t.lval.stringVal {
		case "and":
			t.token = AND

		case "eqv":
			t.token = EQV

		case "imp":
			t.token = IMP

		case "not":
			t.token = NOT

		case "or":
			t.token = OR

		case "xor":
			t.token = XOR

		}
	}

	//
	// Generate the token location information
	//

	t.lval.symLoc = getTokenLoc(s)

	return t, false
}

//
// Scan a BASIC-PLUS string.  It can be either single or double
// quoted, and can contain the other type of quote character,
// which is treated as a regular character
//

func lexString(s *scanner.Scanner, firstQuote rune) Lval {

	var buf []byte

	for {
		rch := s.Next()
		// unterminated string - return USTRING token
		if rch == scanner.EOF {
			return Lval{token: USTRING}
		}

		// we now have a complete string
		if rch == firstQuote {
			t := Lval{token: STRING}
			t.lval.stringVal = string(buf)
			return t
		}

		// something else, append to buf and keep going
		buf = append(buf, byte(rch))
	}
}

//
// This is a dummy to suppress reporting of errors by the scanner
//

func dummyScannerError(s *scanner.Scanner, msg string) {
}

//
// Ident predicate routine for text/scanner
//

func basicPlusIdent(ch rune, pos int) bool {

	//
	// Ugliness ahead: BASIC-PLUS has conversion functions that allow
	// '$' and/or '%' and not always at the end of the token, so trying
	// to scan something like cvt$f is problematic, to say the least.
	// Our 'solution' is to add '$' and '%' to the list of acceptable
	// characters in an identifier.  No valid names in BASIC-PLUS have
	// more than one of each of these, so after scanning an Ident, if we
	// have more than one of either, return BADVARIABLE, so the parser
	// can print a more user-friendly error message
	//
	// 1. If token is in the keyword table, we're good.
	// 2. If no '$' or '%' present, we have a floating variable.
	// 3. Else if one only of '$' or '%' in the token, we have a
	//    string variable or integer variable.
	// 4. Else we have the invalid token situation.  Unfortunately, we
	//    need to do these checks in the upper-level lexer, since that
	//    can send up multiple tokens - getLexeme can't.
	//

	if pos == 0 {
		if unicode.IsLetter(ch) {
			return true
		}
	} else if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '.' ||
		ch == '$' || ch == '%' {
		return true
	}

	return false
}

func getTokenNameAddr(token int) *string {

	basicAssert(token >= yyFirsttok, "invalid token %d\n", token)

	return &yyToknames[token-yyFirsttok+3]
}

func getTokenName(token int) string {

	if token >= yyFirsttok {
		return (*getTokenNameAddr(token))
	} else {
		return strconv.FormatInt(int64(token), 10)
	}
}

func scanFilename(s *scanner.Scanner) Lval {

	var str string
	var fname string
	var ok bool

	for {
		chr := s.Next()
		if chr == scanner.EOF {
			break
		} else {
			str = str + string(chr)
		}
	}

	parts := strings.Fields(trimWhitespace(str))

	switch len(parts) {
	default:
		return Lval{token: BADFILENAME}

	case 0:
		return Lval{token: EOL}

	case 1:
		if fname, ok = validateProgramFilename(parts[0]); !ok {
			return Lval{token: BADFILENAME}
		} else {
			t := Lval{token: FILENAME}
			t.lval.stringVal = fname
			return t
		}
	}
}

//
// This function takes the current scanner object, and gins up
// a yySymLoc object, returning that to the caller.  Because
// BASIC is line-oriented, we ignore the line fields
//
// NB: if the scanner ran out of data in the input string,
// the end column will be less than the start column

func getTokenLoc(s *scanner.Scanner) yySymLoc {

	var symLoc yySymLoc

	symLoc.pos.column = s.Position.Column
	symLoc.end.column = s.Pos().Column - 1

	if symLoc.end.column < symLoc.pos.column {
		symLoc.end.column = symLoc.pos.column
	}

	return symLoc
}
