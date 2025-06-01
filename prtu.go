package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

func printUsing(ioch int, tokens []prtuToken, ops []any) {

	var flen, tokIdx, opIdx int
	var line string

	//
	// Walk the token list, fetching operands from the op list until
	// it is exhausted.  If we exhaust the token list before all of
	// the operands have been consumed, reset to the beginning of the
	// token list.  Bizarrely, although the BASIC-PLUS docs don't say
	// so, the actual behavior is to terminate the current line if we
	// cycle to the beginning of the token list
	//

	for {
		if opIdx == len(ops) {
			break
		}

		//
		// If we've reached the end of the format string and there
		// are still operands to print, terminate the current line
		// and reset to the beginning of the format string
		//
		// NB: if we get to the end of the tokens, and haven't
		// consumed any operands, throw an error.  This could
		// happen if the format string has has no descriptors,
		// but we haven't processed any operands.  Something like:
		// "print using 'hello', 12345"
		//

		if tokIdx == len(tokens) {
			if opIdx == 0 {
				badFormat("")
			}
			resetPrint(true)
			tokIdx = 0
		}

		tok := tokens[tokIdx]
		tokIdx++

		switch tok.(type) {
		case prtuChar:
			info := tok.(prtuChar)
			line += info.ch
			continue
		}

		op := ops[opIdx]
		opIdx++

		switch tok.(type) {
		default:
			unexpectedTypeError(tok)

		case prtuString:
			val, ok := op.(string)
			runtimeCheck(ok, "String operand expected")

			info := tok.(prtuString)
			flen = info.len
			val = copyString(val, flen, false)

			line += val

		case prtuNumeric:
			val := prtuGetNumericFloat(op)

			info := tok.(prtuNumeric)

			line += prtuFormat(val, info)
		}
	}

	basicPrint(line, ioch, false)

	resetPrint(true)
}

func prtuGetNumericFloat(val any) float64 {

	switch val.(type) {
	default:
		unexpectedTypeError(val)

	case string:
		runtimeError("Numeric operand expected")

	case float64:
		return val.(float64)

	case int16:
		return float64(val.(int16))
	}

	panic(nil) // avoid compiler complaint
}

func prtuFormat(num float64, info any) string {

	var tmpstr, rbuf, buf, repstr string

	info2 := info.(prtuNumeric)
	fillch := " "
	left := info2.left
	right := info2.right
	fillchar := info2.fillchar
	dollar := info2.dollar
	doComma := info2.doComma
	trailingMinus := info2.trailingMinus
	exponential := info2.exponential

	//
	// Per the docs, asterisk fill and dollar sign cannot be used
	// for negative numbers unless trailing minus is enabled
	//

	if num < 0 && !trailingMinus && (dollar || fillchar) {
		badFormat("for negative number")
	}

	//
	// 'f' format generates extraneous digits if we give it
	// a float64 with more digits than will fit the mantissa,
	// so punt if we see this!  This is 9007199254740992.
	// We have to use 'f' instead of 'g' as the latter will convert
	// any number > abs(999999) in exponential format, which is
	// incompatible with DEC behavior
	//

	if exponential {
		tmpstr = strconv.FormatFloat(num, 'e', right, 64)
	} else if math.Abs(num) >= myMaxFNum {
		return "% " + strconv.FormatFloat(num, 'g', -1, 64)
	} else {
		tmpstr = strconv.FormatFloat(num, 'f', right, 64)
	}

	if dollar {
		left += 2
		tmpstr = "$" + tmpstr
	} else if fillchar {
		left += 2
		fillch = "*"
	}

	eParts := strings.Split(tmpstr, "e")
	if len(eParts) == 2 {
		if len(eParts[1]) == 3 {
			if eParts[1][0] == '-' {
				repstr = "-"
			} else {
				repstr = "+"
			}
			eParts[1] = strings.Replace(eParts[1], repstr, repstr+"0", 1)
		}

		eParts[1] = "e" + eParts[1]
	}

	fltParts := strings.Split(eParts[0], ".")

	llen := len(fltParts[0])

	switch len(fltParts) {
	default:
		panic("Bad floating point format!")

	case 2:
		rbuf = fltParts[1]
		rlen := len(rbuf)

		//
		// Truncate extraneous digits after the decimal point
		// (more fallout from 'f' format conversion)
		//

		if llen+rlen > 16 {
			rbuf = rbuf[0 : 16-llen]
			rlen = len(rbuf)
		}

		if rlen > right {
			rbuf = rbuf[0:right]
		} else if rlen < right {
			rbuf += strings.Repeat("0", right-rlen)
		}

		rbuf = "." + rbuf

		fallthrough

	case 1:
		if llen > left {
			return "%" + strings.TrimPrefix(tmpstr, "$")
		} else if llen < left {
			buf = strings.Repeat(fillch, left-llen)
		} else {
			buf = ""
		}

		//
		// If num is an integer but the format field specifies
		// one or more digits after the decimal, append as many
		// '0' characters as needed
		//

		if len(fltParts) == 1 && right > 0 {
			rbuf = "." + strings.Repeat("0", right)
		}
	}

	if len(eParts) == 2 {
		rbuf += eParts[1]
	}

	tmpstr = ""
	tlstr := fltParts[0]
	tmpCnt := 0
	ncommas := 0

	if doComma {
		for i := len(tlstr) - 1; i >= 0; i-- {
			tmpstr = string(tlstr[i]) + tmpstr
			if ((tmpCnt+1)%3) == 0 && i > 0 {
				tmpstr = "," + tmpstr
				ncommas++
			}
			tmpCnt++
		}
		tlstr = tmpstr
		trimStr := strings.Repeat(fillch, ncommas)
		buf = strings.TrimPrefix(buf, trimStr)
	}

	buf += tlstr + rbuf

	if trailingMinus && num < 0 {
		buf = strings.Replace(buf, "-", "", 1) + "-"
	}

	return buf
}

func badFormat(msg string) {

	tstr := "Format error"
	if msg != "" {
		tstr += " " + msg
	}

	runtimeError(tstr)

	fmt.Println("") // avoid complaint about 'fmt' not used
}

func prtuSaveToken(prtuCtl *prtuLexer, tok prtuToken) {

	info, ok := tok.(prtuNumeric)

	//
	// Per the docs, asterisk fill and dollar sign cannot be
	// used in exponential mode
	//

	if ok {
		if (info.dollar || info.fillchar) && info.exponential {
			badFormat("")
		}
	}

	prtuCtl.tokens = append(prtuCtl.tokens, tok)
}

func prtuParse(prtuCtl *prtuLexer) {

	var info prtuNumeric
	var tmp, tmp2 string
	var pch byte

	for {
		switch prtuPeekch(prtuCtl) {
		default:
			prtuSaveToken(prtuCtl, prtuChar{string(prtuGetch(prtuCtl))})

		case 0:
			return

		case '$':
			prtuCtl.idx++

			if prtuPeekch(prtuCtl) != '$' {
				prtuSaveToken(prtuCtl, prtuChar{"$"})
			} else {
				prtuCtl.idx++
				info.dollar = true
			}

		case '*':
			prtuCtl.idx++

			if prtuPeekch(prtuCtl) != '*' {
				prtuSaveToken(prtuCtl, prtuChar{"*"})
			} else {
				prtuCtl.idx++
				info.fillchar = true
			}

		case '#':
			//
			// Numeric conversion with no decimal point:
			//

			tmp, info.doComma = prtuGetseq(prtuCtl, '#', ',')
			info.left = len(tmp)

			pch = prtuPeekch(prtuCtl)

			//
			// No decimal point - pseudo-integr
			//

			if pch != '.' {
				if pch == '-' {
					info.trailingMinus = true
					prtuCtl.idx++
				} else if pch == '^' {
					info.exponential = checkExponential(prtuCtl)
				}

				prtuSaveToken(prtuCtl, info)

				break
			}

			prtuCtl.idx++

			//
			// BASIC-PLUS apparently allows '###.' which will result in
			// output like '123.'.  Not sure how to deal with this yet
			// as the printf code I leverage doesn't support that :(
			//

			if prtuPeekch(prtuCtl) != '#' {
				badFormat("")
			}

			//
			// Numeric conversion with decimal point:
			// NUMERIC token with info containing the left and right width
			//

			tmp2, info.doComma = prtuGetseq(prtuCtl, '#', ',')
			info.right = len(tmp2)

			pch = prtuPeekch(prtuCtl)

			if pch == '-' {
				info.trailingMinus = true
				prtuCtl.idx++
			} else if pch == '^' {
				info.exponential = checkExponential(prtuCtl)
			}

			prtuSaveToken(prtuCtl, info)

			//
			// Initial substring:
			// STRING token with info containing length of substring
			//

		case '\\':
			pch := prtuGetch(prtuCtl)
			tmp, _ = prtuGetseq(prtuCtl, ' ', 0)

			if prtuPeekch(prtuCtl) != '\\' {
				badFormat("")
			}

			tmp = string(pch) + tmp + string(prtuGetch(prtuCtl))

			prtuSaveToken(prtuCtl, prtuString{len(tmp)})

			//
			// First character of string
			// STRING token with info containing length = 1
			//

		case '!':
			prtuCtl.idx++
			prtuSaveToken(prtuCtl, prtuString{1})
		}
	}
}

func checkExponential(prtuCtl *prtuLexer) bool {

	var exponential bool

	tmp, _ := prtuGetseq(prtuCtl, '^', 0)
	if len(tmp) < 4 || len(tmp) > 5 {
		badFormat("")
	} else {
		exponential = true
	}

	return exponential
}

func prtuGetseq(prtuCtl *prtuLexer, b1, b2 byte) (string, bool) {

	tmpbuf := make([]byte, 0)
	sawb2 := false

	for {
		pch := prtuPeekch(prtuCtl)
		if pch == b2 && pch != 0 {
			sawb2 = true
			prtuCtl.idx++
		} else if pch == b1 {
			tmpbuf = append(tmpbuf, prtuGetch(prtuCtl))
		} else {
			break
		}
	}

	return string(tmpbuf), sawb2
}

func prtuPeekch(prtuCtl *prtuLexer) byte {

	var ch byte

	if prtuCtl.idx == len(prtuCtl.buf) {
		ch = 0
	} else {
		ch = prtuCtl.buf[prtuCtl.idx]
	}

	return ch
}

func prtuGetch(prtuCtl *prtuLexer) byte {

	if prtuCtl.idx == len(prtuCtl.buf) {
		panic("Input buffer botch!")
	}

	ch := prtuCtl.buf[prtuCtl.idx]
	prtuCtl.idx++

	return ch
}
