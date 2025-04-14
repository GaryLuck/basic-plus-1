package main

//
// Manifest constants for specific DEC BASIC-PLUS error messages.
// NB: only messages with associated error numbers will be present
// in the errorMap
//

const (
	EILLEGALFILENAME     = "Illegal file name"
	EFILEINUSE           = "Account or device in use"
	EFILENOTFOUND        = "Can't find file or account"
	EALREADYOPEN         = "I/0 channel already open"
	EINVALIDDEVICE       = "Not a valid device"
	ENOTOPEN             = "I/O channel not open"
	EPROTECTIONVIOLATION = "Protection violation"
	EENDOFFILE           = "End of file on device"
	ETIMEOUT             = "Keyboard wait exhausted"
	EILLEGALCLUSTERSIZE  = "Illegal cluster size"
	EINTERRUPTED         = "Interrupted"
	EMATRIXTOOLARGE      = "Array or matrix too large"
	EILLEGALIOCHANNEL    = "Illegal I/O channel"
	ELINETOOLONG         = "Line too long"
	EFLOATINGERROR       = "Floating point error"
	EEXPERROR            = "Argument too large in EXP"
	EINTEGERERROR        = "Integer error"
	EILLEGALNUMBER       = "Illegal number"
	ELOGERROR            = "Argument to LOG/LOG10 <= 0"
	ESQRERROR            = "Argument to SQR is negative"
	ESUBSCRIPTERROR      = "Subscript out of range"
	EMATRIXERROR         = "Cannot invert matrix"
	EOUTOFDATA           = "Out of data"
	EONERROR             = "ON statement out of range"
	EINPUTERROR          = "Not enough data in record"
	EFOROVERFLOW         = "Integer overflow for FOR loop"
	EDIVISIONBYZERO      = "Division by 0"
	EILLEGALLINENUMBER   = "Illegal line number(s)"
	EDATATYPEERROR       = "Data type error"
)

//
// 'continuable' is a confusing concept per the DEC manual.  Any and
// all arithmetic faults are continuable, in the sense that if there
// is no ON ERROR handler, an appropriate message will be printed,
// and the appropriate 0 will be returned.  Calling it 'auto-continuable'
// would be more accurate, but we go with the term in the manual
//
// 4 cases for errors
//
// 1. Any error message not in the errorMap is not catchable with
//    an ON ERROR handler and is not continuable.  This is because
//    those error messages do not have a DEC-assigned numeric error
//    code, and that is necessary for ON ERROR
//
// 2. EINTERRUPTED cannot be resumed by an ON ERROR handler without
//    a target statement number as that would allow an infinite loop
//    you could not interrupt.  The alternative would be to never allow
//    catching EINTERRUPTED, but that could make cleaning up in certain
//    cases impossible.  This happens because RESUME starts the statement
//    in question from the beginning.  CONT is usable here.
//
// 3. Arithmetic errors can be caught by an ON ERROR handler, and if
//    they aren't, they return 0 (float or integer as required).
//    Because arithmetic faults are resumed from the beginning of the
//    statement, if you catch it with an error handler, either fix the
//    variable that was problematic (maybe a 0 divisor) and RESUME with
//    no statement number, or fix up the target of a LET statement,
//    RESUME to the next numbered statement, or bail out of the problematic
//    code block
//
// 4. Anything else can be caught by an error handler, and can be
//    resumed.  If not caught, you abort to command level, but the
//    CONT command cannot be used to resume execution
//

func initErrors() {

	errorMap[EILLEGALFILENAME] = 2
	errorMap[EFILEINUSE] = 3
	errorMap[EFILENOTFOUND] = 5
	errorMap[EINVALIDDEVICE] = 6
	errorMap[EALREADYOPEN] = 7
	errorMap[ENOTOPEN] = 9
	errorMap[EPROTECTIONVIOLATION] = 10
	errorMap[EENDOFFILE] = 11
	errorMap[ETIMEOUT] = 15
	errorMap[EILLEGALCLUSTERSIZE] = 23
	errorMap[EINTERRUPTED] = 28
	errorMap[EMATRIXTOOLARGE] = 44
	errorMap[EILLEGALIOCHANNEL] = 46
	errorMap[ELINETOOLONG] = 47
	errorMap[EFLOATINGERROR] = 48
	errorMap[EEXPERROR] = 49
	errorMap[EINTEGERERROR] = 51
	errorMap[EILLEGALNUMBER] = 52
	errorMap[ELOGERROR] = 53
	errorMap[ESQRERROR] = 54
	errorMap[ESUBSCRIPTERROR] = 55
	errorMap[EMATRIXERROR] = 56
	errorMap[EOUTOFDATA] = 57
	errorMap[EONERROR] = 58
	errorMap[EINPUTERROR] = 59
	errorMap[EFOROVERFLOW] = 60
	errorMap[EDIVISIONBYZERO] = 61

	for k, v := range errorMap {
		errorMapRev[v] = k
	}
}

//
// We return -1 on a failed lookup, as the fault handling code relies
// on a -1 meaning ON ERROR is not in play
//

func getErrorNo(msg string) int16 {

	err, ok := errorMap[msg]
	if ok {
		return err
	} else {
		return -1
	}
}

//
// It should not be possible for the  lookup on errorMapRev to fail
//

func getErrorMsg(err int16) string {

	errMsg, ok := errorMapRev[err]
	basicAssert(ok, "No error message")

	return errMsg
}
