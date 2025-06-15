package main

import (
	"fmt"
	"github.com/danswartzendruber/avl"
)

//
// A set of wrapper routines to the AVL package.  We do this to
// hide the AVL interface from the BASIC-PLUS interpreter code,
// as well as providing debug/trace hooks
//

//
// NB: the CONT (continue) command in BASIC-PLUS assumes the program
// structure has not changed.  So, if we insert or remove any statement
// nodes, we need to reinitialize the run structure, so as to disallow
// a CONT command.  We also nuke the active symbol table to remove any
// leftover values
//

func stmtAvlTreeFirstInOrder() *stmtNode {

	p := avl.AvlTreeFirstInOrder(g.program)
	if p != nil {
		return p.(*stmtNode)
	} else {
		return nil
	}
}

func stmtAvlTreeLastInOrder() *stmtNode {

	p := avl.AvlTreeLastInOrder(g.program)
	if p != nil {
		return p.(*stmtNode)
	} else {
		return nil
	}
}

func stmtAvlTreeInsert(stmt *stmtNode, cmp avl.CmpFuncNode) {

	p := avl.AvlTreeInsert(&g.program, &stmt.avl, stmt, cmp)
	if p != nil {
		fatalError(fmt.Sprintf("Stmt %d already in tree???", stmt.stmtNo))
	}

	// If it's an END statement, set the global

	if stmt.token == END {
		g.endStmtNo = stmt.stmtNo
	}

	initializeRun()

	initSymbolTable()

	//
	// Set the modified flag
	//

	setModified()
}

func stmtAvlTreeNextInOrder(stmt *stmtNode) *stmtNode {

	p := avl.AvlTreeNextInOrder(&stmt.avl)
	if p != nil {
		return p.(*stmtNode)
	} else {
		return nil
	}
}

func stmtAvlTreeNextStmt(stmt *stmtNode) *stmtNode {

	//
	// If stmt.head is not nil, it's the 2nd or later statement on
	// the same line, so point back to the 1st statement, to ensure
	// we have a non-zero stmtNo to pass to the AVL code
	//

	if stmt.head != nil {
		stmt = stmt.head
	}

	//
	// The 1st statement on a line MUST have a non-zero stmtNo
	//

	if stmt.stmtNo == 0 {
		fatalError("stmt.stmtNo is 0")
	}

	return stmtAvlTreeNextInOrder(stmtAvlTreeLookup(stmt.stmtNo, cmpInt16Key))
}

func stmtAvlTreeLookup(key int16, cmp avl.CmpFuncKey) *stmtNode {

	p := avl.AvlTreeLookup(g.program, key, cmp)
	if p != nil {
		return p.(*stmtNode)
	} else {
		return nil
	}
}

func stmtAvlTreeRemove(stmt *stmtNode) {

	avl.AvlTreeRemove(&g.program, &stmt.avl)

	//
	// If we just deleted the one and only END statement, zero out
	// g.endStmtNo
	//

	if g.endStmtNo == stmt.stmtNo {
		g.endStmtNo = 0
	}

	initializeRun()

	initSymbolTable()

	//
	// If we have one or more statements left, set the modified flag
	// Tricky: g.program is the AVL root node, so when we delete the
	// last statement, the root node will become nil, at which point
	// we want to clear the modified flag (since the program is now
	// empty)
	//

	if g.program != nil {
		setModified()
	} else {
		clearModified()
	}
}
