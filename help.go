package main

import (
	"fmt"
)

func executeHelp(targ *tokenNode) {

	if targ == nil {
		fmt.Println("bye")
		fmt.Println("config")
		fmt.Println("cont")
		fmt.Println("delete")
		fmt.Println("denorm")
		fmt.Println("list")
		fmt.Println("listnh")
		fmt.Println("new")
		fmt.Println("new")
		fmt.Println("old")
		fmt.Println("reload")
		fmt.Println("renumber")
		fmt.Println("run")
		fmt.Println("runnh")
		fmt.Println("save")
		fmt.Println("stats")
		fmt.Println("trace")
		return
	}

	switch targ.token {
	case BYE:
		fmt.Println("Exit from BASIC-PLUS")

	case CONFIG:
		fmt.Println("Print BASIC-PLUS build configuration")

	case CONT:
		fmt.Println("Continue execution of user program")

	case DELETE:
		fmt.Println("Delete one or more statements/ranges of statements")

	case DENORM:
		fmt.Println("Toggle denormalized floating point mode")

	case LIST:
		fmt.Println("List one or more statements/ranges of statements" +
			" with build info")

	case LISTNH:
		fmt.Println("List one or more statements/ranges of statements")

	case NEW:
		fmt.Println("Erase current program, optionally specifying a" +
			" new filename")

	case OLD:
		fmt.Println("Load an existing program")

	case RELOAD:
		fmt.Println("Re-execute the current interpreter binary" +
			" with the current saved program")

	case RENUMBER:
		fmt.Println("Renumber the current program starting at the" +
			" specified statement number/increment")

	case RUN:
		fmt.Println("Execute the current program, printing the build info")

	case RUNNH:
		fmt.Println("Execute the current program")

	case SAVE:
		fmt.Println("Save the current program, optionally specifying a" +
			" new filename")

	case STATS:
		fmt.Println("Toggle printing execution statistics when user" +
			" program stops")

	case TRACE:
		fmt.Println("Toggle tracing of user statement execution" +
			" or variable modification")
		fmt.Println("\ttrace exec")
		fmt.Println("\ttrace vars")
		fmt.Println("\ttrace <variable name>")
	}
}
