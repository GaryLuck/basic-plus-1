package main

import (
	"fmt"
)

func executeHelp(targ *tokenNode) {

	if targ == nil {
		fmt.Println("assert")
		fmt.Println("bye")
		fmt.Println("config")
		fmt.Println("cont")
		fmt.Println("delete")
		fmt.Println("denorm")
		fmt.Println("edit")
		fmt.Println("kill")
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
	case ASSERT:
		fmt.Println("Toggle interpreter assertions")

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

	case DUMP:
		fmt.Println("Tottle dumping of interpreter structures")

	case EDIT:
		fmt.Println("Invoke the editor defined by the environment")
		fmt.Println("variable EDITOR on the current (saved) program")

	case KILL:
		fmt.Println("Delete a quoted file")

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
		fmt.Println("Toggle printing of execution statistics when user" +
			" program stops")

	case TRACE:
		fmt.Println("toggle tracing of")
		fmt.Println()

		fmt.Println("\tdump - intrepreter structures")
		fmt.Println("\texec - statement execution")
		fmt.Println("\tvars - all variable modifications")
		fmt.Println("\t<variable name> - specific variable modification")
	}
}
