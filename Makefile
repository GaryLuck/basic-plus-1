
GO_FILES = basic.go definitions.go errors.go execute.go help.go lexer.go \
	prtu.go stmt.go symtab.go utils.go parser.y avl.go
EXECUTABLE = basic-plus
DOCS = ./docs
LDFLAGS = "-X 'main.buildTimestampStr=`date`'"
GITPATH = ~/git/basic-plus

all: basic-plus

$(EXECUTABLE): $(GO_FILES) parser.go
	go fmt && CGO_ENABLED=0 go build -ldflags=$(LDFLAGS) -o $(EXECUTABLE)

parser.go: parser.y
	goyacc -o parser.go parser.y

install: $(EXECUTABLE)
	sudo install -v $(EXECUTABLE) /usr/local/bin

clean:
	rm -f $(EXECUTABLE) parser.go y.output

git:
	rsync -a $(EXECUTABLE) $(DOCS) $(GO_FILES) Makefile \
		go.mod go.sum $(GITPATH)

lint:
	golangci-lint run
