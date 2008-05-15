testdir = MarkdownTest
GHCOPTS ?= -O2 -Wall

Markdown : Markdown.hs Text/Pandoc/XML.hs Text/Parsers/Frisby.hs Text/Parsers/Frisby/Char.hs
	ghc $(GHCOPTS) --make Markdown.hs

test : Markdown
	${testdir}/MarkdownTest.pl --tidy --script=./Markdown --testdir=${testdir}/Tests

clean :
	-rm Markdown.hi Markdown.o Markdown \
	  Text/Pandoc/XML/XML.hi Text/Pandoc/XML/XML.o \
	  Text/Parsers/Frisby.hi Text/Parsers/Frisby.o \
	  Text/Parsers/Frisby/Char.hi Text/Parsers/Frisby/Char.o
