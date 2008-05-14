testdir = MarkdownTest

Markdown : Markdown.hs Text/Pandoc/XML.hs Text/Parsers/Frisby.hs Text/Parsers/Frisby/Char.hs
	ghc -O2 -Wall --make Markdown.hs

test : Markdown
	${testdir}/MarkdownTest.pl --tidy --script=./Markdown --testdir=${testdir}/Tests

clean :
	-rm Markdown.o Markdown.hi Markdown
