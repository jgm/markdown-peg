#!/bin/sh 

testdir=MarkdownTest
${testdir}/MarkdownTest.pl --tidy --script=./Markdown --testdir=${testdir}/Tests
# ${testdir}/mdtest.php -n -d -t ${testdir}/Markdown.mdtest -s ./Markdown
# ${testdir}/mdtest.php -n -d -t ${testdir}/PHP\ Markdown.mdtest -s ./Markdown

