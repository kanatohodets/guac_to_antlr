.PHONY: test clean

build:
	antlr4 -o . -lib . -no-listener -no-visitor PerlParser.g4
	javac *.java

clean:
	rm *.class
	rm *.interp
	rm *.java
	rm *.tokens
