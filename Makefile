.PHONY: test clean

test:
	antlr4 -o . -lib . -no-listener -no-visitor Perl.g4
	javac *.java
	grun Perl program -tree test.pl 

clean:
	rm *.class
	rm *.interp
	rm *.java
	rm *.tokens
