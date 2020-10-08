* start porting the tests from Guacamole!

* Expressions always trigger SUPER deep parse trees. Wtf?

* `*` is confused between SigilGlob and OpMultiply

* VersionNumber is too greedy, covers number literals -> SOLVED with more
  application of alternatives. NumberDec is a valid VersionNumber, so put
  NumberDec first and allow VersionNumber as an alternative.

* NonSingleOrEscapedQuote (and friends) consistently do not match. Instead
  SubName or SubNameNonQLike. This means that string literals with spaces or
  other non-subname characters in them do not work. WTF. Specifying multiple
  options in the litString seems silly. -> SOLVED with modes!
