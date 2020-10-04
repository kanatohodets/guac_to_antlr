* NonSingleOrEscapedQuote (and friends) consistently do not match. Instead
  SubName or SubNameNonQLike. This means that string literals with spaces or
  other non-subname characters in them do not work. WTF. Specifying multiple
  options in the litString seems silly.

* Expressions always trigger SUPER deep parse trees. Wtf?

* VersionNumber is too greedy, covers number literals
