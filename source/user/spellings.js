/* This module exports various arrays of spelling-strings (operators, keywords, constants
and reserved words). */

export const operators = [
    ":",
    "=",
    "+",
    "-",
    "*",
    "/",
    "//",
    "%",
    "**",
    "<",
    ">",
    ".",
    "!",
    "?",
    "??",
    "==",
    "!=",
    "<=",
    ">=",
    "->",
    "=>",
    "&",
    "|",
    "||",
    "<<",
    ">>",
    ">>>",
    "...",
    "and",
    "as",
    "freeze",
    "frozen",
    "in",
    "is",
    "new",
    "not",
    "of",
    "on",
    "or",
    "pack",
    "packed",
    "seal",
    "sealed",
    "when",
    "+=",
    "-=",
    "*=",
    "/=",
    "//=",
    "%=",
    "**=",
    "&=",
    "|=",
    "||=",
    "<<=",
    ">>=",
    ">>>=",
];

export const constants = [ // TODO: add `root`
    "false",
    "Infinity",
    "NaN",
    "null",
    "super",
    "true",
    "void",
];

export const keywords = [
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "debug",
    "delete",
    "dev",
    "do",
    "else",
    "export",
    "for",
    "from",
    "function",
    "if",
    "import",
    "let",
    "pass",
    "private",
    "return",
    "static",
    "subclass",
    "throw",
    "unit",
    "var",
    "while",
    "yield",
];

// finish up by exporting an array containing all of the reserved words...

export const reserved = [
    "abstract",
    "all",
    "arguments",
    "at",
    "auto",
    "be",
    "by",
    "case",
    "catch",
    "co",
    "construct",
    "constructor",
    "contra",
    "default",
    "define",
    "emit",
    "enum",
    "exit",
    "external",
    "final",
    "finally",
    "find",
    "generator",
    "get",
    "global",
    "go",
    "ignore",
    "inherit",
    "input",
    "interface",
    "invoke",
    "it",
    "lambda",
    "listen",
    "local",
    "lower",
    "macro",
    "method",
    "module",
    "mono",
    "namespace",
    "no",
    "non",
    "omni",
    "output",
    "own",
    "post",
    "pre",
    "package",
    "preserve",
    "property",
    "protected",
    "prototype",
    "public",
    "query",
    "raise",
    "record",
    "returns",
    "require",
    "set",
    "short",
    "struct",
    "sym",
    "switch",
    "tele",
    "template",
    "this",
    "throw",
    "thus",
    "then",
    "to",
    "try",
    "tuple",
    "undefined",
    "uni",
    "use",
    "virtual",
    "wait",
    "with",
];
