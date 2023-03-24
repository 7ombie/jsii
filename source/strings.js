// start by exporting all of the non-alphanumeric singles...

export const [empty, space, newline, tab, carriage] = ["", " ", "\n", "\t", "\r"];
export const [bang, atsign, pound, dollar, modulo] = ["!", "@", "#", "$", "%"];
export const [caret, ampersand, asterisk, tilde] = ["^", "&", "*", "~"];
export const [accent, apostrophe, quote, equals] = ["`", "'", "\"", "="];
export const [lesser, greater, comma, dot] = ["<", ">", ",", "."];
export const [semicolon, colon, slash, backslash] = [";", ":", "/", "\\"];
export const [plus, minus, underscore, questionmark] = ["+", "-", "_", "?"];
export const [bar, openParen, closeParen] = ["|", "(", ")"];
export const [openBracket, closeBracket] = ["[", "]"];
export const [openBrace, closeBrace] = ["{", "}"];

// export the various character groups as strings...

export const whitespace = space + newline;
export const deadspace = semicolon + whitespace;
export const terminators = semicolon + newline;
export const delimiters = ",{[()]}";

export const bases = "xbXB";
export const binary = "01";
export const decimal = binary + "23456789";
export const hexadecimal = decimal + "ABCDEF";

export const symbolics = (
    dot + questionmark + bang + modulo + ampersand + asterisk + bar +
    equals + plus + minus + slash + backslash + lesser + greater + colon
);

export const lowers = "abcdefghijklmnopqrstuvwxyz";
export const uppers = lowers.toUpperCase();
export const alphas = uppers + lowers;
export const alphanumerics = alphas + decimal;
export const wordInitials = alphas + dollar + underscore;
export const wordCharacters = wordInitials + decimal;

// export an array containing the spelling for each unique operator...

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
    "dec",
    "freeze",
    "frozen",
    "in",
    "inc",
    "is",
    "new",
    "not",
    "of",
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

// export an array containing all of the constant words...

export const constants = [
    "all",
    "arguments",
    "default",
    "false",
    "global",
    "Infinity",
    "NaN",
    "null",
    "random",
    "super",
    "this",
    "true",
    "void",
];

// export an array containing all of the keywords...

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
    "exit",
    "export",
    "for",
    "from",
    "function",
    "generator",
    "if",
    "import",
    "lambda",
    "let",
    "local",
    "pass",
    "private",
    "return",
    "static",
    "subclass",
    "unless",
    "until",
    "var",
    "wait",
    "while",
    "yield",
];

// finish up by exporting an array containing all of the reserved words...

export const reserved = [
    "abstract",
    "at",
    "auto",
    "be",
    "by",
    "call",
    "case",
    "catch",
    "co",
    "construct",
    "constructor",
    "contra",
    "define",
    "emit",
    "enum",
    "external",
    "final",
    "finally",
    "find",
    "get",
    "go",
    "ignore",
    "inherit",
    "input",
    "interface",
    "invoke",
    "it",
    "listen",
    "lower",
    "macro",
    "method",
    "module",
    "mono",
    "namespace",
    "no",
    "non",
    "omni",
    "on",
    "output",
    "own",
    "package",
    "post",
    "pre",
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
    "with",
];
