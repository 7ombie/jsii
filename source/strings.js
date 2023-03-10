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
export const binaries = "01";
export const decimals = binaries + "23456789";
export const hexadecimals = decimals + "ABCDEF";

export const symbolics = (
    dot + questionmark + bang + modulo + ampersand + asterisk + bar +
    equals + plus + minus + slash + backslash + lesser + greater + colon
);

export const lowers = "abcdefghijklmnopqrstuvwxyz";
export const uppers = lowers.toUpperCase();
export const alphas = uppers + lowers;
export const alphanumerics = alphas + decimals;
export const wordInitials = alphas + dollar + underscore;
export const wordCharacters = wordInitials + decimals;

// export a hash mapping numeric base names to strings containing the
// digits that are valid for that base...

export const digits = {
    binary: binaries,
    decimal: decimals,
    hexadecimal: hexadecimals
};

// export an array containing the spelling for each unique operator...

export const operators = [
    ":",
    "=",
    "+",
    "-",
    "*",
    "/",
    "//",
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
    "in",
    "is",
    "not",
    "of",
];

// export an array containing all of the constant words...

export const constants = [
    "null",
    "void",
    "true",
    "false",
    "Infinity",
    "NaN",
    "super",
    "this",
    "global",
    "random",
    "arguments",
];

// export an array containing all of the keywords...

export const keywords = [
    "async",
    "await",
    "break",
    "continue",
    "debug",
    "delete",
    "do",
    "else",
    "exit",
    "for",
    "from",
    "function",
    "generator",
    "if",
    "lambda",
    "let",
    "pass",
    "return",
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
    "access",
    "alias",
    "as",
    "at",
    "attribute",
    "assert",
    "augment",
    "auto",
    "await",
    "be",
    "borrow",
    "bubble",
    "by",
    "call",
    "case",
    "catch",
    "chain",
    "class",
    "co",
    "com",
    "constructor",
    "continue",
    "contra",
    "debug",
    "default",
    "define",
    "delete",
    "emit",
    "enum",
    "export",
    "external",
    "final",
    "finally",
    "find",
    "from",
    "get",
    "global",
    "go",
    "ignore",
    "implements",
    "import",
    "inherit",
    "input",
    "interface",
    "invoke",
    "it",
    "lend",
    "listen",
    "local",
    "lower",
    "macro",
    "message",
    "method",
    "module",
    "mono",
    "namespace",
    "new",
    "no",
    "non",
    "omni",
    "on",
    "once",
    "or",
    "output",
    "own",
    "package",
    "post",
    "pre",
    "preserve",
    "private",
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
    "since",
    "short",
    "static",
    "struct",
    "subclass",
    "submodule",
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
    "type",
    "undefined",
    "uni",
    "use",
    "version",
    "virtual",
    "with",
];
