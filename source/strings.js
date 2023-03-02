// start by exporting all of the non-alphanumeric singles...

export const [empty, space, newline, tab, carriage] = ["", " ", "\n", "\t", "\r"];
export const [bang, atsign, pound, dollar, modulo] = ["!", "@", "#", "$", "%"];
export const [caret, ampersand, asterisk, tilde] = ["^", "&", "*", "~"];
export const [accent, apostrophe, quote, equals] = ["`", "'", "\"", "="];
export const [lesser, greater, comma, period] = ["<", ">", ",", "."];
export const [semicolon, colon, slash, backslash] = [";", ":", "/", "\\"];
export const [plus, minus, underscore, questionmark] = ["+", "-", "_", "?"];
export const [bar, openParen, closeParen] = ["|", "(", ")"];
export const [openBracket, closeBracket] = ["[", "]"];
export const [openBrace, closeBrace] = ["{", "}"];

// export the various character groups as strings...

export const whitespace = space + newline;
export const deadspace = semicolon + whitespace;
export const terminators = semicolon + newline;

export const bases = "xbXB";
export const binaries = "01";
export const decimals = binaries + "23456789";
export const hexadecimals = decimals + "ABCDEF";

export const dotOperators = period + bang + questionmark;

export const symbolics = (
	dotOperators + modulo + ampersand + asterisk + equals +
	plus + minus + slash + backslash + lesser + greater
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

// export a hash mapping delimiting chararacters to their type names, and
// an array of opening delimiter type names...

export const delimiters = {
    ",": "comma-delimiter",
	":": "colon-delimiter",
	"(": "open-paren-delimiter",
	"[": "open-bracket-delimiter",
	"{": "open-brace-delimiter",
	")": "close-paren-delimiter",
	"]": "close-bracket-delimiter",
	"}": "close-brace-delimiter",
}

export const openingDelimiterTypes = [
	"open-paren-delimiter",
	"open-bracket-delimiter",
	"open-brace-delimiter",
];

export const propertyTypes = [
    "variable-name",
    "constant-word",
    "operator-word",
    "key-word",
];

// export an array of primitive constants...

export const constants = [
	"false",
	"Infinity",
	"NaN",
	"null",
	"true",
	"void",
];

// export an array of valid keywords (including qualified keywords)...

export const keywords = [
	"async lambda",
	"async function",
	"async generator",
	"break",
	"continue",
    "debug",
    "do",
	"do async lambda",
	"do async function",
	"do async generator",
	"do lambda",
	"do function",
	"do generator",
	"else",
	"else if",
	"lambda",
	"for",
	"function",
	"generator",
	"if",
	"in",
	"let",
    "pass",
	"put",
	"return",
	"unless",
	"until",
	"var",
	"while",
	"yield",
	"yield from",
];

// export a hash mapping valid qualfiers (including qualified qualifiers)
// to an array containing every word they can qualify, and `true` if they
// can qualify literals...

export const qualifiers = {
	"u8": [true],
	"s8": [true],
	"c8": [true],
	"u16": [true],
	"s16": [true],
	"u32": [true],
	"s32": [true],
	"u64": [true],
	"s64": [true],
	"f32": [true],
	"f64": [true],
	"else": ["if"],
	"is": ["not"],
	"not": ["in"],
	"do": ["async", "lambda", "function", "generator"],
	"do async": ["lambda", "function", "generator"],
	"async": ["lambda", "function", "generator"],
	"yield": ["from"]
};

// export a struct containing two hashes, one mapping prefix operators to
// their masses (their operator precedence), and another doing the same
// for infix operators...

export const masses = {prefix: Object.create(null), infix: Object.create(null)};

function prefix(mass, ...operators) {

	// define one or more prefix operators with the given mass...

	for (const operator of operators) masses.prefix[operator] = mass;
};

function infix(mass, ...operators) {

	// define one or more infix operators with the given mass...

	for (const operator of operators) masses.infix[operator] = mass;
};

prefix(2, "->", "=>");
prefix(14, "+", "-", "not");

infix(2, "->", "=>");
infix(3, "??");
infix(8, "is", "is not", "in", "not in");
infix(9, "<", ">", ">=", "<=");
infix(11, "+", "-");
infix(12, "*", "/", "//");                                                    // TODO: restore "%"
infix(13, "**");
infix(17, ".", "?", "!");

// use the operators now defined inside `masses` to generate and export
// three new arrays, containing every prefix, infix and unique operator
// (as strings)...

const unique = operator => ! prefixOperators.includes(operator);

export const prefixOperators = Object.keys(masses.prefix);
export const infixOperators = Object.keys(masses.infix);
export const operators = [...prefixOperators, ...infixOperators.filter(unique)];

// finish up by exporting an array containing all of the reserved words...

export const reserves = [
	"abstract",
	"access",
	"actor",
	"alias",
	"anti",
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
	"classify",
	"co",
	"com",
	"con",
	"constant",
	"constructor",
	"continue",
	"contra",
	"cyber",
	"de",
	"debug",
	"default",
	"define",
	"delete",
	"deprecated",
	"dis",
	"emit",
	"en",
	"enum",
	"ex",
	"export",
	"exports",
	"extend",
	"extends",
	"extension",
	"external",
	"extra",
	"final",
	"finally",
	"find",
	"fires",
	"from",
	"get",
	"global",
	"go",
	"host",
	"hyper",
	"ignore",
	"implements",
	"imports",
	"implements",
	"import",
	"infinity",
	"inherit",
	"input",
	"interface",
	"inter",
	"invoke",
	"it",
	"lend",
	"listen",
	"local",
	"loop",
	"lower",
	"macro",
	"message",
	"method",
	"micro",
	"mis",
	"mixin",
	"module",
	"mono",
	"namespace",
	"new",
	"no",
	"non",
    "of",
	"omni",
	"on",
	"once",
	"or",
	"output",
	"over",
	"override",
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
	"so",
	"static",
	"struct",
	"submodule",
	"sub",
	"summary",
	"super",
	"sym",
	"switch",
	"tele",
	"template",
	"throw",
	"throws",
	"thus",
	"then",
	"to",
	"try",
	"tuple",
	"type",
	"un",
	"uni",
	"use",
	"version",
	"virtual",
	"with",
    "?!",                                                                                           // TODO: reserve operators
    "<->",
    "<=>",
];
