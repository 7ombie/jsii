// start by exporting all of the non-alphanumeric singles...

export const [empty, space, newline, tab, carriage] = ["", " ", "\n", "\t", "\r"];
export const [bang, atsign, pound, dollar, modulo] = ["!", "@", "#", "$", "%"];
export const [caret, ampersand, asterisk, tilde] = ["^", "&", "*", "~"];
export const [backtick, apostrophe, quote, equals] = ["`", "'", "\"", "="];
export const [lesser, greater, comma, dot] = ["<", ">", ",", "."];
export const [semicolon, colon, slash, backslash] = [";", ":", "/", "\\"];
export const [plus, minus, underscore, questionmark] = ["+", "-", "_", "?"];
export const [bar, openParen, closeParen] = ["|", "(", ")"];
export const [openBracket, closeBracket] = ["[", "]"];
export const [openBrace, closeBrace] = ["{", "}"];

// export the various character groups as strings...

export const whitespace = space + newline;
export const terminators = newline + comma;
export const delimiters = "{[()]}";

export const bases = "xbXB";
export const binary = "01";
export const decimal = binary + "23456789";
export const hexadecimal = decimal + "ABCDEF";

export const operationals =
    dot + questionmark + bang + modulo + ampersand +
    asterisk + bar + equals + plus + minus + slash +
    backslash + lesser + greater + colon + tilde
;

export const lowers = "abcdefghijklmnopqrstuvwxyz";
export const uppers = lowers.toUpperCase();
export const alphas = uppers + lowers;
export const alphanumerics = alphas + decimal;
export const wordInitials = alphas + dollar + underscore;
export const wordCharacters = wordInitials + decimal;
