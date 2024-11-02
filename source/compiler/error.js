export class LarkError extends SyntaxError {

    /* This concrete class is used for all Lark syntax errors (across all of the stages). */

    constructor(message, location) {

        /* Take a message string and a location `Number`, unpack the line and column numbers,
        and use them to create a complete error message, deleting the contents of the `stack`
        and customizing the error name.

        Note: Line and column numbers are stored as if the `Number` is an unsigned 32-bit
        integer, with the most significant three bytes storing the line number, and the
        least significant byte storing the column number (both internally zero-indexed).
        This just makes it easier to ignore the location data when debugging. */

        const line = Math.floor(location / 256) + 1;
        const column = location % 256 + 1;
        const locator = `[${line}:${column}]`;

        super();

        this.stack = [];
        this.name = "LarkError";
        this.message = `${message} ${locator}`;
    }
}
