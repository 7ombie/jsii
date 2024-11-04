export const JS = {
    is:    (value, type) => `${type}?.ƥis?.(${value}) ?? ${value} instanceof ${type}`,
    equals:       (x, y) => `${x}.ƥequals(${y})`,
    entries:         (x) => `${x}.ƥentries()`,
    keys:         (name) => `${name}?.ƥkeys?.() ?? ƥkeys(${name} ?? [])`,
    values:       (name) => `${name}?.ƥvalues?.() ?? Object.values(${name} ?? [])`,
    invert: (expression) => `!(${expression})`,
    wrap:   (expression) => `(${expression})`,
}
