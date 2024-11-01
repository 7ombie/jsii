export const JS = {
    is: (value, type) => `${type}?.ƥis?.(${value}) ?? ${value} instanceof ${type}`,
    keys: name => `${name}?.ƥkeys?.() ?? ƥkeys(${name} ?? [])`,
    values: name => `${name}?.ƥvalues?.() ?? Object.values(${name} ?? [])`,
    invert: expression => `!(${expression})`,
}
